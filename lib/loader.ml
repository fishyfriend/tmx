(* TODO: allow to fail gracefully when unable to autoload a resource file. make
   this behavior configurable. *)
(* TODO: the monadic code is gnarly, can it be de-monadized? *)

module C = Context
module S = State
module UE = Util.Error

module type S = Sigs.Loader

type t = (module S)

let make ~root : t =
  ( module struct
    let () =
      if Filename.is_relative root then UE.invalid_arg "absolute path" root

    let the_context = ref Context.default

    let run state =
      let result, context = State.run state !the_context in
      the_context := context ;
      result

    let tilesets () = C.tilesets !the_context
    let templates () = C.templates !the_context
    let files () = C.files !the_context
    let customtypes () = C.customtypes !the_context
    let maps () = C.maps !the_context

    module Getters = (val Context.make_getters the_context)

    include Getters
    include Core.Make (Getters)

    include struct
      open Util.Option.Infix
      let not_found n k () = UE.not_found n k
      let get_class_exn k ~useas = get_class k ~useas >|? not_found "class" k
      let get_file_exn k = get_file k >|? not_found "loaded file" k
      let get_map_exn k = get_map k >|? not_found "map" k
      let get_template_exn k = get_template k >|? not_found "template" k
      let get_tile_exn gid = get_tile gid >|? not_found "tile" (Gid.show gid)
      let get_tileset_exn k = get_tileset k >|? not_found "tileset" k
    end

    open S.Syntax

    let wrap_error fname f x =
      try f x
      with Error.Error (`Xml_parse (None, path, msg)) ->
        UE.xml_parse ~fname path msg

    let with_file fname f =
      if not (Filename.is_relative fname) then UE.invalid_arg "filename" fname ;
      let fname = Filename.concat root fname in
      if Sys.file_exists fname then
        In_channel.with_open_text fname (wrap_error fname f)
      else UE.file_not_found fname

    let s_with_cache f g =
      let* c = S.get () in
      match f c with Some x -> S.return x | None -> g ()

    let rec s_load_tileset_xml fname : _ S.t =
      s_with_cache (fun c -> Option.map snd (C.get_tileset fname c))
      @@ fun () ->
      with_file fname @@ fun ic ->
      let ts =
        Conv_xml.(with_xml_from_channel ic tileset_of_xml)
        |> Remappers.tileset_relocate ~from_dir:(Filename.dirname fname)
             ~to_dir:"" in
      let* () =
        S.update (C.add_tileset_exn fname ts) >>= fun () ->
        S.iter_list s_load_for_property (Tileset.properties ts) >>= fun () ->
        S.iter_list s_load_for_tile (Tileset.tiles ts) in
      S.return ts

    and s_load_template_xml fname : _ S.t =
      s_with_cache (C.get_template fname) @@ fun () ->
      with_file fname @@ fun ic ->
      let tem =
        Conv_xml.(with_xml_from_channel ic template_of_xml)
        |> Remappers.template_relocate ~from_dir:(Filename.dirname fname)
             ~to_dir:"" in
      let* () =
        S.iter_option
          (fun (_, ts) -> s_load_tileset_xml ts >|= ignore)
          (Template.tileset tem)
        >>= fun () -> s_load_for_object (Template.object_ tem) in
      let* tem =
        let+ c = S.get () in
        C.template_remap_gids c tem in
      let* () = S.update (C.add_template_exn fname tem) in
      S.return tem

    and s_load_map_xml fname : _ S.t =
      s_with_cache (C.get_map fname) @@ fun () ->
      with_file fname @@ fun ic ->
      let m =
        Conv_xml.(with_xml_from_channel ic map_of_xml)
        |> Remappers.map_relocate ~from_dir:(Filename.dirname fname) ~to_dir:""
      in
      let* () =
        S.iter_list s_load_for_property (Map.properties m) >>= fun () ->
        S.iter_list
          (fun (_, ts) -> s_load_tileset_xml ts >|= ignore)
          (Map.tilesets m)
        >>= fun () -> S.iter_list s_load_for_layer (Map.layers m) in
      let* m =
        let+ c = S.get () in
        C.map_remap_gids c m in
      let* () = S.update (C.add_map_exn fname m) in
      S.return m

    and s_load_customtypes_json fname : _ S.t =
      with_file fname @@ fun ic ->
      let cts =
        Conv_json.(with_json_from_channel ic customtypes_of_json)
        |> List.map
             (Remappers.customtype_relocate ~from_dir:(Filename.dirname fname)
                ~to_dir:"" ) in
      let* () =
        S.iter_list (fun ct -> S.update (C.add_customtype_exn ct)) cts
        >>= fun () -> S.iter_list s_load_for_customtype cts in
      S.return cts

    and s_load_file fname : _ S.t =
      s_with_cache (C.get_file fname) @@ fun () ->
      with_file fname @@ fun ic ->
      let data = In_channel.input_all ic in
      let+ () = S.update (C.add_file_exn fname data) in
      data

    and s_load_for_property prop : _ S.t =
      match Property.value prop with
      | `File fname -> s_load_file fname >|= ignore
      | `Class props -> S.iter_list s_load_for_property props
      | _ -> S.return ()

    and s_load_for_object o : _ S.t =
      S.iter_list s_load_for_property (Object.properties o) >>= fun () ->
      S.map_option s_load_template_xml (Object.template o) >|= ignore

    and s_load_for_tile tile : _ S.t =
      S.iter_list s_load_for_object (Tile.objectgroup tile) >>= fun () ->
      S.iter_list s_load_for_property (Tile.properties tile)

    and s_load_for_layer l : _ S.t =
      S.iter_list s_load_for_property (Layer.properties l) >>= fun () ->
      S.iter_list s_load_for_object (Layer.objects l)

    and s_load_for_customtype ct : _ S.t =
      match Customtype.variant ct with
      | `Class c -> S.iter_list s_load_for_property (Class.members c)
      | _ -> S.return ()

    let load_tileset_xml_exn fname = run (s_load_tileset_xml fname)
    let load_map_xml_exn fname = run (s_load_map_xml fname)
    let load_template_xml_exn fname = run (s_load_template_xml fname)
    let load_customtypes_json_exn fn = run (s_load_customtypes_json fn)
    let load_file_exn fname = run (s_load_file fname)

    let load_tileset_xml fname = UE.protect load_tileset_xml_exn fname
    let load_map_xml fname = UE.protect load_map_xml_exn fname
    let load_template_xml fname = UE.protect load_template_xml_exn fname
    let load_customtypes_json fname =
      UE.protect load_customtypes_json_exn fname
    let load_file fname = UE.protect load_file_exn fname

    let unload_tileset k = run (S.update (C.remove_tileset k))
    let unload_template k = run (S.update (C.remove_template k))
    let unload_customtypes k = run (S.update (C.remove_customtypes k))
    let unload_class k ~useas = run (S.update (C.remove_class k ~useas))
    let unload_file k = run (S.update (C.remove_file k))
    let unload_map k = run (S.update (C.remove_map k))
  end )
