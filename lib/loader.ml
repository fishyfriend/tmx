(* TODO: allow to fail gracefully when unable to autoload a resource file. make
   this behavior configurable. *)
(* TODO: tests for file path handling *)

module type S = Sigs.Loader

type t = (module S)

let make ~root : t =
  ( module struct
    module C = Context
    module S = State
    module UE = Util.Error

    open S.Syntax

    include Nonbasic.Make ()

    let wrap_error fname f x =
      try f x
      with Error.Error (`Xml_parse (None, path, msg)) ->
        UE.xml_parse ~fname path msg

    let s_with_cwd dir s : _ S.t =
      let* c0 = S.get () in
      let dir0 = C.cwd c0 in
      let c = C.set_cwd dir c0 in
      let x, c' = S.run s c in
      let c'' = C.set_cwd dir0 c' in
      let* () = S.set c'' in
      S.return x

    let s_with_file fname (f : _ -> _ S.t) : _ S.t =
      let* c = S.get () in
      let fname_rel =
        if Filename.is_relative fname then Filename.concat (C.cwd c) fname
        else UE.invalid_arg "filename" fname in
      let fname_abs = Filename.concat root fname_rel in
      if Sys.file_exists fname_abs then
        In_channel.with_open_text fname_abs @@ fun ic ->
        s_with_cwd (Filename.dirname fname_rel) (wrap_error fname_abs f ic)
      else UE.file_not_found fname_abs

    let s_relocate (type a) (module T : Sigs.RelocT with type t = a) (t : a) :
        _ S.t =
      let+ c = S.get () in
      T.relocate t (C.cwd c)

    let rec s_load_tileset_xml fname : _ S.t =
      s_with_file fname @@ fun ic ->
      let* ts =
        Conv_xml.(with_xml_from_channel ic tileset_of_xml)
        |> s_relocate (module Tileset) in
      let* () =
        S.update (C.add_tileset_exn fname ts) >>= fun () ->
        S.iter_list s_load_for_property (Tileset.properties ts) >>= fun () ->
        S.iter_list s_load_for_tile (Tileset.tiles ts) in
      S.return ts

    and s_load_template_xml fname : _ S.t =
      s_with_file fname @@ fun ic ->
      let* tem =
        Conv_xml.(with_xml_from_channel ic template_of_xml)
        |> s_relocate (module Template) in
      let* () =
        S.update (C.add_template_exn fname tem) >>= fun () ->
        S.iter_option
          (fun (_, ts) -> s_load_tileset_xml ts >|= ignore)
          (Template.tileset tem)
        >>= fun () -> s_load_for_object (Template.object_ tem) in
      S.return tem

    and s_load_map_xml fname : _ S.t =
      s_with_file fname @@ fun ic ->
      let* m =
        Conv_xml.(with_xml_from_channel ic map_of_xml)
        |> s_relocate (module Map) in
      let* () =
        S.update (C.add_map_exn fname m) >>= fun () ->
        S.iter_list s_load_for_property (Map.properties m) >>= fun () ->
        S.iter_list
          (fun (_, ts) -> s_load_tileset_xml ts >|= ignore)
          (Map.tilesets m)
        >>= fun () -> S.iter_list s_load_for_layer (Map.layers m) in
      S.return m

    and s_load_customtypes_json fname : _ S.t =
      s_with_file fname @@ fun ic ->
      let* cts =
        Conv_json.(with_json_from_channel ic customtypes_of_json)
        |> S.map_list (s_relocate (module Customtype)) in
      let* () =
        S.iter_list (fun ct -> S.update (C.add_customtype_exn ct)) cts
        >>= fun () -> S.iter_list s_load_for_customtype cts in
      S.return cts

    and s_load_file fname : _ S.t =
      s_with_file fname @@ fun ic ->
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

    let load_tileset_xml_exn fname = run_context (s_load_tileset_xml fname)
    let load_map_xml_exn fname = run_context (s_load_map_xml fname)
    let load_template_xml_exn fname = run_context (s_load_template_xml fname)
    let load_customtypes_json_exn fn = run_context (s_load_customtypes_json fn)
    let load_file_exn fname = run_context (s_load_file fname)

    let load_tileset_xml fname = UE.protect load_tileset_xml_exn fname
    let load_map_xml fname = UE.protect load_map_xml_exn fname
    let load_template_xml fname = UE.protect load_template_xml_exn fname
    let load_customtypes_json fname =
      UE.protect load_customtypes_json_exn fname
    let load_file fname = UE.protect load_file_exn fname

    let unload_tileset k = run_context (S.update (C.remove_tileset k))
    let unload_template k = run_context (S.update (C.remove_template k))
    let unload_customtypes k = run_context (S.update (C.remove_customtypes k))
    let unload_class k ~useas =
      run_context (S.update (C.remove_class k ~useas))
    let unload_file k = run_context (S.update (C.remove_file k))
    let unload_map k = run_context (S.update (C.remove_map k))

    let get_tileset k = read_context (C.get_tileset k) |> Option.map snd
    let get_template k = read_context (C.get_template k)
    let get_customtypes k = read_context (C.get_customtypes k)
    let get_class k ~useas = read_context (C.get_class k ~useas)
    let get_file k = read_context (C.get_file k)
    let get_map k = read_context (C.get_map k)
  end )
