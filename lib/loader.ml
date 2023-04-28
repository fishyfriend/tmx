module C = Context
module UE = Util.Error

open Util.Option.Infix

include Loader_intf

type t = (module S)

let make ~root : t =
  if Filename.is_relative root then UE.invalid_arg "root" root ;
  ( module struct
    let context = ref C.default

    let protect f x =
      let old_context = !context in
      try f x
      with exn ->
        context := old_context ;
        raise exn

    module Getters : Core.Getters = struct
      let get_tileset k = C.get_tileset k !context |> Option.map snd
      let get_template k = C.get_template k !context
      let get_customtypes k = C.get_customtypes k !context
      let get_file k = C.get_file k !context
      let get_map k = C.get_map k !context
      let get_tile gid = C.get_tile gid !context
    end

    module Aux = Core.Aux

    include Core.Make (Getters)

    let tilesets () = List.map (fun (_, k, v) -> (k, v)) (C.tilesets !context)

    let templates () = C.templates !context
    let files () = C.files !context
    let customtypes () = C.customtypes !context
    let maps () = C.maps !context

    let not_found n k () = UE.not_found n k
    let get_class_exn k ~useas = get_class k ~useas >|? not_found "class" k
    let get_file_exn k = get_file k >|? not_found "loaded file" k
    let get_map_exn k = get_map k >|? not_found "map" k
    let get_template_exn k = get_template k >|? not_found "template" k
    let get_tile_exn gid = get_tile gid >|? not_found "tile" (Gid.show gid)
    let get_tileset_exn k = get_tileset k >|? not_found "tileset" k

    let rebase_gid ~from_firstgid ~to_firstgid gid =
      let flags = Gid.flags gid in
      let id = Gid.id gid - from_firstgid + to_firstgid in
      Gid.make ~flags id

    let remap_gid_to_context ~from_alist gid =
      let id = Gid.id gid in
      if id = 0 then gid
      else
        match List.find_opt (fun (fstgid, _) -> fstgid <= id) from_alist with
        | None -> Util.Error.not_found "gid" (Gid.show gid)
        | Some (from_firstgid, ts) ->
          ( match C.get_tileset ts !context with
          | None -> Util.Error.not_found "tileset" ts
          | Some (to_firstgid, _) -> rebase_gid ~from_firstgid ~to_firstgid gid
          )

    let map_remap_gids m =
      let from_alist = Map.tilesets m in
      Aux.map_map_gids (remap_gid_to_context ~from_alist) m

    let template_remap_gids tem =
      let from_alist = Option.to_list (Template.tileset tem) in
      Aux.template_map_gids (remap_gid_to_context ~from_alist) tem

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

    let with_cache f g = match f !context with Some x -> x | None -> g ()

    let rec load_tileset_xml fname =
      with_cache (fun c -> Option.map snd (C.get_tileset fname c)) @@ fun () ->
      with_file fname @@ fun ic ->
      let ts =
        Conv_xml.(with_xml_from_channel ic tileset_of_xml)
        |> Aux.reloc_tileset ~from_dir:(Filename.dirname fname) ~to_dir:""
      in
      List.iter load_for_property (Tileset.properties ts) ;
      List.iter load_for_tile (Tileset.tiles ts) ;
      context := C.add_tileset_exn fname ts !context ;
      ts

    and load_template_xml fname =
      with_cache (C.get_template fname) @@ fun () ->
      with_file fname @@ fun ic ->
      let tem =
        Conv_xml.(with_xml_from_channel ic template_of_xml)
        |> Aux.reloc_template ~from_dir:(Filename.dirname fname) ~to_dir:""
      in
      ignore (Template.tileset tem >|= fun (_, ts) -> load_tileset_xml ts) ;
      load_for_object (Template.object_ tem) ;
      let tem = template_remap_gids tem in
      context := C.add_template_exn fname tem !context ;
      tem

    and load_map_xml fname =
      with_cache (C.get_map fname) @@ fun () ->
      with_file fname @@ fun ic ->
      let m =
        Conv_xml.(with_xml_from_channel ic map_of_xml)
        |> Aux.reloc_map ~from_dir:(Filename.dirname fname) ~to_dir:"" in
      List.iter (fun (_, ts) -> ignore (load_tileset_xml ts)) (Map.tilesets m) ;
      List.iter load_for_property (Map.properties m) ;
      List.iter load_for_layer (Map.layers m) ;
      let m = map_remap_gids m in
      context := C.add_map_exn fname m !context ;
      m

    and load_customtypes_json fname =
      with_file fname @@ fun ic ->
      let cts =
        Conv_json.(with_json_from_channel ic customtypes_of_json)
        |> List.map
             (Aux.reloc_customtype ~from_dir:(Filename.dirname fname)
                ~to_dir:"" ) in
      List.iter load_for_customtype cts ;
      List.iter (fun ct -> context := C.add_customtype_exn ct !context) cts ;
      cts

    and load_file fname =
      with_cache (C.get_file fname) @@ fun () ->
      with_file fname @@ fun ic ->
      let data = In_channel.input_all ic in
      context := C.add_file_exn fname data !context ;
      data

    and load_for_property prop =
      match Property.value prop with
      | `File fname -> ignore (load_file fname)
      | `Class props -> List.iter load_for_property props
      | _ -> ()

    and load_for_object o =
      ignore (Object.template o >|= load_template_xml) ;
      List.iter load_for_property (Object.properties o)

    and load_for_tile tile =
      List.iter load_for_object (Tile.objectgroup tile) ;
      List.iter load_for_property (Tile.properties tile)

    and load_for_layer l =
      List.iter load_for_property (Layer.properties l) ;
      List.iter load_for_object (Layer.objects l)

    and load_for_customtype ct =
      match Customtype.variant ct with
      | `Class c -> List.iter load_for_property (Class.members c)
      | _ -> ()

    let load_tileset_xml_exn fname = protect load_tileset_xml fname
    let load_map_xml_exn fname = protect load_map_xml fname
    let load_template_xml_exn fname = protect load_template_xml fname
    let load_customtypes_json_exn fname = protect load_customtypes_json fname
    let load_file_exn fname = protect load_file fname

    let load_tileset_xml fname = UE.protect load_tileset_xml_exn fname
    let load_map_xml fname = UE.protect load_map_xml_exn fname
    let load_template_xml fname = UE.protect load_template_xml_exn fname
    let load_customtypes_json fn = UE.protect load_customtypes_json_exn fn
    let load_file fname = UE.protect load_file_exn fname

    let unload_tileset k = context := C.remove_tileset k !context
    let unload_template k = context := C.remove_template k !context
    let unload_customtypes k = context := C.remove_customtypes k !context
    let unload_class k ~useas = context := C.remove_class k ~useas !context
    let unload_file k = context := C.remove_file k !context
    let unload_map k = context := C.remove_map k !context
  end )
