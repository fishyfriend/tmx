module C = Context
module S = State

module Make () = struct
  open S.Infix

  include Nonbasic.Make ()

  let with_file fname f =
    if Sys.file_exists fname then In_channel.with_open_text fname f
    else Util.file_not_found fname

  let rec s_load_tileset_xml fname : _ S.t =
    with_file fname @@ fun ic ->
    let ts = Conv_xml.(with_xml_from_channel ic tileset_of_xml) in
    S.update (C.add_tileset_exn fname ts) >>= fun () ->
    S.iter_list s_load_for_property (Tileset.properties ts) >>= fun () ->
    S.iter_list s_load_for_tile (Tileset.tiles ts) >|= fun () -> ts

  and s_load_template_xml fname : _ S.t =
    with_file fname @@ fun ic ->
    let tem = Conv_xml.(with_xml_from_channel ic template_of_xml) in
    let ts = Option.map snd (Template.tileset tem) in
    S.update (C.add_template_exn fname tem) >>= fun () ->
    s_load_for_object (Template.object_ tem) >>= fun () ->
    S.map_option s_load_tileset_xml ts >|= fun _ts -> tem

  and s_load_map_xml fname : _ S.t =
    with_file fname @@ fun ic ->
    let m = Conv_xml.(with_xml_from_channel ic map_of_xml) in
    let tss = List.map snd (Map.tilesets m) in
    S.update (C.add_map_exn fname m) >>= fun () ->
    S.iter_list s_load_for_property (Map.properties m) >>= fun () ->
    S.iter_list (fun ts -> s_load_tileset_xml ts >|= ignore) tss >>= fun () ->
    S.iter_list s_load_for_layer (Map.layers m) >|= fun () -> m

  and s_load_customtypes_json fname : _ S.t =
    with_file fname @@ fun ic ->
    let cts = Conv_json.(with_json_from_channel ic customtypes_of_json) in
    S.iter_list (fun ct -> S.update (C.add_customtype_exn ct)) cts
    >>= fun () ->
    S.iter_list s_load_for_customtype cts >|= fun () -> cts

  and s_load_file fname : _ S.t =
    let data = with_file fname In_channel.input_all in
    S.update (C.add_file_exn fname data) >|= fun () -> data

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

  let load_tileset_xml fname = Util.protect_result load_tileset_xml_exn fname
  let load_map_xml fname = Util.protect_result load_map_xml_exn fname
  let load_template_xml fname = Util.protect_result load_template_xml_exn fname
  let load_customtypes_json fname =
    Util.protect_result load_customtypes_json_exn fname
  let load_file fname = Util.protect_result load_file_exn fname

  let unload_tileset k = run_context (S.update (C.remove_tileset k))
  let unload_template k = run_context (S.update (C.remove_template k))
  let unload_customtypes k = run_context (S.update (C.remove_customtypes k))
  let unload_class k ~useas = run_context (S.update (C.remove_class k ~useas))
  let unload_file k = run_context (S.update (C.remove_file k))
  let unload_map k = run_context (S.update (C.remove_map k))

  let get_tileset k = read_context (C.get_tileset k) |> Option.map snd
  let get_template k = read_context (C.get_template k)
  let get_customtypes k = read_context (C.get_customtypes k)
  let get_class k ~useas = read_context (C.get_class k ~useas)
  let get_file k = read_context (C.get_file k)
  let get_map k = read_context (C.get_map k)
end
