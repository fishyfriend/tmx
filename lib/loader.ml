include Loader_intf

module Make () : S = struct
  module S = State.Make ()
  module SM = S.Monad

  module Class = Class.Make (S)
  module Customtype = Customtype.Make (S)
  module Enum = Enum.Make (S)
  module Layer = Layer.Make (S)
  module Map = Map.Make (S)
  module Object = Object.Make (S)
  module Property = Property.Make (S)
  module Template = Template.Make (S)
  module Tile = Tile.Make (S)
  module Tileset = Tileset.Make (S)

  open SM.Infix

  let with_file fname f =
    if Sys.file_exists fname then In_channel.with_open_text fname f
    else Util.file_not_found fname

  let rec sm_load_tileset_xml fname : _ SM.t =
    with_file fname @@ fun ic ->
    let ts = Conv_xml.(with_xml_from_channel ic tileset_of_xml) in
    SM.update (S.add_tileset_exn fname ts) >>= fun () ->
    SM.iter_list sm_load_for_property (Tileset0.properties ts) >>= fun () ->
    SM.iter_list sm_load_for_tile (Tileset0.tiles ts) >|= fun () -> ts

  and sm_load_template_xml fname : _ SM.t =
    with_file fname @@ fun ic ->
    let tem = Conv_xml.(with_xml_from_channel ic template_of_xml) in
    let ts = Option.map snd (Template0.tileset tem) in
    SM.update (S.add_template_exn fname tem) >>= fun () ->
    sm_load_for_object (Template0.object_ tem) >>= fun () ->
    SM.map_option sm_load_tileset_xml ts >|= fun _ts -> tem

  and sm_load_map_xml fname : _ SM.t =
    with_file fname @@ fun ic ->
    let m = Conv_xml.(with_xml_from_channel ic map_of_xml) in
    let tss = List.map snd (Map0.tilesets m) in
    SM.update (S.add_map_exn fname m) >>= fun () ->
    SM.iter_list sm_load_for_property (Map0.properties m) >>= fun () ->
    SM.iter_list (fun ts -> sm_load_tileset_xml ts >|= ignore) tss
    >>= fun () ->
    SM.iter_list sm_load_for_layer (Map0.layers m) >|= fun () -> m

  and sm_load_customtypes_json fname : _ SM.t =
    with_file fname @@ fun ic ->
    let cts = Conv_json.(with_json_from_channel ic customtypes_of_json) in
    SM.iter_list (fun ct -> SM.update (S.add_customtype_exn ct)) cts
    >>= fun () ->
    SM.iter_list sm_load_for_customtype cts >|= fun () -> cts

  and sm_load_file fname : _ SM.t =
    let data = with_file fname In_channel.input_all in
    SM.update (S.add_file_exn fname data) >|= fun () -> data

  and sm_load_for_property prop : _ SM.t =
    match Property.value prop with
    | `File fname -> sm_load_file fname >|= ignore
    | `Class props -> SM.iter_list sm_load_for_property props
    | _ -> SM.return ()

  and sm_load_for_object o : _ SM.t =
    SM.iter_list sm_load_for_property (Object0.properties o) >>= fun () ->
    SM.map_option sm_load_template_xml (Object0.template o) >|= ignore

  and sm_load_for_tile tile : _ SM.t =
    SM.iter_list sm_load_for_object (Tile0.objectgroup tile) >>= fun () ->
    SM.iter_list sm_load_for_property (Tile0.properties tile)

  and sm_load_for_layer l : _ SM.t =
    SM.iter_list sm_load_for_property (Layer0.properties l) >>= fun () ->
    SM.iter_list sm_load_for_object (Layer0.objects l)

  and sm_load_for_customtype ct : _ SM.t =
    match Customtype0.variant ct with
    | `Class c -> SM.iter_list sm_load_for_property (Class0.members c)
    | _ -> SM.return ()

  let load_tileset_xml_exn fname = S.run (sm_load_tileset_xml fname)
  let load_map_xml_exn fname = S.run (sm_load_map_xml fname)
  let load_template_xml_exn fname = S.run (sm_load_template_xml fname)
  let load_customtypes_json_exn fn = S.run (sm_load_customtypes_json fn)
  let load_file_exn fname = S.run (sm_load_file fname)
end

type t = (module S)

let make () = (module Make () : S)
