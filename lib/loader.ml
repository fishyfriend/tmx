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

  let rec sm_load_tileset_xml fname : Tileset.t SM.t =
    with_file fname @@ fun ic ->
    let ts = Conv_xml.(with_xml_from_channel ic tileset_of_xml) in
    SM.update (S.add_tileset_exn fname ts) >>= fun () ->
    SM.iter_list sm_load_for_property (Tileset0.properties ts) >>= fun () ->
    SM.iter_list sm_load_for_tile (Tileset0.tiles ts) >|= fun () -> ts

  and sm_load_template_xml fname : Template.t SM.t =
    with_file fname @@ fun ic ->
    let tem = Conv_xml.(with_xml_from_channel ic template_of_xml) in
    let ts = Option.map snd (Template0.tileset tem) in
    SM.update (S.add_template_exn fname tem) >>= fun () ->
    sm_load_for_object (Template0.object_ tem) >>= fun () ->
    SM.map_option sm_load_tileset_xml ts >|= fun _ts -> tem

  and sm_load_map_xml fname : Map.t SM.t =
    with_file fname @@ fun ic ->
    let m = Conv_xml.(with_xml_from_channel ic map_of_xml) in
    let tss = List.map snd (Map0.tilesets m) in
    SM.update (S.add_map_exn fname m) >>= fun () ->
    SM.iter_list sm_load_for_property (Map0.properties m) >>= fun () ->
    SM.iter_list (fun ts -> sm_load_tileset_xml ts >|= ignore) tss
    >>= fun () ->
    SM.iter_list sm_load_for_layer (Map0.layers m) >|= fun () -> m

  and sm_load_customtypes_json fname : Customtype.t list SM.t =
    with_file fname @@ fun ic ->
    let cts = Conv_json.(with_json_from_channel ic customtypes_of_json) in
    SM.iter_list (fun ct -> SM.update (S.add_customtype_exn ct)) cts
    >>= fun () ->
    SM.iter_list sm_load_for_customtype cts >|= fun () -> cts

  and sm_load_file fname : string SM.t =
    let data = with_file fname In_channel.input_all in
    SM.update (S.add_file_exn fname data) >|= fun () -> data

  and sm_load_for_property prop : unit SM.t =
    match Property.value prop with
    | `File fname -> sm_load_file fname >|= ignore
    | `Class props -> SM.iter_list sm_load_for_property props
    | _ -> SM.return ()

  and sm_load_for_object o : unit SM.t =
    SM.iter_list sm_load_for_property (Object0.properties o) >>= fun () ->
    SM.map_option sm_load_template_xml (Object0.template o) >|= ignore

  and sm_load_for_tile tile : unit SM.t =
    SM.iter_list sm_load_for_object (Tile0.objectgroup tile) >>= fun () ->
    SM.iter_list sm_load_for_property (Tile0.properties tile)

  and sm_load_for_layer l : unit SM.t =
    SM.iter_list sm_load_for_property (Layer0.properties l) >>= fun () ->
    SM.iter_list sm_load_for_object (Layer0.objects l)

  and sm_load_for_customtype ct : unit SM.t =
    match Customtype0.variant ct with
    | `Class c -> SM.iter_list sm_load_for_property (Class0.members c)
    | _ -> SM.return ()

  let load_tileset_xml_exn fname = S.run (sm_load_tileset_xml fname)
  let load_map_xml_exn fname = S.run (sm_load_map_xml fname)
  let load_template_xml_exn fname = S.run (sm_load_template_xml fname)
  let load_customtypes_json_exn fname = S.run (sm_load_customtypes_json fname)
  let load_file_exn fname = S.run (sm_load_file fname)

  let load_tileset_xml fname = Util.protect_result load_tileset_xml_exn fname
  let load_map_xml fname = Util.protect_result load_map_xml_exn fname
  let load_template_xml fname = Util.protect_result load_template_xml_exn fname
  let load_customtypes_json fname =
    Util.protect_result load_customtypes_json_exn fname
  let load_file fname = Util.protect_result load_file_exn fname

  let unload_tileset k = S.run (SM.update (S.remove_tileset k))
  let unload_template k = S.run (SM.update (S.remove_template k))
  let unload_customtypes k = S.run (SM.update (S.remove_customtypes k))
  let unload_class k ~useas = S.run (SM.update (S.remove_class k ~useas))
  let unload_file k = S.run (SM.update (S.remove_file k))
  let unload_map k = S.run (SM.update (S.remove_map k))

  let get_tileset k = S.read (S.get_tileset k) |> Option.map snd
  let get_template k = S.read (S.get_template k)
  let get_customtypes k = S.read (S.get_customtypes k)
  let get_class k ~useas = S.read (S.get_class k ~useas)
  let get_file k = S.read (S.get_file k)
  let get_map k = S.read (S.get_map k)
end

type t = (module S)

let make () = (module Make () : S)
