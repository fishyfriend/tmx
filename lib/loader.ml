include Loader_intf

module Make () : S = struct
  module State = State.Make ()
  module Class = Class.Make (State)
  module Customtype = Customtype.Make (State)
  module Enum = Enum.Make (State)
  module Layer = Layer.Make (State)
  module Map = Map.Make (State)
  module Object = Object.Make (State)
  module Property = Property.Make (State)
  module Template = Template.Make (State)
  module Tile = Tile.Make (State)
  module Tileset = Tileset.Make (State)

  include State

  let with_file fname f =
    if Sys.file_exists fname then In_channel.with_open_text fname f
    else Util.file_not_found fname

  let load_tileset_xml_exn fname =
    with_file fname @@ fun ic ->
    let ts = Conv_xml.(with_xml_from_channel ic tileset_of_xml) in
    State.(update (add_tileset_exn fname ts)) ;
    ts

  let load_template_xml_exn fname =
    with_file fname @@ fun ic ->
    let te = Conv_xml.(with_xml_from_channel ic template_of_xml) in
    State.(update (add_template_exn fname te)) ;
    te

  let load_file_exn fname =
    let data = with_file fname In_channel.input_all in
    State.(update (add_file_exn fname data)) ;
    data

  let load_customtypes_json_exn fname =
    with_file fname @@ fun ic ->
    let cts = Conv_json.(with_json_from_channel ic customtypes_of_json) in
    List.iter (fun ct -> State.(update (add_customtype_exn ct))) cts ;
    cts

  let load_map_xml_exn fname =
    with_file fname @@ fun ic -> Conv_xml.(with_xml_from_channel ic map_of_xml)
end

type t = (module S)

let make () = (module Make () : S)
