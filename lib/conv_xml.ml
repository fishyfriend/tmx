module X = Ezxmlm

open Core.Simple
open Util.Option.Infix

type xml = Xmlm.attribute list * X.nodes

let attrs xml = fst xml
let nodes xml = snd xml
let data xml = X.data_to_string (nodes xml)

let wrap name f x =
  try f x with
  | Error.Error (`Xml_parse (_, path, msg)) ->
      Util.Error.xml_parse (name :: path) msg
  | exn ->
      let msg = Printexc.to_string exn in
      Util.Error.xml_parse [name] ("parse error: " ^ msg)

let wrap_list f xs = List.mapi (fun i x -> wrap (string_of_int i) f x) xs

let with_xml_from_channel ic f =
  let xml =
    let attrs = [] in
    let _dtd, nodes = Ezxmlm.from_channel ic in
    (attrs, nodes) in
  wrap "{root}" f xml

let attr0 k xml = X.get_attr k (attrs xml)
let attr k xml f = wrap ("@" ^ k) (fun xml -> attr0 k xml |> f) xml
let attr' k xml = attr k xml Fun.id
let attr_opt k xml f =
  wrap ("@" ^ k) (fun xml -> Util.Option.protect (attr0 k) xml >|= f) xml
let attr_opt' k xml = attr_opt k xml Fun.id

let child0 k xml = X.member_with_attr k (nodes xml)
let child k xml f = wrap k (fun xml -> child0 k xml |> f) xml

(* let child' k xml = child k xml Fun.id *)
let child_opt k xml f =
  wrap k (fun xml -> Util.Option.protect (child0 k) xml >|= f) xml
let child_opt' k xml = child_opt k xml Fun.id

let children0 k xml = X.members_with_attr k (nodes xml)
let children k xml f = wrap k (fun xml -> wrap_list f (children0 k xml)) xml
let children' k xml = children k xml Fun.id

let bool_of_string01 s =
  match s with
  | "0" -> false
  | "1" -> true
  | _ -> Util.Error.xml_parse [] "0 or 1 expected"

let property_type_of_string s =
  match s with
  | "string" -> `String
  | "int" -> `Int
  | "float" -> `Float
  | "bool" -> `Bool
  | "color" -> `Color
  | "file" -> `File
  | "object" -> `Object
  | "class" -> `Class
  | _ -> Util.Error.xml_parse [] ("invalid type: " ^ s)

let rec property_of_xml xml =
  let name = attr' "name" xml in
  let propertytype = attr_opt' "propertytype" xml in
  let type_ = attr_opt "type" xml property_type_of_string |? `String in
  let value =
    let aux f = attr_opt "value" xml f in
    match type_ with
    | `String -> `String (aux Fun.id |? "")
    | `Int -> `Int (aux int_of_string |? 0)
    | `Float -> `Float (aux float_of_string |? 0.)
    | `Bool -> `Bool (aux bool_of_string |? false)
    | `Color -> `Color (aux Color.of_string |? Color.trans)
    | `File -> `File (aux Fun.id |? ".")
    | `Object -> `Object (aux int_of_string |? 0)
    | `Class -> `Class (child_opt "properties" xml properties_of_xml |? [])
  in
  Property.make ~name ?propertytype ~value ()

and properties_of_xml xml = children "property" xml property_of_xml

let halign_of_string s =
  match s with
  | "left" -> `Left
  | "center" -> `Center
  | "right" -> `Right
  | "justify" -> `Justify
  | s -> Util.Error.xml_parse [] ("invalid halign " ^ s)

let valign_of_string s =
  match s with
  | "top" -> `Top
  | "center" -> `Center
  | "bottom" -> `Bottom
  | s -> Util.Error.xml_parse [] ("invalid valign " ^ s)

let text_of_xml xml =
  let text = data xml in
  let fontfamily = attr_opt' "fontfamily" xml in
  let pixelsize = attr_opt "pixelsize" xml int_of_string in
  let wrap = attr_opt "wrap" xml bool_of_string01 in
  let color = attr_opt "color" xml Color.of_string in
  let bold = attr_opt "bold" xml bool_of_string01 in
  let italic = attr_opt "italic" xml bool_of_string01 in
  let underline = attr_opt "underline" xml bool_of_string01 in
  let strikeout = attr_opt "strikeout" xml bool_of_string01 in
  let kerning = attr_opt "kerning" xml bool_of_string01 in
  let halign = attr_opt "halign" xml halign_of_string in
  let valign = attr_opt "valign" xml valign_of_string in
  Object.Text.make ?fontfamily ?pixelsize ?wrap ?color ?bold ?italic ?underline
    ?strikeout ?kerning ?halign ?valign text

let polygon_of_xml xml =
  let point_of_string s = Scanf.sscanf s "%f,%f" @@ fun x y -> (x, y) in
  attr "points" xml @@ fun s ->
  String.split_on_char ' ' s |> wrap_list point_of_string

let shape_of_xml xml =
  let ellipse = child_opt' "ellipse" xml in
  let point = child_opt' "point" xml in
  let polygon = child_opt "polygon" xml polygon_of_xml in
  let polyline = child_opt "polyline" xml polygon_of_xml in
  let text = child_opt "text" xml text_of_xml in
  let gid = attr_opt "gid" xml @@ fun s -> Gid.of_int32 (Int32.of_string s) in
  match (ellipse, point, polygon, polyline, text, gid) with
  | None, None, None, None, None, None -> None
  | Some _, None, None, None, None, None -> Some `Ellipse
  | None, Some _, None, None, None, None -> Some `Point
  | None, None, Some points, None, None, None -> Some (`Polygon points)
  | None, None, None, Some points, None, None -> Some (`Polyline points)
  | None, None, None, None, Some text, None -> Some (`Text text)
  | None, None, None, None, None, Some gid -> Some (`Tile gid)
  | _ -> Util.Error.xml_parse [] "Ambiguous shape"

let object_of_xml xml =
  let id = attr_opt "id" xml int_of_string in
  let name = attr_opt' "name" xml in
  let class_ = attr_opt' "type" xml in
  let x = attr_opt "x" xml float_of_string in
  let y = attr_opt "y" xml float_of_string in
  let width = attr_opt "width" xml float_of_string in
  let height = attr_opt "height" xml float_of_string in
  let rotation = attr_opt "rotation" xml float_of_string in
  let visible = attr_opt "visible" xml bool_of_string01 in
  let template = attr_opt' "template" xml in
  let properties = child_opt "properties" xml properties_of_xml in
  let shape = shape_of_xml xml in
  Object.make ?id ?name ?class_ ?x ?y ?width ?height ?rotation ?visible
    ?template ?properties ?shape ()

let encoding_of_string s =
  match s with
  | "base64" -> `Base64
  | "csv" -> `Csv
  | _ -> Util.Error.xml_parse [] "invalid encoding"

let compression_of_string s =
  match s with
  | "gzip" -> `Gzip
  | "zlib" -> `Zlib
  | "zstd" -> `Zstd
  | _ -> Util.Error.xml_parse [] "invalid compression"

let data_of_xml0 ?encoding ?compression xml =
  match (encoding, compression) with
  | None, None ->
      let gids =
        children "tile" xml @@ fun xml' -> attr "gid" xml' Int32.of_string
      in
      Data.of_int32_list gids
  | _ -> data xml |> Data.of_string ?encoding ?compression

let data_of_xml xml =
  let encoding = attr_opt "encoding" xml encoding_of_string in
  let compression = attr_opt "compression" xml compression_of_string in
  data_of_xml0 ?encoding ?compression xml

let data_of_xml_chunked ~dims:(layer_w, layer_h) xml =
  let encoding = attr_opt "encoding" xml encoding_of_string in
  let compression = attr_opt "compression" xml compression_of_string in
  let chunk_pos xml =
    (attr "x" xml int_of_string, attr "y" xml int_of_string) in
  match child_opt' "chunk" xml with
  | None -> Data.create ?encoding ?compression 0
  | Some xml' ->
      let bytes = Bytes.create (layer_w * layer_h * 4) in
      let _ =
        let x_min, y_min =
          List.fold_left
            (fun (x0, y0) (x, y) -> (min x0 x, min y0 y))
            (chunk_pos xml')
            (children "chunk" xml chunk_pos) in
        let x_max, y_max = (x_min + layer_w - 1, y_min + layer_h - 1) in
        children "chunk" xml @@ fun xml'' ->
        let x, y = chunk_pos xml'' in
        let w = attr "width" xml'' int_of_string in
        let h = attr "height" xml'' int_of_string in
        let data = data_of_xml0 ?encoding ?compression xml'' in
        let src = Data.bytes data in
        let dst = bytes in
        let len = min w (x_max - x + 1) * 4 in
        for row = 0 to min (h - 1) (y_max - y) do
          let src_off = row * w * 4 in
          let dst_off = (x - x_min + ((y - y_min + row) * layer_w)) * 4 in
          Bytes.blit src src_off dst dst_off len
        done in
      Data.make ?encoding ?compression bytes

let image_format_of_string s =
  match String.lowercase_ascii s with
  | "bmp" -> `Bmp
  | "gif" -> `Gif
  | "jpg" -> `Jpg
  | "png" -> `Png
  | _ -> Util.Error.xml_parse [] "invalid format"

let image_of_xml xml =
  let trans = attr_opt "trans" xml Color.of_string in
  let width = attr_opt "width" xml int_of_string in
  let height = attr_opt "height" xml int_of_string in
  let source =
    match attr_opt' "source" xml with
    | Some source -> `File source
    | None ->
        let format = attr "format" xml image_format_of_string in
        let data = child "data" xml data_of_xml in
        `Embed (format, data) in
  Image.make ~source ?trans ?width ?height ()

let animation_of_xml xml =
  children "frame" xml @@ fun xml' ->
  let tileid = attr "tileid" xml' int_of_string in
  let duration = attr "duration" xml' int_of_string in
  Tile.Frame.make ~tileid ~duration

let tile_of_xml xml =
  let id = attr "id" xml int_of_string in
  let class_ = attr_opt' "type" xml in
  let x = attr_opt "x" xml int_of_string in
  let y = attr_opt "y" xml int_of_string in
  let width = attr_opt "width" xml int_of_string in
  let height = attr_opt "height" xml int_of_string in
  let properties = child_opt "properties" xml properties_of_xml in
  let image = child_opt "image" xml image_of_xml in
  let objectgroup =
    child_opt "objectgroup" xml @@ fun xml' ->
    children "object" xml' object_of_xml in
  let animation = child_opt "animation" xml animation_of_xml in
  Tile.make ~id ?class_ ?x ?y ?width ?height ?properties ?image ?objectgroup
    ?animation ()

let objectalignment_of_string s =
  match s with
  | "unspecified" -> `Unspecified
  | "topleft" -> `Topleft
  | "top" -> `Top
  | "topright" -> `Topright
  | "left" -> `Left
  | "center" -> `Center
  | "right" -> `Right
  | "bottomleft" -> `Bottomleft
  | "bottom" -> `Bottom
  | "bottomright" -> `Bottomright
  | _ -> Util.Error.xml_parse [] "invalid objectalignment"

let tilerendersize_of_string s =
  match s with
  | "tile" -> `Tile
  | "grid" -> `Grid
  | _ -> Util.Error.xml_parse [] "invalid tilerendersize"

let fillmode_of_string s =
  match s with
  | "stretch" -> `Stretch
  | "preserve-aspect-fit" -> `Preserve_aspect_fit
  | _ -> Util.Error.xml_parse [] "fillmode"

let tileoffset_of_xml xml =
  let x = attr_opt "x" xml int_of_string in
  let y = attr_opt "y" xml int_of_string in
  Tileset.Tileoffset.make ?x ?y ()

let grid_of_xml xml =
  match attr_opt' "orientation" xml with
  | Some "orthogonal" | None -> `Orthogonal
  | Some "isometric" ->
      let width = attr "width" xml int_of_string in
      let height = attr "height" xml int_of_string in
      `Isometric (width, height)
  | Some s -> Util.Error.invalid_arg "orientation" s

let single_of_xml xml =
  let tilecount = attr "tilecount" xml int_of_string in
  let tilewidth = attr "tilewidth" xml int_of_string in
  let tileheight = attr "tileheight" xml int_of_string in
  let spacing = attr_opt "spacing" xml int_of_string in
  let margin = attr_opt "margin" xml int_of_string in
  let image = child "image" xml image_of_xml in
  Tileset.Single.make ~tilecount ~tilewidth ~tileheight ?spacing ?margin image

let tileset_of_xml xml =
  let name = attr' "name" xml in
  let class_ = attr_opt' "class" xml in
  let columns = attr "columns" xml int_of_string in
  let objectalignment =
    attr_opt "objectalignment" xml objectalignment_of_string in
  let tilerendersize = attr_opt "tilerendersize" xml tilerendersize_of_string in
  let fillmode = attr_opt "fillmode" xml fillmode_of_string in
  let tileoffset = child_opt "tileoffset" xml tileoffset_of_xml in
  let grid = child_opt "grid" xml grid_of_xml in
  let properties = child_opt "properties" xml properties_of_xml in
  let tiles = children "tile" xml tile_of_xml in
  let variant =
    match child_opt' "image" xml with
    | Some _ -> `Single (single_of_xml xml)
    | None -> `Collection in
  Tileset.make ~name ?class_ ~columns ?objectalignment ?tilerendersize
    ?fillmode ?tileoffset ?grid ?properties ~variant tiles

let template_of_xml xml =
  let tileset =
    child_opt "tileset" xml @@ fun xml' ->
    let source = attr' "source" xml' in
    let firstgid = attr "firstgid" xml' int_of_string in
    (firstgid, source) in
  let object_ = child "object" xml object_of_xml in
  Template.make ?tileset object_

let tilelayer_of_xml xml =
  let width = attr "width" xml int_of_string in
  let height = attr "height" xml int_of_string in
  let data =
    let data =
      child_opt "data" xml @@ fun xml ->
      match children' "chunk" xml with
      | [] -> data_of_xml xml
      | _ -> data_of_xml_chunked ~dims:(width, height) xml in
    data |? Data.create (width * height * 4) in
  Tilelayer.make ~width ~height data

let draworder_of_string s =
  match s with
  | "index" -> `Index
  | "topdown" -> `Topdown
  | _ -> Util.Error.xml_parse [] s

let objectgroup_of_xml xml =
  let draworder = attr_opt "draworder" xml draworder_of_string in
  let objects = children "object" xml object_of_xml in
  Objectgroup.make ?draworder ~objects ()

let imagelayer_of_xml xml =
  let image = child_opt "image" xml image_of_xml in
  let repeatx = attr_opt "repeatx" xml bool_of_string01 in
  let repeaty = attr_opt "repeaty" xml bool_of_string01 in
  Imagelayer.make ?image ?repeatx ?repeaty ()

let rec layer_of_xml ~type_ xml =
  let id = attr_opt "id" xml int_of_string in
  let name = attr_opt' "name" xml in
  let class_ = attr_opt' "class" xml in
  let opacity = attr_opt "opacity" xml float_of_string in
  let visible = attr_opt "visible" xml bool_of_string01 in
  let tintcolor = attr_opt "tintcolor" xml Color.of_string in
  let offsetx = attr_opt "offsetx" xml float_of_string in
  let offsety = attr_opt "offsety" xml float_of_string in
  let parallaxx = attr_opt "parallaxx" xml float_of_string in
  let parallaxy = attr_opt "parallaxy" xml float_of_string in
  let properties = child_opt "properties" xml properties_of_xml in
  let variant =
    match type_ with
    | "layer" -> `Tilelayer (tilelayer_of_xml xml)
    | "objectgroup" -> `Objectgroup (objectgroup_of_xml xml)
    | "imagelayer" -> `Imagelayer (imagelayer_of_xml xml)
    | "group" -> `Group (layers_of_xml xml)
    | _ -> Util.Error.invalid_arg "type_" type_ in
  Layer.make ?id ?name ?class_ ?opacity ?visible ?tintcolor ?offsetx ?offsety
    ?parallaxx ?parallaxy ?properties ~variant ()

and layers_of_xml xml =
  List.concat_map
    (fun type_ -> children type_ xml @@ layer_of_xml ~type_)
    ["layer"; "objectgroup"; "imagelayer"; "group"]

let renderorder_of_string s =
  match s with
  | "right-down" -> `Right_down
  | "right-up" -> `Right_up
  | "left-down" -> `Left_down
  | "left-up" -> `Left_up
  | _ -> Util.Error.xml_parse [] "invalid renderorder"

let staggeraxis_of_string s =
  match s with
  | "x" -> `X
  | "y" -> `Y
  | _ -> Util.Error.xml_parse [] "invalid staggeraxis"

let staggerindex_of_string s =
  match s with
  | "even" -> `Even
  | "odd" -> `Odd
  | _ -> Util.Error.xml_parse [] "staggerindex"

let orientation_of_string s =
  match s with
  | "orthogonal" -> `Orthogonal
  | "isometric" -> `Isometric
  | "staggered" -> `Staggered
  | "hexagonal" -> `Hexagonal
  | _ -> Util.Error.xml_parse [] "invalid orientation"

let staggered_of_xml xml =
  let staggeraxis = attr "staggeraxis" xml staggeraxis_of_string in
  let staggerindex = attr "staggerindex" xml staggerindex_of_string in
  Map.Staggered.make ~staggeraxis ~staggerindex

let hexagonal_of_xml xml =
  let staggeraxis = attr "staggeraxis" xml staggeraxis_of_string in
  let staggerindex = attr "staggerindex" xml staggerindex_of_string in
  let hexsidelength = attr "hexsidelength" xml int_of_string in
  Map.Hexagonal.make ~hexsidelength ~staggeraxis ~staggerindex

let map_variant_of_xml xml =
  match attr "orientation" xml orientation_of_string with
  | `Orthogonal -> `Orthogonal
  | `Isometric -> `Isometric
  | `Staggered -> `Staggered (staggered_of_xml xml)
  | `Hexagonal -> `Hexagonal (hexagonal_of_xml xml)

let map_tileset_of_xml xml =
  let firstgid = attr "firstgid" xml int_of_string in
  match attr_opt' "source" xml with
  | Some source -> (firstgid, source)
  | None ->
      Util.Error.xml_parse []
        "Missing attribute \"source\" (embedded tilesets are not supported)"

let map_of_xml xml =
  let class_ = attr_opt' "class" xml in
  let renderorder = attr_opt "renderorder" xml renderorder_of_string in
  let compressionlevel = attr_opt "compressionlevel" xml int_of_string in
  let width = attr "width" xml int_of_string in
  let height = attr "height" xml int_of_string in
  let tilewidth = attr "tilewidth" xml int_of_string in
  let tileheight = attr "tileheight" xml int_of_string in
  let parallaxoriginx = attr_opt "parallaxoriginx" xml int_of_string in
  let parallaxoriginy = attr_opt "parallaxoriginy" xml int_of_string in
  let backgroundcolor = attr_opt "backgroundcolor" xml Color.of_string in
  let infinite = attr_opt "infinite" xml bool_of_string01 in
  let properties = child_opt "properties" xml properties_of_xml in
  let tilesets = children "tileset" xml map_tileset_of_xml in
  let layers = layers_of_xml xml in
  let geometry = map_variant_of_xml xml in
  Map.make ?class_ ?renderorder ?compressionlevel ~width ~height ~tilewidth
    ~tileheight ?parallaxoriginx ?parallaxoriginy ?backgroundcolor ?infinite
    ?properties ~tilesets ~layers ~geometry ()

let version_of_string s =
  try Scanf.sscanf s "%d.%d" @@ fun x y -> (x, y)
  with _ -> Util.Error.invalid_arg "version" s

let check_format_version xml =
  ignore
    ( attr_opt "version" xml @@ fun s ->
      let version = version_of_string s in
      if version < Util.min_format_version then
        let maj, min = version in
        Util.Error.invalid_arg "version" (Format.sprintf "%d.%d" maj min) )

let map_of_toplevel_xml xml =
  child "map" xml @@ fun xml' -> check_format_version xml' ; map_of_xml xml'

let tileset_of_toplevel_xml xml =
  child "tileset" xml @@ fun xml' ->
  check_format_version xml' ; tileset_of_xml xml'

let template_of_toplevel_xml xml =
  child "template" xml @@ fun xml' ->
  check_format_version xml' ; template_of_xml xml'
