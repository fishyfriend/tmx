open Util.Option_infix

module X = struct
  include Ezxmlm

  type t = Xmlm.attribute list * nodes

  let get_attr_opt k attrs =
    try Some (get_attr k attrs) with Not_found -> None

  let member_opt k nodes =
    try Some (member k nodes) with Tag_not_found _ -> None

  let member_with_attr_opt k nodes =
    try Some (member_with_attr k nodes) with Tag_not_found _ -> None
end

let bool_of_string01 s =
  match s with
  | "0" -> false
  | "1" -> true
  | _ -> Util.invalid_arg "string01" s

let rec property_of_xml (attrs, nodes) =
  let name = X.get_attr "name" attrs in
  let propertytype = X.get_attr_opt "propertytype" attrs in
  let type_ = X.get_attr_opt "value" attrs |? "string" in
  let value =
    let s = X.get_attr_opt "value" attrs in
    match type_ with
    | "string" -> `String (s |? "")
    | "int" -> `Int (s >|= int_of_string |? 0)
    | "float" -> `Float (s >|= float_of_string |? 0.)
    | "bool" -> `Bool (s >|= bool_of_string |? false)
    | "color" -> `Color (s >|= Color.of_string |? Color.trans)
    | "file" -> `File (s |? ".")
    | "object" -> `Object (s >|= int_of_string |? 0)
    | "class" -> `Class (get_properties nodes)
    | _ -> Util.invalid_arg "type" type_ in
  Property0.make ~name ?propertytype ~value ()

and get_properties nodes =
  X.member_opt "properties" nodes
  >|= X.members_with_attr "property"
  >|= List.map property_of_xml |? []

let text_of_xml (attrs, nodes) =
  let text = X.data_to_string nodes in
  let fontfamily = X.get_attr_opt "fontfamily" attrs in
  let pixelsize = X.get_attr_opt "pixelsize" attrs >|= int_of_string in
  let wrap = X.get_attr_opt "wrap" attrs >|= bool_of_string01 in
  let color = X.get_attr_opt "color" attrs >|= Color.of_string in
  let bold = X.get_attr_opt "bold" attrs >|= bool_of_string01 in
  let italic = X.get_attr_opt "italic" attrs >|= bool_of_string01 in
  let underline = X.get_attr_opt "underline" attrs >|= bool_of_string01 in
  let strikeout = X.get_attr_opt "strikeout" attrs >|= bool_of_string01 in
  let kerning = X.get_attr_opt "kerning" attrs >|= bool_of_string01 in
  let halign =
    X.get_attr_opt "halign" attrs >|= function
    | "left" -> `Left
    | "center" -> `Center
    | "right" -> `Right
    | "justify" -> `Justify
    | s -> Util.invalid_arg "halign" s in
  let valign =
    X.get_attr_opt "valign" attrs >|= function
    | "top" -> `Top
    | "center" -> `Center
    | "bottom" -> `Bottom
    | s -> Util.invalid_arg "valign" s in
  Object0.Text.make ?fontfamily ?pixelsize ?wrap ?color ?bold ?italic
    ?underline ?strikeout ?kerning ?halign ?valign text

let object_of_xml ((attrs, nodes) as xml) =
  let id = X.get_attr_opt "id" attrs >|= int_of_string in
  let name = X.get_attr_opt "name" attrs in
  let class_ = X.get_attr_opt "class" attrs in
  let x = X.get_attr_opt "x" attrs >|= float_of_string in
  let y = X.get_attr_opt "y" attrs >|= float_of_string in
  let width = X.get_attr_opt "width" attrs >|= float_of_string in
  let height = X.get_attr_opt "height" attrs >|= float_of_string in
  let rotation = X.get_attr_opt "rotation" attrs >|= float_of_string in
  let visible = X.get_attr_opt "visible" attrs >|= bool_of_string01 in
  let properties =
    X.member_opt "properties" nodes
    >|= X.members_with_attr "property"
    >|= List.map property_of_xml in
  let shape =
    let get_points attrs =
      let point_of_string s = Scanf.sscanf s "%f,%f" @@ fun x y -> (x, y) in
      X.get_attr "points" attrs |> String.split_on_char ' '
      |> List.map point_of_string in
    let ellipse = X.member_with_attr_opt "ellipse" nodes in
    let point = X.member_with_attr_opt "point" nodes in
    let polygon = X.member_with_attr_opt "polygon" nodes in
    let polyline = X.member_with_attr_opt "polyline" nodes in
    let text = X.member_with_attr_opt "text" nodes in
    let gid = X.get_attr_opt "gid" attrs >|= Int32.of_string >|= Gid.of_int32 in
    match (ellipse, point, polygon, polyline, text, gid) with
    | None, None, None, None, None, None -> None
    | Some _, None, None, None, None, None -> Some `Ellipse
    | None, Some _, None, None, None, None -> Some `Point
    | None, None, Some (attrs, _), None, None, None ->
        Some (`Polygon (get_points attrs))
    | None, None, None, Some (attrs, _), None, None ->
        Some (`Polyline (get_points attrs))
    | None, None, None, None, Some xml, None -> Some (`Text (text_of_xml xml))
    | None, None, None, None, None, Some gid -> Some (`Tile gid)
    | _ -> Util.xml_parse xml "Ambiguous shape" in
  Object0.make ?id ?name ?class_ ?x ?y ?width ?height ?rotation ?visible
    ?properties ?shape ()

let image_of_xml (attrs, nodes) =
  let trans = X.get_attr_opt "trans" attrs >|= Color.of_string in
  let width = X.get_attr_opt "width" attrs >|= int_of_string in
  let height = X.get_attr_opt "height" attrs >|= int_of_string in
  let source =
    match X.get_attr_opt "source" attrs with
    | Some source -> `File source
    | None ->
        let format =
          X.get_attr "format" attrs |> String.lowercase_ascii |> function
          | "bmp" -> `Bmp
          | "gif" -> `Gif
          | "jpg" -> `Jpg
          | "png" -> `Png
          | s -> Util.invalid_arg "format" s in
        let data =
          X.member_with_attr "data" nodes |> Conv_data.image_data_of_xml in
        `Embed (format, data) in
  Image.make ~source ?trans ?width ?height ()

let tile_of_xml (attrs, nodes) =
  let id = X.get_attr "id" attrs |> int_of_string in
  let class_ = X.get_attr_opt "class" attrs in
  let x = X.get_attr_opt "x" attrs >|= int_of_string in
  let y = X.get_attr_opt "y" attrs >|= int_of_string in
  let width = X.get_attr_opt "width" attrs >|= int_of_string in
  let height = X.get_attr_opt "height" attrs >|= int_of_string in
  let properties = get_properties nodes in
  let image = X.member_with_attr_opt "image" nodes >|= image_of_xml in
  let objectgroup =
    X.member_opt "objectgroup" nodes
    >|= X.members_with_attr "object"
    >|= List.map object_of_xml in
  let animation =
    let frame_of_xml (attrs, _) =
      let tileid = X.get_attr "tileid" attrs |> int_of_string in
      let duration = X.get_attr "duration" attrs |> int_of_string in
      Tile0.Frame.make ~tileid ~duration in
    X.member_opt "animation" nodes
    >|= X.members_with_attr "frame"
    >|= List.map frame_of_xml in
  Tile0.make ~id ?class_ ?x ?y ?width ?height ~properties ?image ?objectgroup
    ?animation ()

let tileset_of_xml (attrs, nodes) =
  let name = X.get_attr "name" attrs in
  let class_ = X.get_attr_opt "class" attrs in
  let tilecount = X.get_attr "tilecount" attrs |> int_of_string in
  let columns = X.get_attr "columns" attrs |> int_of_string in
  let objectalignment =
    X.get_attr_opt "objectalignment" attrs >|= function
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
    | s -> Util.invalid_arg "objectalignment" s in
  let tilerendersize =
    X.get_attr_opt "tilerendersize" attrs >|= function
    | "tile" -> `Tile
    | "grid" -> `Grid
    | s -> Util.invalid_arg "tilerendersize" s in
  let fillmode =
    X.get_attr_opt "fillmode" attrs >|= function
    | "stretch" -> `Stretch
    | "preserve-aspect-fit" -> `Preserve_aspect_fit
    | s -> Util.invalid_arg "fillmode" s in
  let tileoffset =
    X.member_with_attr_opt "tileoffset" nodes >|= fun (attrs, _) ->
    let x = X.get_attr_opt "x" attrs >|= int_of_string in
    let y = X.get_attr_opt "y" attrs >|= int_of_string in
    Tileset0.Tileoffset.make ?x ?y () in
  let grid =
    X.member_with_attr_opt "grid" nodes >|= fun (attrs, _) ->
    match X.get_attr_opt "orientation" attrs with
    | Some "orthogonal" | None -> `Orthogonal
    | Some "isometric" ->
        let width = X.get_attr "width" attrs |> int_of_string in
        let height = X.get_attr "height" attrs |> int_of_string in
        `Isometric (width, height)
    | Some s -> Util.invalid_arg "orientation" s in
  let properties = get_properties nodes in
  let tiles = X.members_with_attr "tile" nodes |> List.map tile_of_xml in
  let variant =
    match X.member_with_attr_opt "image" nodes with
    | Some xml ->
        let tilewidth = X.get_attr "tilewidth" attrs |> int_of_string in
        let tileheight = X.get_attr "tileheight" attrs |> int_of_string in
        let spacing = X.get_attr_opt "spacing" attrs >|= int_of_string in
        let margin = X.get_attr_opt "margin" attrs >|= int_of_string in
        let image = image_of_xml xml in
        let single =
          Tileset0.Single.make ~tilewidth ~tileheight ?spacing ?margin image
        in
        `Single single
    | None -> `Collection in
  Tileset0.make ~name ?class_ ~tilecount ~columns ?objectalignment
    ?tilerendersize ?fillmode ?tileoffset ?grid ~properties ~variant tiles

let tileset_ref_of_xml ((attrs, _) as xml) :
    int * [`File of string | `Embed of Tileset0.t] =
  let firstgid = X.get_attr "firstgid" attrs |> int_of_string in
  match X.get_attr_opt "source" attrs with
  | None ->
      let ts = tileset_of_xml xml in
      (firstgid, `Embed ts)
  | Some source -> (firstgid, `File source)

let template_of_xml ((_, nodes) as xml) =
  let tileset =
    let+ xml' = X.member_with_attr_opt "tileset" nodes in
    match tileset_ref_of_xml xml' with
    | firstgid, `File fname -> (firstgid, fname)
    | _, `Embed _ -> Util.xml_parse xml "Embedded tileset not allowed here"
  in
  let object_ = X.member_with_attr "object" nodes |> object_of_xml in
  Template0.make ?tileset object_

let rec layer_of_xml ~type_ (attrs, nodes) =
  let id = X.get_attr_opt "id" attrs >|= int_of_string in
  let name = X.get_attr_opt "name" attrs in
  let class_ = X.get_attr_opt "class" attrs in
  let opacity = X.get_attr_opt "opacity" attrs >|= float_of_string in
  let visible = X.get_attr_opt "visible" attrs >|= bool_of_string01 in
  let tintcolor = X.get_attr_opt "tintcolor" attrs >|= Color.of_string in
  let offsetx = X.get_attr_opt "offsetx" attrs >|= float_of_string in
  let offsety = X.get_attr_opt "offsety" attrs >|= float_of_string in
  let parallaxx = X.get_attr_opt "parallaxx" attrs >|= float_of_string in
  let parallaxy = X.get_attr_opt "parallaxy" attrs >|= float_of_string in
  let properties = get_properties nodes in
  let variant =
    match type_ with
    | `Tilelayer ->
        let width = X.get_attr "width" attrs |> int_of_string in
        let height = X.get_attr "height" attrs |> int_of_string in
        let data =
          X.member_with_attr_opt "data" nodes >|= fun ((_, nodes) as xml) ->
          let dims = (width, height) in
          let chunked = X.has_member "chunk" nodes in
          Conv_data.tile_data_of_xml ~dims ~chunked xml in
        let tilelayer = Layer0.Tilelayer.make ~width ~height ?data () in
        `Tilelayer tilelayer
    | `Objectgroup ->
        let draworder =
          X.get_attr_opt "draworder" attrs >|= function
          | "index" -> `Index
          | "topdown" -> `Topdown
          | s -> Util.invalid_arg "draworder" s in
        let objects =
          X.members_with_attr "object" nodes |> List.map object_of_xml in
        let objectgroup = Layer0.Objectgroup.make ?draworder ~objects () in
        `Objectgroup objectgroup
    | `Imagelayer ->
        let image = X.member_with_attr_opt "image" nodes >|= image_of_xml in
        let repeatx = X.get_attr_opt "repeatx" attrs >|= bool_of_string01 in
        let repeaty = X.get_attr_opt "repeaty" attrs >|= bool_of_string01 in
        let imagelayer = Layer0.Imagelayer.make ?image ?repeatx ?repeaty () in
        `Imagelayer imagelayer
    | `Group ->
        let layers = X.member "layers" nodes |> get_layers in
        `Group layers in
  Layer0.make ?id ?name ?class_ ?opacity ?visible ?tintcolor ?offsetx ?offsety
    ?parallaxx ?parallaxy ~properties ~variant ()

and get_layers nodes =
  Fun.flip List.filter_map nodes @@ function
  | `El (((_, name), attrs), nodes) ->
      let type_ =
        match name with
        | "layer" -> Some `Tilelayer
        | "objectgroup" -> Some `Objectgroup
        | "imagelayer" -> Some `Imagelayer
        | "group" -> Some `Group
        | _ -> None in
      type_ >|= fun type_ -> layer_of_xml ~type_ @@ (attrs, nodes)
  | `Data _ -> None

and map_of_xml (attrs, nodes) =
  let version = X.get_attr "version" attrs in
  let tiledversion = X.get_attr_opt "tiledversion" attrs in
  let class_ = X.get_attr_opt "class" attrs in
  let renderorder =
    X.get_attr_opt "renderorder" attrs >|= function
    | "right-down" -> `Right_down
    | "right-up" -> `Right_up
    | "left-down" -> `Left_down
    | "left-up" -> `Left_up
    | s -> Util.invalid_arg "renderorder" s in
  let compressionlevel =
    X.get_attr_opt "compressionlevel" attrs >|= int_of_string in
  let width = X.get_attr "width" attrs |> int_of_string in
  let height = X.get_attr "height" attrs |> int_of_string in
  let tilewidth = X.get_attr "tilewidth" attrs |> int_of_string in
  let tileheight = X.get_attr "tileheight" attrs |> int_of_string in
  let parallaxoriginx =
    X.get_attr_opt "parallaxoriginx" attrs >|= int_of_string in
  let parallaxoriginy =
    X.get_attr_opt "parallaxoriginy" attrs >|= int_of_string in
  let backgroundcolor =
    X.get_attr_opt "backgroundcolor" attrs >|= Color.of_string in
  let nextlayerid = X.get_attr_opt "nextlayerid" attrs >|= int_of_string in
  let nextobjectid = X.get_attr_opt "nextobjectid" attrs >|= int_of_string in
  let infinite = X.get_attr_opt "infinite" attrs >|= bool_of_string01 in
  let properties = get_properties nodes in
  let tilesets =
    (* TODO sort and ensure gids are reasonable *)
    X.members_with_attr "tileset" nodes |> List.map tileset_ref_of_xml in
  let layers = get_layers nodes in
  let variant =
    let staggeraxis () =
      match X.get_attr "staggeraxis" attrs with
      | "x" -> `X
      | "y" -> `Y
      | s -> Util.invalid_arg "staggeraxis" s in
    let staggerindex () =
      match X.get_attr "staggerindex" attrs with
      | "even" -> `Even
      | "odd" -> `Odd
      | s -> Util.invalid_arg "staggerindex" s in
    match X.get_attr "orientation" attrs with
    | "orthogonal" -> `Orthogonal
    | "isometric" -> `Isometric
    | "staggered" ->
        let staggered =
          let staggeraxis = staggeraxis () in
          let staggerindex = staggerindex () in
          Map0.Staggered.make ~staggeraxis ~staggerindex in
        `Staggered staggered
    | "hexagonal" ->
        let hexagonal =
          let staggeraxis = staggeraxis () in
          let staggerindex = staggerindex () in
          let hexsidelength =
            X.get_attr "hexsidelength" attrs |> int_of_string in
          Map0.Hexagonal.make ~hexsidelength ~staggeraxis ~staggerindex in
        `Hexagonal hexagonal
    | s -> Util.invalid_arg "orientation" s in
  Map0.make ~version ?tiledversion ?class_ ?renderorder ?compressionlevel
    ~width ~height ~tilewidth ~tileheight ?parallaxoriginx ?parallaxoriginy
    ?backgroundcolor ?nextlayerid ?nextobjectid ?infinite ~properties ~tilesets
    ~layers ~variant ()
