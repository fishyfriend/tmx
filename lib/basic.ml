open Util.Option.Infix

module type Properties0 = sig
  type property
  type t

  val properties : t -> property list
  val get_property : string -> t -> property option
  val get_property_exn : string -> t -> property
end

module Property0 = struct
  type t = {name : string; propertytype : string option; value : value}

  and value =
    [ `String of string
    | `Int of int
    | `Float of float
    | `Bool of bool
    | `Color of Color.t
    | `File of string
    | `Object of int
    | `Class of t list ]
  [@@deriving eq, ord, show]

  module Value = struct type t = value [@@deriving eq, ord, show] end

  let make ~name ?propertytype ~value () =
    let value =
      match value with
      | `Class props -> `Class (List.sort_uniq compare props)
      | _ -> value in
    {name; propertytype; value}

  let name t = t.name
  let propertytype t = t.propertytype
  let value t = t.value
  let properties t = match value t with `Class props -> props | _ -> []
end

module type Properties = Properties0 with type property := Property0.t

module MakeProps (T : sig
  type t val properties : t -> Property0.t list
end) : Properties with type t := T.t = struct
  let properties t = T.properties t

  let get_property k t =
    List.find_opt (fun p -> Property0.name p = k) (properties t)

  let get_property_exn k t =
    match get_property k t with
    | Some p -> p
    | None -> Util.not_found "property" k
end

module Property = struct include Property0 include MakeProps (Property0) end

module Object = struct
  module Text = struct
    module Halign = struct
      type t = [`Center | `Right | `Justify | `Left] [@@deriving eq, ord, show]
    end

    type halign = Halign.t

    module Valign = struct
      type t = [`Center | `Bottom | `Top] [@@deriving eq, ord, show]
    end

    type valign = Valign.t

    type t =
      { text : string; [@main]
        fontfamily : string option;
        pixelsize : int option;
        wrap : bool option;
        color : Color.t option;
        bold : bool option;
        italic : bool option;
        underline : bool option;
        strikeout : bool option;
        kerning : bool option;
        halign : Halign.t option;
        valign : Valign.t option }
    [@@deriving eq, ord, show, make]

    let text t = t.text
    let fontfamily t = t.fontfamily |? "sans-serif"
    let pixelsize t = t.pixelsize |? 16
    let wrap t = t.wrap |? false
    let color t = t.color |? Color.black
    let bold t = t.bold |? false
    let italic t = t.italic |? false
    let underline t = t.underline |? false
    let strikeout t = t.strikeout |? false
    let kerning t = t.kerning |? true
    let halign t = t.halign |? `Left
    let valign t = t.valign |? `Top
  end

  type text = Text.t

  module Shape = struct
    type t =
      [ `Rectangle
      | `Ellipse
      | `Point
      | `Polygon of (float * float) list
      | `Polyline of (float * float) list
      | `Text of Text.t
      | `Tile of Gid.t ]
    [@@deriving eq, ord, show]
  end

  type shape = Shape.t

  type t =
    { id : int option;
      name : string option;
      class_ : string option;
      x : float option;
      y : float option;
      width : float option;
      height : float option;
      rotation : float option;
      visible : bool option;
      template : string option;
      properties : Property.t list;
      shape : Shape.t option }
  [@@deriving eq, ord, show, make]

  let id t = t.id |? 0
  let name t = t.name
  let class_ t = t.class_
  let x t = t.x |? 0.
  let y t = t.y |? 0.
  let rotation t = t.rotation |? 0.
  let visible t = t.visible |? true
  let template t = t.template
  let properties t = t.properties
  let shape t = t.shape |? `Rectangle

  let set_shape t shape = {t with shape}

  (* TODO: this will go away? *)
  let raw_shape t = t.shape

  let width t =
    match shape t with
    | `Point -> 0.
    | `Polygon pts | `Polyline pts ->
        let xmin, xmax =
          List.fold_left
            (fun (xmin, xmax) (x, _) -> (min xmin x, max xmax x))
            (0., 0.) pts in
        abs_float (xmax -. xmin)
    | _ -> 0.

  let height t =
    match shape t with
    | `Point -> 0.
    | `Polygon pts | `Polyline pts ->
        let ymin, ymax =
          List.fold_left
            (fun (ymin, ymax) (_, y) -> (min ymin y, max ymax y))
            (0., 0.) pts in
        abs_float (ymax -. ymin)
    | _ -> 0.

  include MakeProps (struct type nonrec t = t let properties = properties end)
end

module Layer = struct
  module Tilelayer = struct
    type t = {width : int; height : int; data : Data.t option}
    [@@deriving eq, ord, show, make]

    let width t = t.width
    let height t = t.height
    let data t = t.data
  end

  type tilelayer = Tilelayer.t

  module Objectgroup = struct
    module Draworder = struct
      type t = [`Topdown | `Index] [@@deriving eq, ord, show]
    end

    type draworder = Draworder.t

    type t = {draworder : Draworder.t option; objects : Object.t list}
    [@@deriving eq, ord, show, make]

    let draworder t = t.draworder |? `Topdown
    let objects t = t.objects
    let get_object t id = List.find_opt (fun o -> Object.id o = id) (objects t)
    let get_object_exn t id =
      match get_object t id with
      | Some o -> o
      | None -> Util.object_not_found id
    let set_objects t objects = {t with objects}
  end

  type objectgroup = Objectgroup.t

  module Imagelayer = struct
    type t =
      {image : Image.t option; repeatx : bool option; repeaty : bool option}
    [@@deriving eq, ord, show, make]

    let image t = t.image
    let repeatx t = t.repeatx |? false
    let repeaty t = t.repeaty |? false
  end

  type imagelayer = Imagelayer.t

  module Layer = struct
    type t =
      { id : int option;
        name : string option;
        class_ : string option;
        opacity : float option;
        visible : bool option;
        tintcolor : Color.t option;
        offsetx : float option;
        offsety : float option;
        parallaxx : float option;
        parallaxy : float option;
        properties : Property.t list;
        variant :
          [ `Tilelayer of Tilelayer.t
          | `Objectgroup of Objectgroup.t
          | `Imagelayer of Imagelayer.t
          | `Group of t list ] }
    [@@deriving eq, ord, show, make]
  end

  include Layer

  module Variant = struct
    type t =
      [ `Tilelayer of Tilelayer.t
      | `Objectgroup of Objectgroup.t
      | `Imagelayer of Imagelayer.t
      | `Group of Layer.t list ]
    [@@deriving eq, ord, show]
  end

  type variant = Variant.t

  let id t = t.id |? 0
  let name t = t.name |? ""
  let class_ t = t.class_
  let opacity t = t.opacity |? 1.
  let visible t = t.visible |? true
  let tintcolor t = t.tintcolor
  let offsetx t = t.offsetx |? 0.
  let offsety t = t.offsety |? 0.
  let parallaxx t = t.parallaxx |? 1.
  let parallaxy t = t.parallaxy |? 1.
  let properties t = t.properties
  let variant t = t.variant
  let set_variant t variant = {t with variant}

  let rec objects t =
    match t.variant with
    | `Objectgroup og -> Objectgroup.objects og
    | `Group ts -> List.concat_map objects ts
    | _ -> []

  let rec get_object t id =
    match t.variant with
    | `Objectgroup og -> Objectgroup.get_object og id
    | `Group ts -> List.find_map (fun t -> get_object t id) ts
    | _ -> None

  let get_object_exn t id =
    match get_object t id with Some o -> o | None -> Util.object_not_found id

  include MakeProps (struct type nonrec t = t let properties = properties end)
end

module Tile = struct
  module Frame = struct
    type t = {tileid : int; duration : int} [@@deriving eq, ord, show, make]

    let tileid t = t.tileid
    let duration t = t.duration
  end

  type frame = Frame.t

  type t =
    { id : int;
      class_ : string option;
      x : int option;
      y : int option;
      width : int option;
      height : int option;
      properties : Property.t list;
      image : Image.t option;
      objectgroup : Object.t list;
      animation : Frame.t list }
  [@@deriving eq, ord, show, make]

  let id t = t.id
  let class_ t = t.class_
  let x t = t.x |? 0
  let y t = t.y |? 0
  let image t = t.image
  let properties t = t.properties
  let objectgroup t = t.objectgroup
  let animation t = t.animation

  let width t =
    match (t.width, image t) with
    | None, Some img -> Image.width img
    | w, _ -> w

  let height t =
    match (t.height, image t) with
    | None, Some img -> Image.height img
    | h, _ -> h

  let set_image t image = {t with image}
  let set_x t x = {t with x}
  let set_y t y = {t with y}
  let set_width t width = {t with width}
  let set_height t height = {t with height}

  include MakeProps (struct type nonrec t = t let properties = properties end)
end

module Tileset = struct
  module Int_map = Stdlib.Map.Make (Int)

  module Tileoffset = struct
    type t = {x : int option; y : int option} [@@deriving eq, ord, show, make]

    let x t = t.x |? 0
    let y t = t.y |? 0
  end

  type tileoffset = Tileoffset.t

  module Single = struct
    type t =
      { tilecount : int;
        tilewidth : int;
        tileheight : int;
        spacing : int option;
        margin : int option;
        image : Image.t [@main] }
    [@@deriving eq, ord, show, make]

    let tilecount t = t.tilecount
    let tilewidth t = t.tilewidth
    let tileheight t = t.tileheight
    let spacing t = t.spacing |? 0
    let margin t = t.margin |? 0
    let image t = t.image
  end

  type single = Single.t

  module Objectalignment = struct
    type t =
      [ `Unspecified
      | `Topleft
      | `Top
      | `Topright
      | `Left
      | `Center
      | `Right
      | `Bottomleft
      | `Bottom
      | `Bottomright ]
    [@@deriving eq, ord, show]
  end

  type objectalignment = Objectalignment.t

  module Tilerendersize = struct
    type t = [`Tile | `Grid] [@@deriving eq, ord, show]
  end

  type tilerendersize = Tilerendersize.t

  module Fillmode = struct
    type t = [`Stretch | `Preserve_aspect_fit] [@@deriving eq, ord, show]
  end

  type fillmode = Fillmode.t

  module Grid = struct
    type t = [`Orthogonal | `Isometric of int * int] [@@deriving eq, ord, show]
  end

  type grid = Grid.t

  module Variant = struct
    type t = [`Single of Single.t | `Collection] [@@deriving eq, ord, show]
  end

  type variant = Variant.t

  type t =
    { name : string;
      class_ : string option;
      columns : int;
      objectalignment : Objectalignment.t option;
      tilerendersize : Tilerendersize.t option;
      fillmode : Fillmode.t option;
      tileoffset : Tileoffset.t option;
      grid : Grid.t option;
      properties : Property.t list;
      tiles : Tile.t Int_map.t; [@opaque] [@main]
      variant : Variant.t }
  [@@deriving eq, ord, show]

  let make ~name ?class_ ~columns ?objectalignment ?tilerendersize ?fillmode
      ?tileoffset ?grid ?(properties = []) ~variant tiles =
    let tile_map_of_list tiles =
      List.to_seq tiles
      |> Seq.map (fun tile -> (Tile.id tile, tile))
      |> Int_map.of_seq in
    let tiles =
      match variant with
      | `Single _ -> tile_map_of_list tiles
      | `Collection -> tile_map_of_list tiles in
    { name;
      class_;
      columns;
      objectalignment;
      tilerendersize;
      fillmode;
      tileoffset;
      grid;
      properties;
      tiles;
      variant }

  let name t = t.name
  let class_ t = t.class_
  let columns t = t.columns
  let grid t = t.grid |? `Orthogonal
  let tilerendersize t = t.tilerendersize |? `Tile
  let fillmode t = t.fillmode |? `Stretch
  let tileoffset t = t.tileoffset
  let properties t = t.properties
  let variant t = t.variant
  let tiles t = Int_map.bindings t.tiles |> List.map snd

  let objectalignment t =
    match (t.objectalignment, grid t) with
    | Some x, _ -> x
    | None, `Orthogonal -> `Bottomleft
    | None, `Isometric _ -> `Bottom

  let tilecount t =
    match variant t with
    | `Single single -> Single.tilecount single
    | `Collection -> Int_map.cardinal t.tiles

  let max_id t =
    match variant t with
    | `Single _ -> tilecount t - 1
    | `Collection -> Int_map.fold (fun id _ max_id -> max id max_id) t.tiles 0

  let get_tile t id : Tile.t option =
    match variant t with
    | `Collection -> Int_map.find_opt id t.tiles
    | `Single single ->
        if id >= tilecount t then None
        else
          let tile =
            match Int_map.find_opt id t.tiles with
            | Some tile -> tile
            | None -> Tile.make ~id () in
          let image = Single.image single in
          let width = Single.tilewidth single in
          let height = Single.tileheight single in
          let margin = Single.margin single in
          let spacing = Single.spacing single in
          let columns = columns t in
          let col = id mod columns in
          let row = id / columns in
          let x = margin + (col * (width + spacing)) in
          let y = margin + (row * (height + spacing)) in
          Some tile
          >|= Fun.flip Tile.set_image (Some image)
          >|= Fun.flip Tile.set_x (Some x)
          >|= Fun.flip Tile.set_y (Some y)
          >|= Fun.flip Tile.set_width (Some width)
          >|= Fun.flip Tile.set_height (Some height)

  include MakeProps (struct type nonrec t = t let properties = properties end)
end

module Map = struct
  module Staggeraxis = struct type t = [`X | `Y] [@@deriving eq, ord, show] end

  type staggeraxis = Staggeraxis.t

  module Staggerindex = struct
    type t = [`Even | `Odd] [@@deriving eq, ord, show]
  end

  type staggerindex = Staggerindex.t

  module Staggered = struct
    type t = {staggeraxis : Staggeraxis.t; staggerindex : Staggerindex.t}
    [@@deriving eq, ord, show, make]

    let staggeraxis t = t.staggeraxis
    let staggerindex t = t.staggerindex
  end

  type staggered = Staggered.t

  module Hexagonal = struct
    type t =
      { hexsidelength : int;
        staggeraxis : Staggeraxis.t;
        staggerindex : Staggerindex.t }
    [@@deriving eq, ord, show, make]

    let hexsidelength t = t.hexsidelength
    let staggeraxis t = t.staggeraxis
    let staggerindex t = t.staggerindex
  end

  type hexagonal = Hexagonal.t

  module Renderorder = struct
    type t = [`Right_down | `Right_up | `Left_down | `Left_up]
    [@@deriving eq, ord, show]
  end

  type renderorder = Renderorder.t

  module Variant = struct
    type t =
      [ `Orthogonal
      | `Isometric
      | `Staggered of Staggered.t
      | `Hexagonal of Hexagonal.t ]
    [@@deriving eq, ord, show]
  end

  type variant = Variant.t

  type t =
    { version : string;
      tiledversion : string option;
      class_ : string option;
      renderorder : Renderorder.t option;
      compressionlevel : int option;
      width : int;
      height : int;
      tilewidth : int;
      tileheight : int;
      parallaxoriginx : int option;
      parallaxoriginy : int option;
      backgroundcolor : Color.t option;
      infinite : bool option;
      properties : Property.t list;
      tilesets : (int * string) list;
      layers : Layer.t list;
      variant : Variant.t }
  [@@deriving eq, ord, show]

  let make ~version ?tiledversion ?class_ ?renderorder ?compressionlevel ~width
      ~height ~tilewidth ~tileheight ?parallaxoriginx ?parallaxoriginy
      ?backgroundcolor ?infinite ?(properties = []) ?(tilesets = [])
      ?(layers = []) ~variant () =
    let layers =
      let cmp l l' = Int.compare (Layer.id l) (Layer.id l') in
      let layers' = List.sort_uniq cmp layers in
      if List.compare_lengths layers layers' = 0 then layers'
      else Util.invalid_arg "layers" "id not unique" in
    let tilesets =
      let tilesets' =
        let cmp (gid, _) (gid', _) = Int.compare gid gid' in
        List.sort_uniq cmp tilesets in
      if List.compare_lengths tilesets tilesets' = 0 then tilesets'
      else Util.invalid_arg "tilesets" "firstgid not unique" in
    { version;
      tiledversion;
      class_;
      renderorder;
      compressionlevel;
      width;
      height;
      tilewidth;
      tileheight;
      parallaxoriginx;
      parallaxoriginy;
      backgroundcolor;
      infinite;
      properties;
      tilesets;
      layers;
      variant }

  let version t = t.version
  let tiledversion t = t.tiledversion
  let class_ t = t.class_
  let renderorder t = t.renderorder |? `Right_down
  let compressionlevel t = t.compressionlevel |? -1
  let width t = t.width
  let height t = t.height
  let tilewidth t = t.tilewidth
  let tileheight t = t.tileheight
  let parallaxoriginx t = t.parallaxoriginx |? 0
  let parallaxoriginy t = t.parallaxoriginy |? 0
  let backgroundcolor t = t.backgroundcolor |? Color.trans
  let infinite t = t.infinite |? false
  let properties t = t.properties
  let tilesets t = t.tilesets
  let layers t = t.layers
  let variant t = t.variant

  let set_layers t layers = {t with layers}

  let objects t = List.concat_map Layer.objects (layers t)
  let get_object t id = List.find_opt (fun o -> Object.id o = id) (objects t)
  let get_object_exn t id =
    match get_object t id with Some o -> o | None -> Util.object_not_found id

  let nextlayerid t =
    List.fold_left (fun id l -> max id (Layer.id l + 1)) 0 (layers t)

  let nextobjectid t =
    List.fold_left
      (fun id l ->
        List.fold_left
          (fun id' o -> max id' (Object.id o + 1))
          id (Layer.objects l) )
      0 (layers t)

  include MakeProps (struct type nonrec t = t let properties = properties end)
end

module Template = struct
  type t = {tileset : (int * string) option; object_ : Object.t}
  [@@deriving eq, ord, show]

  let make ?tileset object_ =
    match Object.template object_ with
    | Some _ -> Util.nested_template ()
    | None -> {tileset; object_}

  let tileset t = t.tileset
  let object_ t = t.object_
end

module Class = struct
  module Useas = struct
    type t =
      [ `Property
      | `Map
      | `Layer
      | `Object
      | `Tile
      | `Tileset
      | `Wangcolor
      | `Wangset ]
    [@@deriving eq, ord, show]
  end

  type useas = Useas.t

  type t = {useas : Useas.t list; members : Property.t list}
  [@@deriving eq, ord, show]

  let make ~useas ~members =
    if useas = [] then Util.invalid_arg "useas" "[]" ;
    let members = List.sort_uniq Property.compare members in
    {useas; members}

  let useas t = t.useas
  let members t = t.members
end

module Enum = struct
  module Storagetype = struct
    type t = [`Int | `String] [@@deriving eq, ord, show]
  end

  type storagetype = Storagetype.t

  type t =
    {storagetype : Storagetype.t; valuesasflags : bool; values : string array}
  [@@deriving eq, ord, show]

  let make ~storagetype ~valuesasflags values =
    let values = Array.of_list values in
    {storagetype; valuesasflags; values}

  let storagetype t = t.storagetype
  let valuesasflags t = t.valuesasflags
  let values t = Array.to_list t.values
end

module Customtype = struct
  module Variant = struct
    type t = [`Class of Class.t | `Enum of Enum.t] [@@deriving eq, ord, show]
  end

  type variant = Variant.t

  type t = {id : int; name : string; variant : Variant.t}
  [@@deriving eq, ord, show, make]

  let id t = t.id
  let name t = t.name
  let variant t = t.variant
end
