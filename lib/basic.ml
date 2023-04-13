open Util.Option.Infix

module Property = struct
  include Property0

  let class_ t = match value t with `Class _ -> propertytype t | _ -> None
  let properties t = match value t with `Class props -> props | _ -> []

  let rec relocate t dir =
    let name = name t in
    let propertytype = propertytype t in
    let value =
      match value t with
      | `File fname -> `File (Filename.concat dir fname)
      | `Class props ->
          let props = List.map (Fun.flip relocate dir) props in
          `Class props
      | v -> v in
    make ~name ?propertytype ~value ()

  include (val Props.make_shallow properties)
end

module Object = struct
  module Text = struct
    module Halign = struct
      type t = [`Center | `Right | `Justify | `Left]
      [@@deriving eq, ord, show {with_path = false}]
    end

    type halign = Halign.t

    module Valign = struct
      type t = [`Center | `Bottom | `Top]
      [@@deriving eq, ord, show {with_path = false}]
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
    [@@deriving eq, ord, show {with_path = false}, make]

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
    [@@deriving eq, ord, show {with_path = false}]
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
  [@@deriving eq, ord, show {with_path = false}, make]

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

  let relocate t dir =
    { t with
      template = t.template >|= Filename.concat dir;
      properties = List.map (Fun.flip Property.relocate dir) t.properties }

  include ((val Props.make_shallow properties) : Props.S with type t := t)
end

module Layer = struct
  module Tilelayer = struct
    type t = {width : int; height : int; data : Data.t option}
    [@@deriving eq, ord, show {with_path = false}, make]

    let width t = t.width
    let height t = t.height
    let data t = t.data
  end

  type tilelayer = Tilelayer.t

  module Objectgroup = struct
    module Draworder = struct
      type t = [`Topdown | `Index]
      [@@deriving eq, ord, show {with_path = false}]
    end

    type draworder = Draworder.t

    type t = {draworder : Draworder.t option; objects : Object.t list}
    [@@deriving eq, ord, show {with_path = false}, make]

    let draworder t = t.draworder |? `Topdown
    let objects t = t.objects
    let get_object t id = List.find_opt (fun o -> Object.id o = id) (objects t)
    let get_object_exn t id =
      match get_object t id with
      | Some o -> o
      | None -> Util.object_not_found id
    let set_objects t objects = {t with objects}

    let relocate t dir =
      {t with objects = List.map (Fun.flip Object.relocate dir) t.objects}
  end

  type objectgroup = Objectgroup.t

  module Imagelayer = struct
    type t =
      {image : Image.t option; repeatx : bool option; repeaty : bool option}
    [@@deriving eq, ord, show {with_path = false}, make]

    let image t = t.image
    let repeatx t = t.repeatx |? false
    let repeaty t = t.repeaty |? false

    let relocate t dir =
      {t with image = t.image >|= Fun.flip Image.relocate dir}
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
    [@@deriving eq, ord, show {with_path = false}, make]

    let rec relocate t dir =
      { t with
        properties = List.map (Fun.flip Property.relocate dir) t.properties;
        variant = relocate_variant t.variant dir }

    and relocate_variant v dir =
      match v with
      | `Tilelayer _ -> v
      | `Objectgroup og -> `Objectgroup (Objectgroup.relocate og dir)
      | `Imagelayer il -> `Imagelayer (Imagelayer.relocate il dir)
      | `Group ts -> `Group (List.map (Fun.flip relocate dir) ts)
  end

  include Layer

  module Variant = struct
    type t =
      [ `Tilelayer of Tilelayer.t
      | `Objectgroup of Objectgroup.t
      | `Imagelayer of Imagelayer.t
      | `Group of Layer.t list ]
    [@@deriving eq, ord, show {with_path = false}]

    let relocate t dir = relocate_variant t dir
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

  include ((val Props.make_shallow properties) : Props.S with type t := t)
end

module Tile = struct
  module Frame = struct
    type t = {tileid : int; duration : int}
    [@@deriving eq, ord, show {with_path = false}, make]

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
  [@@deriving eq, ord, show {with_path = false}, make]

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

  let relocate t dir =
    { t with
      properties = List.map (Fun.flip Property.relocate dir) t.properties;
      image = t.image >|= Fun.flip Image.relocate dir;
      objectgroup = List.map (Fun.flip Object.relocate dir) t.objectgroup }

  include ((val Props.make_shallow properties) : Props.S with type t := t)
end

module Tileset = struct
  module Int_map = Stdlib.Map.Make (Int)

  module Tileoffset = struct
    type t = {x : int option; y : int option}
    [@@deriving eq, ord, show {with_path = false}, make]

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
    [@@deriving eq, ord, show {with_path = false}, make]

    let tilecount t = t.tilecount
    let tilewidth t = t.tilewidth
    let tileheight t = t.tileheight
    let spacing t = t.spacing |? 0
    let margin t = t.margin |? 0
    let image t = t.image
    let relocate t dir = {t with image = Image.relocate t.image dir}
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
    [@@deriving eq, ord, show {with_path = false}]
  end

  type objectalignment = Objectalignment.t

  module Tilerendersize = struct
    type t = [`Tile | `Grid] [@@deriving eq, ord, show {with_path = false}]
  end

  type tilerendersize = Tilerendersize.t

  module Fillmode = struct
    type t = [`Stretch | `Preserve_aspect_fit]
    [@@deriving eq, ord, show {with_path = false}]
  end

  type fillmode = Fillmode.t

  module Grid = struct
    type t = [`Orthogonal | `Isometric of int * int]
    [@@deriving eq, ord, show {with_path = false}]
  end

  type grid = Grid.t

  module Variant = struct
    type t = [`Single of Single.t | `Collection]
    [@@deriving eq, ord, show {with_path = false}]

    let relocate t dir =
      match t with `Single s -> `Single (Single.relocate s dir) | _ -> t
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
  [@@deriving eq, ord, show {with_path = false}]

  let make ~name ?class_ ~columns ?objectalignment ?tilerendersize ?fillmode
      ?tileoffset ?grid ?(properties = []) ~variant tiles =
    let tiles =
      List.to_seq tiles
      |> Seq.map (fun tile -> (Tile.id tile, tile))
      |> Int_map.of_seq in
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

  let relocate t dir =
    { t with
      properties = List.map (Fun.flip Property.relocate dir) t.properties;
      tiles = Int_map.map (Fun.flip Tile.relocate dir) t.tiles;
      variant = Variant.relocate t.variant dir }

  include ((val Props.make_shallow properties) : Props.S with type t := t)
end

module Map = struct
  module Staggeraxis = struct
    type t = [`X | `Y] [@@deriving eq, ord, show {with_path = false}]
  end

  type staggeraxis = Staggeraxis.t

  module Staggerindex = struct
    type t = [`Even | `Odd] [@@deriving eq, ord, show {with_path = false}]
  end

  type staggerindex = Staggerindex.t

  module Staggered = struct
    type t = {staggeraxis : Staggeraxis.t; staggerindex : Staggerindex.t}
    [@@deriving eq, ord, show {with_path = false}, make]

    let staggeraxis t = t.staggeraxis
    let staggerindex t = t.staggerindex
  end

  type staggered = Staggered.t

  module Hexagonal = struct
    type t =
      { hexsidelength : int;
        staggeraxis : Staggeraxis.t;
        staggerindex : Staggerindex.t }
    [@@deriving eq, ord, show {with_path = false}, make]

    let hexsidelength t = t.hexsidelength
    let staggeraxis t = t.staggeraxis
    let staggerindex t = t.staggerindex
  end

  type hexagonal = Hexagonal.t

  module Renderorder = struct
    type t = [`Right_down | `Right_up | `Left_down | `Left_up]
    [@@deriving eq, ord, show {with_path = false}]
  end

  type renderorder = Renderorder.t

  module Variant = struct
    type t =
      [ `Orthogonal
      | `Isometric
      | `Staggered of Staggered.t
      | `Hexagonal of Hexagonal.t ]
    [@@deriving eq, ord, show {with_path = false}]
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
  [@@deriving eq, ord, show {with_path = false}]

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

  let relocate t dir =
    { t with
      properties = List.map (Fun.flip Property.relocate dir) t.properties;
      tilesets =
        List.map
          (fun (firstgid, fname) -> (firstgid, Filename.concat dir fname))
          t.tilesets;
      layers = List.map (Fun.flip Layer.relocate dir) t.layers }

  include ((val Props.make_shallow properties) : Props.S with type t := t)
end

module Template = struct
  type t = {tileset : (int * string) option; object_ : Object.t}
  [@@deriving eq, ord, show {with_path = false}]

  let make ?tileset object_ =
    match Object.template object_ with
    | Some _ -> Util.nested_template ()
    | None -> {tileset; object_}

  let tileset t = t.tileset
  let object_ t = t.object_

  let relocate t dir =
    { tileset =
        ( t.tileset >|= fun (firstgid, fname) ->
          (firstgid, Filename.concat dir fname) );
      object_ = Object.relocate t.object_ dir }
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
    [@@deriving eq, ord, show {with_path = false}]
  end

  type useas = Useas.t

  type t = {useas : Useas.t list; members : Property.t list}
  [@@deriving eq, ord, show {with_path = false}]

  let make ~useas ~members =
    if useas = [] then Util.invalid_arg "useas" "[]" ;
    let members = List.sort_uniq Property.compare members in
    {useas; members}

  let useas t = t.useas
  let members t = t.members

  let relocate t dir =
    {t with members = List.map (Fun.flip Property.relocate dir) t.members}
end

module Enum = struct
  module Storagetype = struct
    type t = [`Int | `String] [@@deriving eq, ord, show {with_path = false}]
  end

  type storagetype = Storagetype.t

  type t =
    {storagetype : Storagetype.t; valuesasflags : bool; values : string array}
  [@@deriving eq, ord, show {with_path = false}]

  let make ~storagetype ~valuesasflags values =
    let values = Array.of_list values in
    {storagetype; valuesasflags; values}

  let storagetype t = t.storagetype
  let valuesasflags t = t.valuesasflags
  let values t = Array.to_list t.values

  let read_as_int t v =
    let n = Array.length t.values in
    match (storagetype t, valuesasflags t, v) with
    | `Int, true, `Int x ->
        Some Int.(lognot 0 |> Fun.flip shift_left n |> lognot |> logand x)
    | `Int, false, `Int x when x < n -> Some x
    | `String, true, `String s ->
        let vs = String.split_on_char ',' s in
        let x, _ =
          Array.fold_right
            (fun v (x, m) ->
              let x' = if List.mem v vs then x lor m else x in
              (x', m lsr 1) )
            t.values
            (0, 1 lsl (n - 1)) in
        Some x
    | `String, false, `String s ->
        let rec aux i =
          if i < 0 then None
          else if s = t.values.(i) then Some i
          else aux (i - 1) in
        aux (n - 1)
    | _ -> None

  let read_as_string t v =
    match (storagetype t, valuesasflags t, v) with
    | `Int, true, `Int x ->
        let vs, _ =
          Array.fold_right
            (fun v (vs, n) ->
              let vs' = if (1 lsl n) land x = 0 then vs else v :: vs in
              (vs', n - 1) )
            t.values
            ([], Array.length t.values - 1) in
        Some (String.concat "," vs)
    | `Int, false, `Int x when x < Array.length t.values -> Some t.values.(x)
    | `String, true, `String s ->
        let vs = String.split_on_char ',' s in
        values t
        |> List.filter (fun v -> List.mem v vs)
        |> String.concat "," |> Option.some
    | `String, false, `String s when Array.mem s t.values -> Some s
    | _ -> None

  let read_as_alist t v =
    match (storagetype t, valuesasflags t, v) with
    | `Int, true, `Int x ->
        let alist =
          List.mapi (fun i v -> (v, (1 lsl i) land x <> 0)) (values t) in
        Some alist
    | `Int, false, `Int x when x < Array.length t.values ->
        let alist = List.mapi (fun i v -> (v, i = x)) (values t) in
        Some alist
    | `String, true, `String s ->
        let vs = String.split_on_char ',' s in
        let alist = List.map (fun v -> (v, List.mem v vs)) (values t) in
        Some alist
    | `String, false, `String s ->
        let alist, found =
          List.fold_right
            (fun v (alist, found) ->
              let found' = v = s in
              ((v, found') :: alist, found || found') )
            (values t) ([], false) in
        if found then Some alist else None
    | _ -> None
end

module Customtype = struct
  module Variant = struct
    type t = [`Class of Class.t | `Enum of Enum.t]
    [@@deriving eq, ord, show {with_path = false}]

    let relocate t dir =
      match t with `Class c -> `Class (Class.relocate c dir) | _ -> t
  end

  type variant = Variant.t

  type t = {id : int; name : string; variant : Variant.t}
  [@@deriving eq, ord, show {with_path = false}, make]

  let id t = t.id
  let name t = t.name
  let variant t = t.variant

  let relocate t dir = {t with variant = Variant.relocate t.variant dir}
end
