(* TODO: move auxiliary functions like reloc & map_gids to an [Aux] module *)

module Make (Getters : Sigs.Getters) : Sigs.Core_generic = struct
  open Util.Option.Infix
  open Types
  open Getters

  module Error = Error

  type error = Error.t

  module Color = Color

  type color = Color.t

  module Gid = Gid

  type gid = Gid.t

  module Data = Data

  type data = Data.t

  module Image = Image

  type image = Image.t

  let get_class_members name ~useas =
    get_class name ~useas >|= fun c -> c.members

  let std_plists ~class_ ~properties ~useas x =
    let class_props = class_ x >>= get_class_members ~useas |? [] in
    let own_props = properties x in
    [class_props; own_props]

  let make_std_props ~class_ ~properties ~useas =
    let property_lists x = std_plists ~class_ ~properties ~useas x in
    Props.make ~strict:false ~property_lists

  module Property = struct
    type t = Types.property =
      {name : string; propertytype : string option; value : value}

    and value =
      [ `String of string
      | `Int of int
      | `Float of float
      | `Bool of bool
      | `Color of Color.t
      | `File of string
      | `Object of int
      | `Class of t list ]
    [@@deriving eq, ord, show {with_path = false}]

    module Value = struct
      type t = value [@@deriving eq, ord, show {with_path = false}]
    end

    let make ~name ?propertytype ~value () : t =
      let value =
        match value with
        | `Class props ->
            let compare t1 t2 = String.compare t1.name t2.name in
            `Class (List.sort_uniq compare props)
        | _ -> value in
      {name; propertytype; value}

    let name t = t.name
    let propertytype t = t.propertytype
    let value t = t.value

    let class_ t = match value t with `Class _ -> propertytype t | _ -> None
    let properties t = match value t with `Class ts -> ts | _ -> []
    let property_lists t = std_plists ~class_ ~properties ~useas:`Property t

    module P = (val Props.make ~strict:true ~property_lists)

    include (P : Props.S with type t := t)

    let rec reloc t ~from_ ~to_ =
      let value =
        match t.value with
        | `File fname -> `File (Util.Filename.reloc fname ~from_ ~to_)
        | `Class ts -> `Class (List.map (reloc ~from_ ~to_) ts)
        | v -> v in
      {t with value}
  end

  type property = Property.t

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

      type t = Types.text =
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

      let map_gids f t = match t with `Tile gid -> `Tile (f gid) | _ -> t
    end

    type shape = Shape.t

    type t = Types.object_ =
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
    let x t = t.x |? 0.
    let y t = t.y |? 0.
    let template t = t.template
    let properties t = t.properties

    let proto t = t.template >>= get_template >|= fun tem -> tem.object_
    let with_proto t f = f t >>? fun () -> proto t >>= f
    let name t = with_proto t (fun t -> t.name)
    let rotation t = with_proto t (fun t -> t.rotation) |? 0.
    let visible t = with_proto t (fun t -> t.visible) |? true
    let shape t = with_proto t (fun t -> t.shape) |? `Rectangle
    let tile t = match shape t with `Tile gid -> get_tile gid | _ -> None
    let tile_class t = tile t >>= fun tile -> tile.class_
    let class_ t = with_proto t (fun t -> t.class_) >>? fun () -> tile_class t

    let width t =
      match shape t with
      | `Point -> 0.
      | `Polygon pts | `Polyline pts ->
          let xmin, xmax =
            List.fold_left
              (fun (xmin, xmax) (x, _) -> (min xmin x, max xmax x))
              (0., 0.) pts in
          abs_float (xmax -. xmin)
      | _ -> with_proto t (fun t -> t.width) |? 0.

    let height t =
      match shape t with
      | `Point -> 0.
      | `Polygon pts | `Polyline pts ->
          let ymin, ymax =
            List.fold_left
              (fun (ymin, ymax) (_, y) -> (min ymin y, max ymax y))
              (0., 0.) pts in
          abs_float (ymax -. ymin)
      | _ -> with_proto t (fun t -> t.height) |? 0.

    let property_lists t =
      let class_props = class_ t >>= get_class_members ~useas:`Object |? [] in
      let template_props = proto t >|= properties |? [] in
      let tile_props = tile t >|= (fun tile -> tile.properties) |? [] in
      let own_props = properties t in
      [class_props; tile_props; template_props; own_props]

    module P = (val Props.make ~strict:false ~property_lists)

    include (P : Props.S with type t := t)

    let reloc t ~from_ ~to_ =
      let template = t.template >|= Util.Filename.reloc ~from_ ~to_ in
      let properties = List.map (Property.reloc ~from_ ~to_) t.properties in
      {t with template; properties}

    let map_gids f t = {t with shape = t.shape >|= Shape.map_gids f}
  end

  type object_ = Object.t

  module Layer = struct
    module Tilelayer = struct
      type t = Types.tilelayer =
        {width : int; height : int; data : Data.t option}
      [@@deriving eq, ord, show {with_path = false}, make]

      let width t = t.width
      let height t = t.height
      let data t = t.data

      let gid_at ~col ~row t =
        (* TODO: make data non-optional. this will disallow empty tile
           layers *)
        if col >= width t then Util.Error.invalid_arg "col" (string_of_int col) ;
        if row >= height t then
          Util.Error.invalid_arg "row" (string_of_int row) ;
        let i = col + (row * width t) in
        data t |> Option.get |> Data.bytes
        |> Fun.flip Bytes.get_int32_ne (i * 4)
        |> Gid.of_int32

      let map_gids f t =
        (* TODO: It would be better to modify the data in place; we should
           avoid allocate a whole new map in case the map is huge. *)
        match t.data with
        | None -> t
        | Some data0 ->
            let bytes0 = Data.bytes data0 in
            let bytes = Bytes.copy bytes0 in
            for i = 0 to (Bytes.length bytes / 4) - 1 do
              let gid0 = Gid.of_int32 (Bytes.get_int32_ne bytes (i * 4)) in
              let gid = f gid0 in
              Bytes.set_int32_ne bytes (i * 4) (Gid.to_int32 gid)
            done ;
            let encoding = Data.encoding data0 in
            let compression = Data.compression data0 in
            let data = Some (Data.make ?encoding ?compression bytes) in
            {t with data}
    end

    type tilelayer = Tilelayer.t

    module Objectgroup = struct
      module Draworder = struct
        type t = [`Topdown | `Index]
        [@@deriving eq, ord, show {with_path = false}]
      end

      type draworder = Draworder.t

      (* TODO: enforce unique object IDs *)
      type t = Types.objectgroup =
        {draworder : Draworder.t option; objects : Object.t list}
      [@@deriving eq, ord, show {with_path = false}, make]

      let draworder t = t.draworder |? `Topdown
      let objects t = t.objects

      let get_object t id =
        List.find_opt (fun o -> Object.id o = id) (objects t)

      let get_object_exn t id =
        get_object t id >|? fun () -> Util.Error.object_not_found id

      let reloc t ~from_ ~to_ =
        let objects = List.map (Object.reloc ~from_ ~to_) t.objects in
        {t with objects}

      let map_gids f t =
        {t with objects = List.map (Object.map_gids f) t.objects}
    end

    type objectgroup = Objectgroup.t

    module Imagelayer = struct
      type t = Types.imagelayer =
        {image : Image.t option; repeatx : bool option; repeaty : bool option}
      [@@deriving eq, ord, show {with_path = false}, make]

      let image t = t.image
      let repeatx t = t.repeatx |? false
      let repeaty t = t.repeaty |? false

      let reloc t ~from_ ~to_ =
        let image = Option.map (Image.reloc ~from_ ~to_) t.image in
        {t with image}
    end

    type imagelayer = Imagelayer.t

    type t = Types.layer =
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

    type variant =
      [ `Tilelayer of Tilelayer.t
      | `Objectgroup of Objectgroup.t
      | `Imagelayer of Imagelayer.t
      | `Group of t list ]
    [@@deriving eq, ord, show {with_path = false}]

    let rec reloc t ~from_ ~to_ =
      let properties = List.map (Property.reloc ~from_ ~to_) t.properties in
      let variant = reloc_variant t.variant ~from_ ~to_ in
      {t with properties; variant}

    and reloc_variant v ~from_ ~to_ =
      match v with
      | `Tilelayer _ -> v
      | `Objectgroup og -> `Objectgroup (Objectgroup.reloc og ~from_ ~to_)
      | `Imagelayer il -> `Imagelayer (Imagelayer.reloc il ~from_ ~to_)
      | `Group ts -> `Group (List.map (reloc ~from_ ~to_) ts)

    let rec map_gids f t = {t with variant = map_gids_variant f t.variant}

    and map_gids_variant f t =
      match t with
      | `Tilelayer tl -> `Tilelayer (Tilelayer.map_gids f tl)
      | `Objectgroup og -> `Objectgroup (Objectgroup.map_gids f og)
      | `Group ls -> `Group (List.map (map_gids f) ls)
      | _ -> t

    module Variant = struct
      type t = variant [@@deriving eq, ord, show {with_path = false}]

      let reloc t ~from_ ~to_ = reloc_variant t ~from_ ~to_
      let map_gids f t = map_gids_variant f t
    end

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
      get_object t id >|? fun () -> Util.Error.object_not_found id

    module P = (val make_std_props ~class_ ~properties ~useas:`Layer)

    include (P : Props.S with type t := t)
  end

  type layer = Layer.t

  module Tile = struct
    module Frame = struct
      type t = Types.frame = {tileid : int; duration : int}
      [@@deriving eq, ord, show {with_path = false}, make]

      let tileid t = t.tileid
      let duration t = t.duration
    end

    type frame = Frame.t

    type t = Types.tile =
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
    let width t = t.width >>? fun () -> image t >>= Image.width
    let height t = t.height >>? fun () -> image t >>= Image.height

    module P = (val make_std_props ~class_ ~properties ~useas:`Tile)

    include (P : Props.S with type t := t)

    let reloc t ~from_ ~to_ =
      let properties = List.map (Property.reloc ~from_ ~to_) t.properties in
      let image = Option.map (Image.reloc ~from_ ~to_) t.image in
      let objectgroup = List.map (Object.reloc ~from_ ~to_) t.objectgroup in
      {t with properties; image; objectgroup}
  end

  type tile = Tile.t

  module Tileset = struct
    module Int_map = Stdlib.Map.Make (Int)

    module Tileoffset = struct
      type t = Types.tileoffset = {x : int option; y : int option}
      [@@deriving eq, ord, show {with_path = false}, make]

      let x t = t.x |? 0
      let y t = t.y |? 0
    end

    type tileoffset = Tileoffset.t

    module Single = struct
      type t = Types.single =
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

      let reloc t ~from_ ~to_ =
        let image = Image.reloc t.image ~from_ ~to_ in
        {t with image}
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

      let reloc t ~from_ ~to_ =
        match t with
        | `Single s -> `Single (Single.reloc s ~from_ ~to_)
        | _ -> t
    end

    type variant = Variant.t

    type t = Types.tileset =
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
      t.objectalignment >|? fun () ->
      match grid t with `Orthogonal -> `Bottomleft | `Isometric _ -> `Bottom

    let tilecount t =
      match variant t with
      | `Single single -> Single.tilecount single
      | `Collection -> Int_map.cardinal t.tiles

    let max_id t =
      match variant t with
      | `Single _ -> tilecount t - 1
      | `Collection ->
          Int_map.fold (fun id _ max_id -> max id max_id) t.tiles 0

    let get_tile t id : Tile.t option =
      match variant t with
      | `Collection -> Int_map.find_opt id t.tiles
      | `Single single ->
          if id >= tilecount t then None
          else
            let tile =
              Int_map.find_opt id t.tiles >|? fun () -> Tile.make ~id () in
            let image = Some (Single.image single) in
            let x, y, width, height =
              let width = Single.tilewidth single in
              let height = Single.tileheight single in
              let margin = Single.margin single in
              let spacing = Single.spacing single in
              let columns = columns t in
              let col = id mod columns in
              let row = id / columns in
              let x = margin + (col * (width + spacing)) in
              let y = margin + (row * (height + spacing)) in
              (Some x, Some y, Some width, Some height) in
            Some {tile with image; x; y; width; height}

    module P = (val make_std_props ~class_ ~properties ~useas:`Tile)

    include (P : Props.S with type t := t)

    let reloc t ~from_ ~to_ =
      { t with
        properties = List.map (Property.reloc ~from_ ~to_) t.properties;
        tiles = Int_map.map (Tile.reloc ~from_ ~to_) t.tiles;
        variant = Variant.reloc t.variant ~from_ ~to_ }
  end

  type tileset = Tileset.t

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
      type t = Types.staggered =
        {staggeraxis : Staggeraxis.t; staggerindex : Staggerindex.t}
      [@@deriving eq, ord, show {with_path = false}, make]

      let staggeraxis t = t.staggeraxis
      let staggerindex t = t.staggerindex
    end

    type staggered = Staggered.t

    module Hexagonal = struct
      type t = Types.hexagonal =
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

    module Geometry = struct
      type t =
        [ `Orthogonal
        | `Isometric
        | `Staggered of Staggered.t
        | `Hexagonal of Hexagonal.t ]
      [@@deriving eq, ord, show {with_path = false}]
    end

    type geometry = Geometry.t

    (* Invariant : Tilesets in decreasing order of firstgid *)
    type t = Types.map =
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
        geometry : Geometry.t }
    [@@deriving eq, ord, show {with_path = false}]

    let make ~version ?tiledversion ?class_ ?renderorder ?compressionlevel
        ~width ~height ~tilewidth ~tileheight ?parallaxoriginx ?parallaxoriginy
        ?backgroundcolor ?infinite ?(properties = []) ?(tilesets = [])
        ?(layers = []) ~geometry () =
      let layers =
        let cmp l l' = Int.compare (Layer.id l) (Layer.id l') in
        let layers' = List.sort_uniq cmp layers in
        if List.compare_lengths layers layers' = 0 then layers'
        else Util.Error.invalid_arg "layers" "id not unique" in
      let tilesets =
        let tilesets' =
          let cmp (gid, _) (gid', _) = -Int.compare gid gid' in
          List.sort_uniq cmp tilesets in
        if List.compare_lengths tilesets tilesets' = 0 then tilesets'
        else Util.Error.invalid_arg "tilesets" "firstgid not unique" in
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
        geometry }

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
    let geometry t = t.geometry

    let nextlayerid t =
      List.fold_left (fun id l -> max id (Layer.id l + 1)) 0 (layers t)

    let nextobjectid t =
      List.fold_left
        (fun id l ->
          List.fold_left
            (fun id' o -> max id' (Object.id o + 1))
            id (Layer.objects l) )
        0 (layers t)

    let objects t = List.concat_map Layer.objects (layers t)
    let get_object t id = List.find_opt (fun o -> Object.id o = id) (objects t)
    let get_object_exn t id =
      get_object t id >|? fun () -> Util.Error.object_not_found id

    let get_tile_ref t gid =
      let id = Gid.id gid in
      List.find_map
        (fun (firstgid, ts) ->
          if firstgid <= id then Some (firstgid, ts, id - firstgid) else None
          )
        t.tilesets

    module P = (val make_std_props ~class_ ~properties ~useas:`Tile)

    include (P : Props.S with type t := t)

    let reloc t ~from_ ~to_ =
      let properties = List.map (Property.reloc ~from_ ~to_) t.properties in
      let tilesets =
        List.map
          (fun (firstgid, fname) ->
            (firstgid, Util.Filename.reloc fname ~from_ ~to_) )
          t.tilesets in
      let layers = List.map (Layer.reloc ~from_ ~to_) t.layers in
      {t with properties; tilesets; layers}

    let map_gids f t =
      let layers = List.map (Layer.map_gids f) t.layers in
      {t with layers}
  end

  type map = Map.t

  module Template = struct
    type t = Types.template =
      {tileset : (int * string) option; object_ : Object.t}
    [@@deriving eq, ord, show {with_path = false}]

    let make ?tileset object_ =
      match Object.template object_ with
      | Some _ -> Util.Error.nested_template ()
      | None -> {tileset; object_}

    let tileset t = t.tileset
    let object_ t = t.object_

    let reloc t ~from_ ~to_ =
      { tileset =
          Option.map
            (fun (firstgid, fname) ->
              (firstgid, Util.Filename.reloc fname ~from_ ~to_) )
            t.tileset;
        object_ = Object.reloc t.object_ ~from_ ~to_ }

    let map_gids f t = {t with object_ = Object.map_gids f t.object_}
  end

  type template = Template.t

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

    type t = Types.class_ = {useas : Useas.t list; members : Property.t list}
    [@@deriving eq, ord, show {with_path = false}]

    let make ~useas ~members =
      if useas = [] then Util.Error.invalid_arg "useas" "[]" ;
      let members = List.sort_uniq Property.compare members in
      {useas; members}

    let useas t = t.useas
    let members t = t.members

    let reloc t ~from_ ~to_ =
      {t with members = List.map (Property.reloc ~from_ ~to_) t.members}
  end

  type class_ = Class.t

  module Enum = Enum

  type enum = Enum.t

  module Customtype = struct
    module Variant = struct
      type t = [`Class of Class.t | `Enum of Enum.t]
      [@@deriving eq, ord, show {with_path = false}]

      let reloc t ~from_ ~to_ =
        match t with `Class c -> `Class (Class.reloc c ~from_ ~to_) | _ -> t
    end

    type variant = Variant.t

    type t = Types.customtype = {id : int; name : string; variant : Variant.t}
    [@@deriving eq, ord, show {with_path = false}, make]

    let id t = t.id
    let name t = t.name
    let variant t = t.variant

    let reloc t ~from_ ~to_ =
      {t with variant = Variant.reloc t.variant ~from_ ~to_}
  end

  type customtype = Customtype.t
end
