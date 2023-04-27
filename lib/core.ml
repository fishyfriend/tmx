include Core_intf

module Make (Getters : Getters) = struct
  open Util.Option.Infix
  open Types

  include Getters

  let get_class k ~useas =
    List.find_map
      (fun ct ->
        match ct.variant with
        | `Class c when List.mem useas c.useas -> Some c
        | _ -> None )
      (get_customtypes k)

  let get_class_members name ~useas =
    get_class name ~useas >|= fun c -> c.members

  let normalize_plist ps =
    let cmp (a : property) (b : property) = compare a.name b.name in
    List.sort_uniq cmp ps

  let get_std_plists ~class_ ~properties ~useas x =
    let class_props = class_ x >>= get_class_members ~useas |? [] in
    let own_props = properties x in
    [class_props; own_props]

  let make_std_props ~class_ ~properties ~useas =
    let property_lists x = get_std_plists ~class_ ~properties ~useas x in
    Props.make ~strict:false ~property_lists

  module Int_map = Stdlib.Map.Make (Int)

  module Error = Error

  type error = Error.t

  module Color = Color

  type color = Color.t

  module Gid = Gid

  type gid = Gid.t

  module Property = struct
    type t = property =
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
        | `Class ts -> `Class (normalize_plist ts)
        | _ -> value in
      {name; propertytype; value}

    let name t = t.name
    let propertytype t = t.propertytype
    let value t = t.value

    let class_ t = match value t with `Class _ -> propertytype t | _ -> None
    let properties t = match value t with `Class ts -> ts | _ -> []
    let property_lists t =
      get_std_plists ~class_ ~properties ~useas:`Property t

    module P = (val Props.make ~strict:true ~property_lists)

    include (P : Props.S with type t := t)

    let rec reloc ~from_dir ~to_dir t =
      let value =
        match t.value with
        | `File fname -> `File (Util.Filename.reloc ~from_dir ~to_dir fname)
        | `Class ts -> `Class (List.map (reloc ~from_dir ~to_dir) ts)
        | v -> v in
      {t with value}
  end

  type property = Property.t

  module Data = struct
    module Encoding = struct
      type t = [`Base64 | `Csv] [@@deriving eq, ord, show {with_path = false}]
    end

    type encoding = Encoding.t

    module Compression = struct
      type t = [`Gzip | `Zlib | `Zstd]
      [@@deriving eq, ord, show {with_path = false}]
    end

    type compression = Compression.t

    type t = data =
      { encoding : Encoding.t option;
        compression : Compression.t option;
        bytes : bytes [@opaque] [@main] }
    [@@deriving eq, ord, show {with_path = false}, make]

    let create ?encoding ?compression n =
      make ?encoding ?compression (Bytes.create n)

    let encoding t = t.encoding
    let compression t = t.compression
    let bytes t = t.bytes

    let read_base64 s =
      try Base64.decode_exn s
      with Invalid_argument msg -> Util.Error.base64 msg

    let read_gzip s =
      match Ezgzip.decompress s with
      | Ok s -> s
      | Error (`Gzip e) -> Util.Error.gzip e

    let read_zlib s =
      match Ezgzip.Z.decompress ~header:true s with
      | Ok s -> s
      | Error (`Zlib e) -> Util.Error.zlib e

    let read_zstd _ = Util.Error.zstd ()

    let read_csv s =
      let cells = String.split_on_char ',' s in
      let n_cells = List.length cells in
      let next = List.to_seq cells |> Seq.to_dispenser in
      let bytes = Bytes.create (n_cells * 4) in
      for i = 0 to n_cells - 1 do
        let gid = next () |> Option.get |> String.trim |> Int32.of_string in
        Bytes.set_int32_ne bytes (i * 4) gid
      done ;
      String.of_bytes bytes

    let of_string ?encoding ?compression s =
      let s =
        match (encoding, compression) with
        | None, None -> s
        | None, Some `Gzip -> read_gzip s
        | None, Some `Zlib -> read_zlib s
        | None, Some `Zstd -> read_zstd s
        | Some `Base64, None -> read_base64 s
        | Some `Base64, Some `Gzip -> read_base64 s |> read_gzip
        | Some `Base64, Some `Zlib -> read_base64 s |> read_zlib
        | Some `Base64, Some `Zstd -> read_base64 s |> read_zstd
        | Some `Csv, None -> read_csv s
        | Some `Csv, Some c ->
            Util.Error.invalid_arg "compression" (Compression.show c) in
      let bytes = Bytes.unsafe_of_string s in
      make ?encoding ?compression bytes

    let of_int32_list xs =
      let n = List.length xs in
      let next = List.to_seq xs |> Seq.to_dispenser in
      let bytes = Bytes.create (n * 4) in
      for i = 0 to n - 1 do
        let gid = next () |> Option.get in
        Bytes.set_int32_ne bytes (i * 4) gid
      done ;
      make bytes
  end

  type data = Data.t

  module Image = struct
    module Format = struct
      type t = [`Bmp | `Gif | `Jpg | `Png]
      [@@deriving eq, ord, show {with_path = false}]
    end

    type format = Format.t

    module Source = struct
      type t = [`File of string | `Embed of Format.t * Data.t]
      [@@deriving eq, ord, show {with_path = false}]

      let reloc ~from_dir ~to_dir t =
        match t with
        | `File fname -> `File (Util.Filename.reloc ~from_dir ~to_dir fname)
        | _ -> t
    end

    type source = Source.t

    type t = image =
      { source : Source.t;
        trans : Color.t option;
        width : int option;
        height : int option }
    [@@deriving eq, ord, show {with_path = false}, make]

    let source t = t.source
    let trans t = t.trans
    let width t = t.width
    let height t = t.height

    let reloc ~from_dir ~to_dir t =
      {t with source = Source.reloc ~from_dir ~to_dir t.source}
  end

  type image = Image.t

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

      type t = text =
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

    type t = object_ =
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
    [@@deriving eq, make, ord, show {with_path = false}]

    let make ?id ?name ?class_ ?x ?y ?width ?height ?rotation ?visible
        ?template ?properties ?shape () =
      let properties = properties >|= normalize_plist in
      make ?id ?name ?class_ ?x ?y ?width ?height ?rotation ?visible ?template
        ?properties ?shape ()

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

    let reloc ~from_dir ~to_dir t =
      let template = t.template >|= Util.Filename.reloc ~from_dir ~to_dir in
      let properties =
        List.map (Property.reloc ~from_dir ~to_dir) t.properties in
      {t with template; properties}

    let map_gids f t = {t with shape = t.shape >|= Shape.map_gids f}
  end

  type object_ = Object.t

  module Layer = struct
    module Tilelayer = struct
      type t = tilelayer = {width : int; height : int; data : Data.t [@main]}
      [@@deriving eq, ord, show {with_path = false}, make]

      let width t = t.width
      let height t = t.height
      let data t = t.data

      let gid_at ~col ~row t =
        if col >= width t then Util.Error.invalid_arg "col" (string_of_int col) ;
        if row >= height t then
          Util.Error.invalid_arg "row" (string_of_int row) ;
        let i = col + (row * width t) in
        data t |> Data.bytes
        |> Fun.flip Bytes.get_int32_ne (i * 4)
        |> Gid.of_int32

      let map_gids_inplace f t : unit =
        let bytes = Data.bytes t.data in
        for i = 0 to (Bytes.length bytes / 4) - 1 do
          Bytes.get_int32_ne bytes (i * 4)
          |> Gid.of_int32 |> f |> Gid.to_int32
          |> Bytes.set_int32_ne bytes (i * 4)
        done
    end

    type tilelayer = Tilelayer.t

    module Objectgroup = struct
      module Draworder = struct
        type t = [`Topdown | `Index]
        [@@deriving eq, ord, show {with_path = false}]
      end

      type draworder = Draworder.t

      type t = objectgroup =
        {draworder : Draworder.t option; objects : Object.t Int_map.t [@opaque]}
      [@@deriving eq, ord, show {with_path = false}]

      let make ?draworder ?(objects = []) () =
        let objects =
          List.fold_left
            (fun objects o -> Int_map.add (Object.id o) o objects)
            Int_map.empty objects in
        {draworder; objects}

      let draworder t = t.draworder |? `Topdown
      let objects t = Int_map.to_seq t.objects |> Seq.map snd |> List.of_seq

      let get_object t id = Int_map.find_opt id t.objects

      let get_object_exn t id =
        get_object t id >|? fun () -> Util.Error.object_not_found id

      let reloc ~from_dir ~to_dir t =
        { t with
          objects = Int_map.map (Object.reloc ~from_dir ~to_dir) t.objects }

      let map_gids f t =
        {t with objects = Int_map.map (Object.map_gids f) t.objects}
    end

    type objectgroup = Objectgroup.t

    module Imagelayer = struct
      type t = imagelayer =
        {image : Image.t option; repeatx : bool option; repeaty : bool option}
      [@@deriving eq, ord, show {with_path = false}, make]

      let image t = t.image
      let repeatx t = t.repeatx |? false
      let repeaty t = t.repeaty |? false

      let reloc ~from_dir ~to_dir t =
        {t with image = t.image >|= Image.reloc ~from_dir ~to_dir}
    end

    type imagelayer = Imagelayer.t

    type t = layer =
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

    let make ?id ?name ?class_ ?opacity ?visible ?tintcolor ?offsetx ?offsety
        ?parallaxx ?parallaxy ?properties ~variant () =
      let properties = properties >|= normalize_plist in
      make ?id ?name ?class_ ?opacity ?visible ?tintcolor ?offsetx ?offsety
        ?parallaxx ?parallaxy ?properties ~variant ()

    type variant =
      [ `Tilelayer of Tilelayer.t
      | `Objectgroup of Objectgroup.t
      | `Imagelayer of Imagelayer.t
      | `Group of t list ]
    [@@deriving eq, ord, show {with_path = false}]

    let rec reloc ~from_dir ~to_dir t =
      let properties =
        List.map (Property.reloc ~from_dir ~to_dir) t.properties in
      let variant = reloc_variant ~from_dir ~to_dir t.variant in
      {t with properties; variant}

    and reloc_variant ~from_dir ~to_dir v =
      match v with
      | `Tilelayer _ -> v
      | `Objectgroup og ->
          `Objectgroup (Objectgroup.reloc ~from_dir ~to_dir og)
      | `Imagelayer il -> `Imagelayer (Imagelayer.reloc ~from_dir ~to_dir il)
      | `Group ts -> `Group (List.map (reloc ~from_dir ~to_dir) ts)

    let rec map_gids f t = {t with variant = map_gids_variant f t.variant}

    and map_gids_variant f t =
      match t with
      | `Tilelayer tl ->
          Tilelayer.map_gids_inplace f tl ;
          `Tilelayer tl
      | `Objectgroup og -> `Objectgroup (Objectgroup.map_gids f og)
      | `Group ls -> `Group (List.map (map_gids f) ls)
      | _ -> t

    module Variant = struct
      type t = variant [@@deriving eq, ord, show {with_path = false}]
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
      type t = frame = {tileid : int; duration : int}
      [@@deriving eq, ord, show {with_path = false}, make]

      let tileid t = t.tileid
      let duration t = t.duration
    end

    type frame = Frame.t

    type t = tile =
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

    let make ~id ?class_ ?x ?y ?width ?height ?properties ?image ?objectgroup
        ?animation () =
      let properties = properties >|= normalize_plist in
      make ~id ?class_ ?x ?y ?width ?height ?properties ?image ?objectgroup
        ?animation ()

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

    let reloc ~from_dir ~to_dir t =
      let properties =
        List.map (Property.reloc ~from_dir ~to_dir) t.properties in
      let image = t.image >|= Image.reloc ~from_dir ~to_dir in
      let objectgroup =
        List.map (Object.reloc ~from_dir ~to_dir) t.objectgroup in
      {t with properties; image; objectgroup}
  end

  type tile = Tile.t

  module Tileset = struct
    module Tileoffset = struct
      type t = tileoffset = {x : int option; y : int option}
      [@@deriving eq, ord, show {with_path = false}, make]

      let x t = t.x |? 0
      let y t = t.y |? 0
    end

    type tileoffset = Tileoffset.t

    module Single = struct
      type t = single =
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

      let reloc ~from_dir ~to_dir t =
        {t with image = Image.reloc ~from_dir ~to_dir t.image}
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

      let reloc ~from_dir ~to_dir t =
        match t with
        | `Single s -> `Single (Single.reloc ~from_dir ~to_dir s)
        | _ -> t
    end

    type variant = Variant.t

    type t = tileset =
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
    [@@deriving eq, ord, show {with_path = false}, make]

    let make ~name ?class_ ~columns ?objectalignment ?tilerendersize ?fillmode
        ?tileoffset ?grid ?properties ~variant tiles =
      let properties = properties >|= normalize_plist in
      let tiles =
        match variant with
        | `Collection ->
            List.fold_left
              (fun tiles tile -> Int_map.add (Tile.id tile) tile tiles)
              Int_map.empty tiles
        | `Single single ->
            let setup_tile tile : tile =
              let image = Some (Single.image single) in
              let x, y, width, height =
                let width = Single.tilewidth single in
                let height = Single.tileheight single in
                let margin = Single.margin single in
                let spacing = Single.spacing single in
                let col = Tile.id tile mod columns in
                let row = Tile.id tile / columns in
                let x = margin + (col * (width + spacing)) in
                let y = margin + (row * (height + spacing)) in
                (Some x, Some y, Some width, Some height) in
              {tile with image; x; y; width; height} in
            let ensure_tile (tiles : tile Int_map.t as 'a) id : 'a =
              Int_map.update id
                (fun tile -> Some (tile >|? Tile.make ~id |> setup_tile))
                tiles in
            let tilecount = Single.tilecount single in
            let tiles =
              List.fold_left
                (fun tiles tile ->
                  let id = Tile.id tile in
                  if id < tilecount then Int_map.add id tile tiles else tiles
                  )
                Int_map.empty tiles in
            let id_seq = Seq.init tilecount Fun.id in
            Seq.fold_left ensure_tile tiles id_seq in
      make ~name ?class_ ~columns ?objectalignment ?tilerendersize ?fillmode
        ?tileoffset ?grid ?properties ~variant tiles

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

    let get_tile t id = Int_map.find_opt id t.tiles

    let get_tile_exn t id =
      get_tile t id >|? fun () ->
      Util.Error.not_found "tile" (string_of_int id)

    module P = (val make_std_props ~class_ ~properties ~useas:`Tile)

    include (P : Props.S with type t := t)

    let reloc ~from_dir ~to_dir t =
      { t with
        properties = List.map (Property.reloc ~from_dir ~to_dir) t.properties;
        tiles = Int_map.map (Tile.reloc ~from_dir ~to_dir) t.tiles;
        variant = Variant.reloc ~from_dir ~to_dir t.variant }
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
      type t = staggered =
        {staggeraxis : Staggeraxis.t; staggerindex : Staggerindex.t}
      [@@deriving eq, ord, show {with_path = false}, make]

      let staggeraxis t = t.staggeraxis
      let staggerindex t = t.staggerindex
    end

    type staggered = Staggered.t

    module Hexagonal = struct
      type t = hexagonal =
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

    type t = map =
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
    [@@deriving eq, ord, show {with_path = false}, make]

    let make ~version ?tiledversion ?class_ ?renderorder ?compressionlevel
        ~width ~height ~tilewidth ~tileheight ?parallaxoriginx ?parallaxoriginy
        ?backgroundcolor ?infinite ?properties ?(tilesets = []) ?(layers = [])
        ~geometry () =
      let properties = properties >|= normalize_plist in
      let tilesets =
        (* Sort by firstgid descending! *)
        let tilesets' =
          let cmp (a, _) (b, _) = Int.compare b a in
          List.sort_uniq cmp tilesets in
        if List.compare_lengths tilesets tilesets' = 0 then tilesets'
        else Util.Error.invalid_arg "tilesets" "firstgid not unique" in
      let layers =
        let cmp l l' = Int.compare (Layer.id l) (Layer.id l') in
        let layers' = List.sort_uniq cmp layers in
        if List.compare_lengths layers layers' = 0 then layers'
        else Util.Error.invalid_arg "layers" "id not unique" in
      make ~version ?tiledversion ?class_ ?renderorder ?compressionlevel ~width
        ~height ~tilewidth ~tileheight ?parallaxoriginx ?parallaxoriginy
        ?backgroundcolor ?infinite ?properties ~tilesets ~layers ~geometry ()

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

    let get_layer t id =
      let rec aux ls =
        List.find_map
          (fun l ->
            if Layer.id l = id then Some l
            else match Layer.variant l with `Group ls -> aux ls | _ -> None )
          ls in
      aux (layers t)

    let get_layer_exn t id =
      get_layer t id >|? fun () ->
      Util.Error.not_found "layer" (string_of_int id)

    let objects t = List.concat_map Layer.objects (layers t)
    let get_object t id = List.find_opt (fun o -> Object.id o = id) (objects t)
    let get_object_exn t id =
      get_object t id >|? fun () -> Util.Error.object_not_found id

    module P = (val make_std_props ~class_ ~properties ~useas:`Tile)

    include (P : Props.S with type t := t)

    let reloc ~from_dir ~to_dir t =
      let properties =
        List.map (Property.reloc ~from_dir ~to_dir) t.properties in
      let tilesets =
        List.map
          (fun (firstgid, fname) ->
            (firstgid, Util.Filename.reloc ~from_dir ~to_dir fname) )
          t.tilesets in
      let layers = List.map (Layer.reloc ~from_dir ~to_dir) t.layers in
      {t with properties; tilesets; layers}

    let map_gids f t =
      let layers = List.map (Layer.map_gids f) t.layers in
      {t with layers}
  end

  type map = Map.t

  module Template = struct
    type t = template = {tileset : (int * string) option; object_ : Object.t}
    [@@deriving eq, ord, show {with_path = false}]

    let make ?tileset object_ =
      match Object.template object_ with
      | Some _ -> Util.Error.nested_template ()
      | None -> {tileset; object_}

    let tileset t = t.tileset
    let object_ t = t.object_

    let reloc ~from_dir ~to_dir t =
      { tileset =
          ( t.tileset >|= fun (firstgid, fname) ->
            (firstgid, Util.Filename.reloc ~from_dir ~to_dir fname) );
        object_ = Object.reloc ~from_dir ~to_dir t.object_ }

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

    (* Invariant: members sorted/unique by name *)
    type t = class_ = {useas : Useas.t list; members : Property.t list}
    [@@deriving eq, ord, show {with_path = false}]

    let make ~useas ~members =
      if useas = [] then Util.Error.invalid_arg "useas" "[]" ;
      let members = normalize_plist members in
      {useas; members}

    let useas t = t.useas
    let members t = t.members

    let reloc ~from_dir ~to_dir t =
      {t with members = List.map (Property.reloc ~from_dir ~to_dir) t.members}
  end

  type class_ = Class.t

  module Enum = struct
    module Storagetype = struct
      type t = [`Int | `String] [@@deriving eq, ord, show {with_path = false}]
    end

    type storagetype = Storagetype.t

    type t = enum =
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

  type enum = Enum.t

  module Customtype = struct
    module Variant = struct
      type t = [`Class of Class.t | `Enum of Enum.t]
      [@@deriving eq, ord, show {with_path = false}]

      let reloc ~from_dir ~to_dir t =
        match t with
        | `Class c -> `Class (Class.reloc ~from_dir ~to_dir c)
        | _ -> t
    end

    type variant = Variant.t

    type t = customtype = {id : int; name : string; variant : Variant.t}
    [@@deriving eq, ord, show {with_path = false}, make]

    let id t = t.id
    let name t = t.name
    let variant t = t.variant

    let reloc ~from_dir ~to_dir t =
      {t with variant = Variant.reloc ~from_dir ~to_dir t.variant}
  end

  type customtype = Customtype.t

  let reloc_tileset = Tileset.reloc
  let reloc_map = Map.reloc
  let reloc_template = Template.reloc
  let reloc_customtype = Customtype.reloc

  let map_map_gids = Map.map_gids
  let template_map_gids = Template.map_gids
end

module Simple = Make (struct
  let get_tileset _ = None
  let get_template _ = None
  let get_customtypes _ = []
  let get_map _ = None
  let get_file _ = None
  let get_tile _ = None
end)

module Aux = Simple
