module type Getters = sig
  val get_tileset : string -> Types.tileset option
  val get_template : string -> Types.template option
  val get_customtypes : string -> Types.customtype list
  val get_map : string -> Types.map option
  val get_file : string -> string option
  val get_tile : Gid.t -> Types.tile option
end

module type Aux = sig
  open Types

  val reloc_tileset : from_dir:string -> to_dir:string -> tileset -> tileset
  val reloc_map : from_dir:string -> to_dir:string -> map -> map
  val reloc_template : from_dir:string -> to_dir:string -> template -> template
  val reloc_customtype :
    from_dir:string -> to_dir:string -> customtype -> customtype

  val map_map_gids : (Gid.t -> Gid.t) -> map -> map
  val template_map_gids : (Gid.t -> Gid.t) -> template -> template
end

module type S = sig
  module Error : module type of Error

  type error = Error.t

  module Color : module type of Color

  type color = Color.t

  module Gid : module type of Gid

  type gid = Gid.t

  module Data : sig
    module Encoding : sig
      type t = [`Base64 | `Csv]

      include Sigs.StdT with type t := t
    end

    type encoding = Encoding.t

    module Compression : sig
      type t = [`Gzip | `Zlib | `Zstd]

      include Sigs.StdT with type t := t
    end

    type compression = Compression.t

    type t

    val make : ?encoding:encoding -> ?compression:compression -> bytes -> t
    val create : ?encoding:encoding -> ?compression:compression -> int -> t
    val encoding : t -> encoding option
    val compression : t -> compression option

    (** Access the raw mutable binary data. This allows modifying a [t] in
        place. {b Use with caution.} *)
    val bytes : t -> bytes

    val of_string :
      ?encoding:encoding -> ?compression:compression -> string -> t
    val of_int32_list : int32 list -> t

    include Sigs.StdT with type t := t
  end

  type data = Data.t

  module Image : sig
    module Format : sig
      type t = [`Bmp | `Gif | `Jpg | `Png]

      include Sigs.StdT with type t := t
    end

    type format = Format.t

    module Source : sig
      type t = [`File of string | `Embed of Format.t * Data.t]

      include Sigs.StdT with type t := t
    end

    type source = Source.t

    type t

    val make :
      source:Source.t ->
      ?trans:Color.t ->
      ?width:int ->
      ?height:int ->
      unit ->
      t

    val source : t -> Source.t
    val trans : t -> Color.t option
    val width : t -> int option
    val height : t -> int option

    include Sigs.StdT with type t := t
  end

  type image = Image.t

  module Property : sig
    type t

    module Value : sig
      type property := t

      type t =
        [ `String of string
        | `Int of int
        | `Float of float
        | `Bool of bool
        | `Color of Color.t
        | `File of string
        | `Object of int
        | `Class of property list ]

      include Sigs.StdT with type t := t
    end

    type value = Value.t

    val make :
      name:string -> ?propertytype:string -> value:Value.t -> unit -> t
    val name : t -> string
    val propertytype : t -> string option
    val value : t -> Value.t

    include Sigs.StdT with type t := t
    include Sigs.ClassPropsT with type t := t and type property := t
  end

  type property = Property.t

  module Object : sig
    module Text : sig
      module Halign : sig
        type t = [`Center | `Justify | `Left | `Right]
      end

      type halign = Halign.t

      module Valign : sig
        type t = [`Bottom | `Center | `Top]
      end

      type valign = Valign.t

      type t

      val make :
        ?fontfamily:string ->
        ?pixelsize:int ->
        ?wrap:bool ->
        ?color:Color.t ->
        ?bold:bool ->
        ?italic:bool ->
        ?underline:bool ->
        ?strikeout:bool ->
        ?kerning:bool ->
        ?halign:halign ->
        ?valign:valign ->
        string ->
        t

      val text : t -> string
      val fontfamily : t -> string
      val pixelsize : t -> int
      val wrap : t -> bool
      val color : t -> Color.t
      val bold : t -> bool
      val italic : t -> bool
      val underline : t -> bool
      val strikeout : t -> bool
      val kerning : t -> bool
      val halign : t -> halign
      val valign : t -> valign

      include Sigs.StdT with type t := t
    end

    type text = Text.t

    module Shape : sig
      type t =
        [ `Rectangle
        | `Ellipse
        | `Point
        | `Polygon of (float * float) list
        | `Polyline of (float * float) list
        | `Text of Text.t
        | `Tile of Gid.t ]

      include Sigs.StdT with type t := t
    end

    type shape = Shape.t

    type t

    val make :
      ?id:int ->
      ?name:string ->
      ?class_:string ->
      ?x:float ->
      ?y:float ->
      ?width:float ->
      ?height:float ->
      ?rotation:float ->
      ?visible:bool ->
      ?template:string ->
      ?properties:Property.t list ->
      ?shape:shape ->
      unit ->
      t

    val id : t -> int
    val name : t -> string option
    val x : t -> float
    val y : t -> float
    val rotation : t -> float
    val visible : t -> bool
    val template : t -> string option
    val shape : t -> shape
    val width : t -> float
    val height : t -> float

    include Sigs.StdT with type t := t
    include Sigs.ClassPropsT with type t := t and type property := Property.t
  end

  type object_ = Object.t

  module Tile : sig
    module Frame : sig
      type t

      val make : tileid:int -> duration:int -> t
      val tileid : t -> int
      val duration : t -> int

      include Sigs.StdT with type t := t
    end

    type frame = Frame.t

    type t

    val make :
      id:int ->
      ?class_:string ->
      ?x:int ->
      ?y:int ->
      ?width:int ->
      ?height:int ->
      ?properties:Property.t list ->
      ?image:Image.t ->
      ?objectgroup:Object.t list ->
      ?animation:frame list ->
      unit ->
      t

    val id : t -> int
    val x : t -> int
    val y : t -> int
    val width : t -> int option
    val height : t -> int option
    val image : t -> Image.t option
    val objectgroup : t -> Object.t list
    val animation : t -> Frame.t list

    include Sigs.StdT with type t := t
    include Sigs.ClassPropsT with type t := t and type property := Property.t
  end

  type tile = Tile.t

  module Tilelayer : sig
    type t

    val make : width:int -> height:int -> Data.t -> t
    val width : t -> int
    val height : t -> int
    val data : t -> Data.t

    val gid_at : col:int -> row:int -> t -> Gid.t
    val tile_at : col:int -> row:int -> t -> Tile.t option

    include Sigs.StdT with type t := t
  end

  type tilelayer = Tilelayer.t

  module Objectgroup : sig
    module Draworder : sig
      type t = [`Topdown | `Index]

      include Sigs.StdT with type t := t
    end

    type draworder = Draworder.t

    type t

    val make : ?draworder:draworder -> ?objects:Object.t list -> unit -> t
    val draworder : t -> draworder
    val objects : t -> Object.t list
    val get_object : t -> int -> Object.t option
    val get_object_exn : t -> int -> Object.t

    include Sigs.StdT with type t := t
  end

  type objectgroup = Objectgroup.t

  module Imagelayer : sig
    type t

    val make : ?image:Image.t -> ?repeatx:bool -> ?repeaty:bool -> unit -> t
    val image : t -> Image.t option
    val repeatx : t -> bool
    val repeaty : t -> bool

    include Sigs.StdT with type t := t
  end

  type imagelayer = Imagelayer.t

  module Layer : sig
    type t

    module Variant : sig
      type layer := t

      type t =
        [ `Tilelayer of Tilelayer.t
        | `Objectgroup of Objectgroup.t
        | `Imagelayer of Imagelayer.t
        | `Group of layer list ]

      include Sigs.StdT with type t := t
    end

    type variant = Variant.t

    val make :
      ?id:int ->
      ?name:string ->
      ?class_:string ->
      ?opacity:float ->
      ?visible:bool ->
      ?tintcolor:Color.t ->
      ?offsetx:float ->
      ?offsety:float ->
      ?parallaxx:float ->
      ?parallaxy:float ->
      ?properties:Property.t list ->
      variant:variant ->
      unit ->
      t

    val id : t -> int
    val name : t -> string
    val opacity : t -> float
    val visible : t -> bool
    val tintcolor : t -> Color.t option
    val offsetx : t -> float
    val offsety : t -> float
    val parallaxx : t -> float
    val parallaxy : t -> float
    val variant : t -> variant

    val objects : t -> Object.t list
    val get_object : t -> int -> Object.t option
    val get_object_exn : t -> int -> Object.t

    include Sigs.StdT with type t := t
    include Sigs.ClassPropsT with type t := t and type property := Property.t
  end

  type layer = Layer.t

  module Tileset : sig
    module Tileoffset : sig
      include Sigs.StdT

      val make : ?x:int -> ?y:int -> unit -> t
      val x : t -> int
      val y : t -> int
    end

    type tileoffset = Tileoffset.t

    module Single : sig
      type t

      include Sigs.StdT with type t := t

      val make :
        tilecount:int ->
        tilewidth:int ->
        tileheight:int ->
        ?spacing:int ->
        ?margin:int ->
        Image.t ->
        t

      val tilecount : t -> int
      val tilewidth : t -> int
      val tileheight : t -> int
      val spacing : t -> int
      val margin : t -> int
      val image : t -> Image.t
    end

    type single = Single.t

    module Objectalignment : sig
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

      include Sigs.StdT with type t := t
    end

    type objectalignment = Objectalignment.t

    module Tilerendersize : sig
      type t = [`Tile | `Grid]

      include Sigs.StdT with type t := t
    end

    type tilerendersize = Tilerendersize.t

    module Fillmode : sig
      type t = [`Stretch | `Preserve_aspect_fit]

      include Sigs.StdT with type t := t
    end

    type fillmode = Fillmode.t

    module Grid : sig
      type t = [`Orthogonal | `Isometric of int * int]

      include Sigs.StdT with type t := t
    end

    type grid = Grid.t

    module Variant : sig
      type t = [`Single of Single.t | `Collection]

      include Sigs.StdT with type t := t
    end

    type variant = Variant.t

    type t

    val make :
      name:string ->
      ?class_:string ->
      columns:int ->
      ?objectalignment:Objectalignment.t ->
      ?tilerendersize:Tilerendersize.t ->
      ?fillmode:Fillmode.t ->
      ?tileoffset:Tileoffset.t ->
      ?grid:Grid.t ->
      ?properties:Property.t list ->
      variant:variant ->
      Tile.t list ->
      t

    val name : t -> string
    val tilecount : t -> int
    val columns : t -> int
    val objectalignment : t -> Objectalignment.t
    val tilerendersize : t -> Tilerendersize.t
    val fillmode : t -> Fillmode.t
    val tileoffset : t -> Tileoffset.t option
    val grid : t -> Grid.t
    val variant : t -> variant
    val tiles : t -> Tile.t list
    val max_id : t -> int
    val get_tile : t -> int -> Tile.t option
    val get_tile_exn : t -> int -> Tile.t

    include Sigs.StdT with type t := t
    include Sigs.ClassPropsT with type t := t and type property := Property.t
  end

  type tileset = Tileset.t

  module Map : sig
    module Staggeraxis : sig
      type t = [`X | `Y]

      include Sigs.StdT with type t := t
    end

    type staggeraxis = Staggeraxis.t

    module Staggerindex : sig
      type t = [`Even | `Odd]

      include Sigs.StdT with type t := t
    end

    type staggerindex = Staggerindex.t

    module Staggered : sig
      include Sigs.StdT

      val make : staggeraxis:staggeraxis -> staggerindex:staggerindex -> t
      val staggeraxis : t -> Staggeraxis.t
      val staggerindex : t -> Staggerindex.t
    end

    type staggered = Staggered.t

    module Hexagonal : sig
      type t =
        { hexsidelength : int;
          staggeraxis : Staggeraxis.t;
          staggerindex : Staggerindex.t }

      val make :
        hexsidelength:int ->
        staggeraxis:staggeraxis ->
        staggerindex:staggerindex ->
        t
      val hexsidelength : t -> int
      val staggeraxis : t -> Staggeraxis.t
      val staggerindex : t -> Staggerindex.t
    end

    type hexagonal = Hexagonal.t

    module Renderorder : sig
      type t = [`Left_down | `Left_up | `Right_down | `Right_up]

      include Sigs.StdT with type t := t
    end

    type renderorder = Renderorder.t

    module Geometry : sig
      type t =
        [ `Hexagonal of Hexagonal.t
        | `Isometric
        | `Orthogonal
        | `Staggered of Staggered.t ]
    end

    type geometry = Geometry.t

    type t

    val make :
      version:string ->
      ?tiledversion:string ->
      ?class_:string ->
      ?renderorder:renderorder ->
      ?compressionlevel:int ->
      width:int ->
      height:int ->
      tilewidth:int ->
      tileheight:int ->
      ?parallaxoriginx:int ->
      ?parallaxoriginy:int ->
      ?backgroundcolor:Color.t ->
      ?infinite:bool ->
      ?properties:Property.t list ->
      ?tilesets:(int * string) list ->
      ?layers:Layer.t list ->
      geometry:geometry ->
      unit ->
      t

    val version : t -> string
    val tiledversion : t -> string option
    val renderorder : t -> Renderorder.t
    val compressionlevel : t -> int
    val width : t -> int
    val height : t -> int
    val tilewidth : t -> int
    val tileheight : t -> int
    val parallaxoriginx : t -> int
    val parallaxoriginy : t -> int
    val backgroundcolor : t -> Color.t
    val infinite : t -> bool
    val tilesets : t -> (int * string) list
    val layers : t -> Layer.t list
    val geometry : t -> geometry
    val nextlayerid : t -> int
    val nextobjectid : t -> int

    val get_layer : t -> int -> Layer.t option
    val get_layer_exn : t -> int -> Layer.t

    val objects : t -> Object.t list
    val get_object : t -> int -> Object.t option
    val get_object_exn : t -> int -> Object.t

    include Sigs.StdT with type t := t
    include Sigs.ClassPropsT with type t := t and type property := Property.t
  end

  type map = Map.t

  module Template : sig
    type t

    val make : ?tileset:int * string -> Object.t -> t
    val tileset : t -> (int * string) option
    val object_ : t -> Object.t

    include Sigs.StdT with type t := t
  end

  type template = Template.t

  module Class : sig
    type t

    module Useas : sig
      type t =
        [ `Property
        | `Map
        | `Layer
        | `Object
        | `Tile
        | `Tileset
        | `Wangcolor
        | `Wangset ]

      include Sigs.StdT with type t := t
    end

    type useas = Useas.t

    val make : useas:Useas.t list -> members:Property.t list -> t
    val useas : t -> Useas.t list
    val members : t -> Property.t list

    include Sigs.StdT with type t := t
  end

  type class_ = Class.t

  module Enum : sig
    module Storagetype : sig
      type t = [`Int | `String]

      include Sigs.StdT with type t := t
    end

    type storagetype = Storagetype.t

    type t

    val make :
      storagetype:storagetype -> valuesasflags:bool -> string list -> t
    val storagetype : t -> Storagetype.t
    val valuesasflags : t -> bool
    val values : t -> string list

    val read_as_string : t -> Property.Value.t -> string option
    val read_as_int : t -> Property.Value.t -> int option
    val read_as_alist : t -> Property.Value.t -> (string * bool) list option
    val read_as_string_exn : t -> Property.Value.t -> string
    val read_as_int_exn : t -> Property.Value.t -> int
    val read_as_alist_exn : t -> Property.Value.t -> (string * bool) list

    include Sigs.StdT with type t := t
  end

  type enum = Enum.t

  module Customtype : sig
    module Variant : sig
      type t = [`Class of Class.t | `Enum of Enum.t]

      include Sigs.StdT with type t := t
    end

    type variant = Variant.t

    type t

    val make : id:int -> name:string -> variant:variant -> t
    val id : t -> int
    val name : t -> string
    val variant : t -> Variant.t

    include Sigs.StdT with type t := t
  end

  type customtype = Customtype.t

  include Getters

  val get_class : string -> useas:Class.useas -> class_ option
  val get_enum : string -> enum option
end

module type S_generic =
  S
    with type Property.t = Types.property
     and type Data.t = Types.data
     and type Image.t = Types.image
     and type Object.Text.t = Types.text
     and type Object.t = Types.object_
     and type Tilelayer.t = Types.tilelayer
     and type Objectgroup.t = Types.objectgroup
     and type Imagelayer.t = Types.imagelayer
     and type Layer.t = Types.layer
     and type Tile.Frame.t = Types.frame
     and type Tile.t = Types.tile
     and type Tileset.Tileoffset.t = Types.tileoffset
     and type Tileset.Single.t = Types.single
     and type Tileset.t = Types.tileset
     and type Map.Staggered.t = Types.staggered
     and type Map.Hexagonal.t = Types.hexagonal
     and type Map.t = Types.map
     and type Template.t = Types.template
     and type Class.t = Types.class_
     and type Enum.t = Types.enum
     and type Customtype.t = Types.customtype

module type Intf = sig
  module type Getters = Getters
  module type S = S
  module type S_generic = S_generic

  module Make (_ : Getters) : S_generic
  module Simple : S_generic
  module Aux : Aux
end
