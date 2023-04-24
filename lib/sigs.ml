include Sigs0

module type Getters = sig
  val get_tileset : string -> Types.tileset option
  val get_template : string -> Types.template option
  val get_customtypes : string -> Types.customtype list
  val get_class : string -> useas:Types.useas -> Types.class_ option
  val get_map : string -> Types.map option
  val get_file : string -> string option
  val get_tile : Gid.t -> Types.tile option
end

module type ClassT = sig
  type t

  val class_ : t -> string option
end

module type PropsT = sig
  type t
  type property

  val properties : t -> property list
  val get_property : string -> t -> property option
  val get_property_exn : string -> t -> property
end

module type ClassPropsT = sig
  type t

  include ClassT with type t := t
  include PropsT with type t := t
end

module type GidsT = sig
  include T val map_gids : (Gid.t -> Gid.t) -> t -> t
end

module type Core = sig
  module Error : module type of Error

  type error = Error.t

  module Color : module type of Color

  type color = Color.t

  module Gid : module type of Gid

  type gid = Gid.t

  module Data : module type of Data

  type data = Data.t

  module Image : module type of Image

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

      include StdT with type t := t
    end

    type value = Value.t

    val make :
      name:string -> ?propertytype:string -> value:Value.t -> unit -> t
    val name : t -> string
    val propertytype : t -> string option
    val value : t -> Value.t

    include StdT with type t := t
    include RelocT with type t := t
    include ClassPropsT with type t := t and type property := t
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

      include StdT with type t := t
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

      include StdT with type t := t
      include GidsT with type t := t
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

    include StdT with type t := t
    include RelocT with type t := t
    include ClassPropsT with type t := t and type property := Property.t
    include GidsT with type t := t
  end

  type object_ = Object.t

  module Layer : sig
    type t

    module Tilelayer : sig
      type t

      val make : width:int -> height:int -> ?data:Data.t -> unit -> t
      val width : t -> int
      val height : t -> int
      val data : t -> Data.t option

      val gid_at : col:int -> row:int -> t -> Gid.t

      include StdT with type t := t
      include GidsT with type t := t
    end

    type tilelayer = Tilelayer.t

    module Objectgroup : sig
      module Draworder : sig
        type t = [`Topdown | `Index]

        include StdT with type t := t
      end

      type draworder = Draworder.t

      type t

      val make : ?draworder:draworder -> ?objects:Object.t list -> unit -> t
      val draworder : t -> draworder
      val objects : t -> Object.t list
      val get_object : t -> int -> Object.t option
      val get_object_exn : t -> int -> Object.t

      include StdT with type t := t
      include RelocT with type t := t
      include GidsT with type t := t
    end

    type objectgroup = Objectgroup.t

    module Imagelayer : sig
      type t

      val make : ?image:Image.t -> ?repeatx:bool -> ?repeaty:bool -> unit -> t
      val image : t -> Image.t option
      val repeatx : t -> bool
      val repeaty : t -> bool

      include StdT with type t := t
      include RelocT with type t := t
    end

    type imagelayer = Imagelayer.t

    module Variant : sig
      type layer := t

      type t =
        [ `Tilelayer of Tilelayer.t
        | `Objectgroup of Objectgroup.t
        | `Imagelayer of Imagelayer.t
        | `Group of layer list ]

      include StdT with type t := t
      include RelocT with type t := t
      include GidsT with type t := t
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

    include StdT with type t := t
    include RelocT with type t := t
    include ClassPropsT with type t := t and type property := Property.t
    include GidsT with type t := t
  end

  type layer = Layer.t

  module Tile : sig
    module Frame : sig
      type t

      val make : tileid:int -> duration:int -> t
      val tileid : t -> int
      val duration : t -> int

      include StdT with type t := t
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

    include StdT with type t := t
    include RelocT with type t := t
    include ClassPropsT with type t := t and type property := Property.t
  end

  type tile = Tile.t

  module Tileset : sig
    module Tileoffset : sig
      include StdT

      val make : ?x:int -> ?y:int -> unit -> t
      val x : t -> int
      val y : t -> int
    end

    type tileoffset = Tileoffset.t

    module Single : sig
      type t

      include StdT with type t := t
      include RelocT with type t := t

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

      include StdT with type t := t
    end

    type objectalignment = Objectalignment.t

    module Tilerendersize : sig
      type t = [`Tile | `Grid]

      include StdT with type t := t
    end

    type tilerendersize = Tilerendersize.t

    module Fillmode : sig
      type t = [`Stretch | `Preserve_aspect_fit]

      include StdT with type t := t
    end

    type fillmode = Fillmode.t

    module Grid : sig
      type t = [`Orthogonal | `Isometric of int * int]

      include StdT with type t := t
    end

    type grid = Grid.t

    module Variant : sig
      type t = [`Single of Single.t | `Collection]

      include StdT with type t := t
      include RelocT with type t := t
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

    include StdT with type t := t
    include RelocT with type t := t
    include ClassPropsT with type t := t and type property := Property.t
  end

  type tileset = Tileset.t

  module Map : sig
    module Staggeraxis : sig
      type t = [`X | `Y]

      include StdT with type t := t
    end

    type staggeraxis = Staggeraxis.t

    module Staggerindex : sig
      type t = [`Even | `Odd]

      include StdT with type t := t
    end

    type staggerindex = Staggerindex.t

    module Staggered : sig
      include StdT

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

      include StdT with type t := t
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

    val objects : t -> Object.t list
    val get_object : t -> int -> Object.t option
    val get_object_exn : t -> int -> Object.t
    val get_tile_ref : t -> Gid.t -> (int * string * int) option

    include StdT with type t := t
    include RelocT with type t := t
    include ClassPropsT with type t := t and type property := Property.t
    include GidsT with type t := t
  end

  type map = Map.t

  module Template : sig
    type t

    val make : ?tileset:int * string -> Object.t -> t
    val tileset : t -> (int * string) option
    val object_ : t -> Object.t

    include StdT with type t := t
    include RelocT with type t := t
    include GidsT with type t := t
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

      include StdT with type t := t
    end

    type useas = Useas.t

    val make : useas:Useas.t list -> members:Property.t list -> t
    val useas : t -> Useas.t list
    val members : t -> Property.t list

    include StdT with type t := t
    include RelocT with type t := t
  end

  type class_ = Class.t

  module Enum : module type of Enum

  type enum = Enum.t

  module Customtype : sig
    module Variant : sig
      type t = [`Class of Class.t | `Enum of Enum.t]

      include StdT with type t := t
      include RelocT with type t := t
    end

    type variant = Variant.t

    type t

    val make : id:int -> name:string -> variant:variant -> t
    val id : t -> int
    val name : t -> string
    val variant : t -> Variant.t

    include StdT with type t := t
    include RelocT with type t := t
  end

  type customtype = Customtype.t
end

module type Core_generic =
  Core
    with type Property.t = Types.property
     and type Object.Text.t = Types.text
     and type Object.t = Types.object_
     and type Layer.Tilelayer.t = Types.tilelayer
     and type Layer.Objectgroup.t = Types.objectgroup
     and type Layer.Imagelayer.t = Types.imagelayer
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
     and type Customtype.t = Types.customtype

module type Loader = sig
  include Core

  val tilesets : unit -> (int * string * Tileset.t) list
  val templates : unit -> (string * Template.t) list
  val files : unit -> (string * string) list
  val customtypes : unit -> Customtype.t list
  val maps : unit -> (string * Map.t) list

  val get_tileset : string -> Tileset.t option
  val get_template : string -> Template.t option
  val get_customtypes : string -> Customtype.t list
  val get_class : string -> useas:Class.useas -> Class.t option
  val get_map : string -> Map.t option
  val get_file : string -> string option
  val get_tile : Gid.t -> Tile.t option

  val get_tileset_exn : string -> Tileset.t
  val get_template_exn : string -> Template.t
  val get_class_exn : string -> useas:Class.useas -> Class.t
  val get_map_exn : string -> Map.t
  val get_file_exn : string -> string
  val get_tile_exn : Gid.t -> Tile.t

  val load_tileset_xml : string -> (Tileset.t, Error.t) result
  val load_template_xml : string -> (Template.t, Error.t) result
  val load_customtypes_json : string -> (Customtype.t list, Error.t) result
  val load_file : string -> (string, Error.t) result
  val load_map_xml : string -> (Map.t, Error.t) result

  val load_tileset_xml_exn : string -> Tileset.t
  val load_template_xml_exn : string -> Template.t
  val load_customtypes_json_exn : string -> Customtype.t list
  val load_file_exn : string -> string
  val load_map_xml_exn : string -> Map.t

  val unload_tileset : string -> unit
  val unload_template : string -> unit
  val unload_customtypes : string -> unit
  val unload_class : string -> useas:Class.useas -> unit
  val unload_file : string -> unit
  val unload_map : string -> unit
end
