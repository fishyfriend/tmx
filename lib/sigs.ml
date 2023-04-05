module type Properties0 = sig
  type property
  type t

  val properties : t -> property list
  val get_property : string -> t -> property option
  val get_property_exn : string -> t -> property
end

module type S = sig
  module Property : sig
    type t [@@deriving eq, ord, show]

    module Value : sig
      type nonrec t =
        [ `String of string
        | `Int of int
        | `Float of float
        | `Bool of bool
        | `Color of Color.t
        | `File of string
        | `Object of int
        | `Class of t list ]
      [@@deriving eq, ord, show]
    end

    type value = Value.t

    val make :
      name:string -> ?propertytype:string -> value:Value.t -> unit -> t
    val name : t -> string
    val propertytype : t -> string option
    val value : t -> Value.t

    include Properties0 with type property := t and type t := t
  end

  module type Properties = Properties0 with type property := Property.t

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

      type t [@@deriving eq, ord, show]

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
      [@@deriving eq, ord, show]
    end

    type shape = Shape.t

    type t [@@deriving eq, ord, show]

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
    val class_ : t -> string option
    val x : t -> float
    val y : t -> float
    val rotation : t -> float
    val visible : t -> bool
    val template : t -> string option
    val shape : t -> shape
    val width : t -> float
    val height : t -> float

    val set_shape : t -> shape option -> t

    include Properties with type t := t
  end

  module Layer : sig
    type t [@@deriving eq, ord, show]

    module Tilelayer : sig
      type t [@@deriving eq, ord, show]

      val make : width:int -> height:int -> ?data:Data.t -> unit -> t
      val width : t -> int
      val height : t -> int
      val data : t -> Data.t option
    end

    type tilelayer = Tilelayer.t

    module Objectgroup : sig
      module Draworder : sig
        type t = [`Topdown | `Index] [@@deriving eq, ord, show]
      end

      type draworder = Draworder.t

      type t [@@deriving eq, ord, show]

      val make : ?draworder:draworder -> ?objects:Object.t list -> unit -> t
      val draworder : t -> draworder
      val objects : t -> Object.t list
      val get_object : t -> int -> Object.t option
      val get_object_exn : t -> int -> Object.t
      val set_objects : t -> Object.t list -> t
    end

    type objectgroup = Objectgroup.t

    module Imagelayer : sig
      type t [@@deriving eq, ord, show]

      val make : ?image:Image.t -> ?repeatx:bool -> ?repeaty:bool -> unit -> t
      val image : t -> Image.t option
      val repeatx : t -> bool
      val repeaty : t -> bool
    end

    type imagelayer = Imagelayer.t

    module Variant : sig
      type nonrec t =
        [ `Tilelayer of Tilelayer.t
        | `Objectgroup of Objectgroup.t
        | `Imagelayer of Imagelayer.t
        | `Group of t list ]
      [@@deriving eq, ord, show]
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
    val class_ : t -> string option
    val opacity : t -> float
    val visible : t -> bool
    val tintcolor : t -> Color.t option
    val offsetx : t -> float
    val offsety : t -> float
    val parallaxx : t -> float
    val parallaxy : t -> float
    val variant : t -> variant
    val set_variant : t -> variant -> t

    val objects : t -> Object.t list
    val get_object : t -> int -> Object.t option
    val get_object_exn : t -> int -> Object.t

    include Properties with type t := t
  end

  module Tile : sig
    module Frame : sig
      type t [@@deriving eq, ord, show]

      val make : tileid:int -> duration:int -> t
      val tileid : t -> int
      val duration : t -> int
    end

    type frame = Frame.t

    type t [@@deriving eq, ord, show]

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
    val class_ : t -> string option
    val x : t -> int
    val y : t -> int
    val width : t -> int option
    val height : t -> int option
    val image : t -> Image.t option
    val objectgroup : t -> Object.t list
    val animation : t -> Frame.t list

    val set_image : t -> Image.t option -> t
    val set_x : t -> int option -> t
    val set_y : t -> int option -> t
    val set_width : t -> int option -> t
    val set_height : t -> int option -> t

    include Properties with type t := t
  end

  module Tileset : sig
    module Tileoffset : sig
      type t [@@deriving eq, ord, show]

      val make : ?x:int -> ?y:int -> unit -> t
      val x : t -> int
      val y : t -> int
    end

    type tileoffset = Tileoffset.t

    module Single : sig
      type t [@@deriving eq, ord, show]

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
      [@@deriving eq, ord, show]
    end

    type objectalignment = Objectalignment.t

    module Tilerendersize : sig
      type t = [`Tile | `Grid] [@@deriving eq, ord, show]
    end

    type tilerendersize = Tilerendersize.t

    module Fillmode : sig
      type t = [`Stretch | `Preserve_aspect_fit] [@@deriving eq, ord, show]
    end

    type fillmode = Fillmode.t

    module Grid : sig
      type t = [`Orthogonal | `Isometric of int * int]
      [@@deriving eq, ord, show]
    end

    type grid = Grid.t

    module Variant : sig
      type t = [`Single of Single.t | `Collection] [@@deriving eq, ord, show]
    end

    type variant = Variant.t

    type t [@@deriving eq, ord, show]

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
    val class_ : t -> string option
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

    include Properties with type t := t
  end

  module Map : sig
    module Staggeraxis : sig
      type t = [`X | `Y] [@@deriving eq, ord, show]
    end

    type staggeraxis = Staggeraxis.t

    module Staggerindex : sig
      type t = [`Even | `Odd] [@@deriving eq, ord, show]
    end

    type staggerindex = Staggerindex.t

    module Staggered : sig
      type t [@@deriving eq, ord, show]

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
      [@@deriving eq, ord, show]
    end

    type renderorder = Renderorder.t

    module Variant : sig
      type t =
        [ `Hexagonal of Hexagonal.t
        | `Isometric
        | `Orthogonal
        | `Staggered of Staggered.t ]
    end

    type variant = Variant.t

    type t [@@deriving eq, ord, show]

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
      variant:variant ->
      unit ->
      t

    val version : t -> string
    val tiledversion : t -> string option
    val class_ : t -> string option
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
    val variant : t -> variant
    val objects : t -> Object.t list
    val get_object : t -> int -> Object.t option
    val get_object_exn : t -> int -> Object.t
    val nextlayerid : t -> int
    val nextobjectid : t -> int

    val set_layers : t -> Layer.t list -> t

    include Properties with type t := t
  end

  module Template : sig
    type t [@@deriving eq, ord, show]

    val make : ?tileset:int * string -> Object.t -> t
    val tileset : t -> (int * string) option
    val object_ : t -> Object.t
  end

  module Class : sig
    type t [@@deriving eq, ord, show]

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
      [@@deriving eq, ord, show]
    end

    type useas = Useas.t

    val make : useas:Useas.t list -> members:Property.t list -> t
    val useas : t -> Useas.t list
    val members : t -> Property.t list
  end

  module Enum : sig
    module Storagetype : sig
      type t = [`Int | `String] [@@deriving eq, ord, show]
    end

    type storagetype = Storagetype.t

    type t [@@deriving eq, ord, show]

    val make :
      storagetype:storagetype -> valuesasflags:bool -> string list -> t
    val storagetype : t -> Storagetype.t
    val valuesasflags : t -> bool
    val values : t -> string list

    val read_as_string :
      t -> [> `String of string | `Int of int] -> string option

    val read_as_int : t -> [> `String of string | `Int of int] -> int option

    val read_as_alist :
      t -> [> `String of string | `Int of int] -> (string * bool) list option
  end

  module Customtype : sig
    module Variant : sig
      type t = [`Class of Class.t | `Enum of Enum.t] [@@deriving eq, ord, show]
    end

    type variant = Variant.t

    type t [@@deriving eq, ord, show]

    val make : id:int -> name:string -> variant:variant -> t
    val id : t -> int
    val name : t -> string
    val variant : t -> Variant.t
  end
end
