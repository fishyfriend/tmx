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
  ?properties:Property0.t list ->
  ?tilesets:(int * [`Embed of Tileset0.t | `File of string]) list ->
  ?layers:Layer0.t list ->
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
val properties : t -> Property0.t list
val tilesets : t -> (int * [`File of string | `Embed of Tileset0.t]) list
val layers : t -> Layer0.t list
val variant : t -> variant
val objects : t -> Object0.t list
val get_object : t -> int -> Object0.t option
val get_object_exn : t -> int -> Object0.t
val nextlayerid : t -> int
val nextobjectid : t -> int
