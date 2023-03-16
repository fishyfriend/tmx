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

  val make : ?draworder:draworder -> ?objects:Object0.t list -> unit -> t
  val draworder : t -> draworder
  val objects : t -> Object0.t list
  val get_object : t -> int -> Object0.t option
  val get_object_exn : t -> int -> Object0.t
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
  ?properties:Property0.t list ->
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
val properties : t -> Property0.t list
val variant : t -> variant

val objects : t -> Object0.t list
val get_object : t -> int -> Object0.t option
val get_object_exn : t -> int -> Object0.t
