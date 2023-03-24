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
  ?properties:Property0.t list ->
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
val properties : t -> Property0.t list
val shape : t -> shape
val width : t -> float
val height : t -> float

val set_shape : t -> shape option -> t

(* TODO: this will go away? *)
val raw_shape : t -> shape option
