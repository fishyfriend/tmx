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
  ?properties:Property0.t list ->
  ?image:Image.t ->
  ?objectgroup:Object0.t list ->
  ?animation:frame list ->
  unit ->
  t

val id : t -> int
val class_ : t -> string
val x : t -> int
val y : t -> int
val width : t -> int option
val height : t -> int option
val properties : t -> Property0.t list
val image : t -> Image.t option
val objectgroup : t -> Object0.t list
val animation : t -> Frame.t list

val set_image : t -> Image.t option -> t
val set_x : t -> int option -> t
val set_y : t -> int option -> t
val set_width : t -> int option -> t
val set_height : t -> int option -> t
