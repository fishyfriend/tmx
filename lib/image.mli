module Format : sig
  type t = [`Bmp | `Gif | `Jpg | `Png] [@@deriving eq, ord, show]
end

type format = Format.t

module Source : sig
  type t = [`File of string | `Embed of Format.t * Data.t]
  [@@deriving eq, ord, show]
end

type source = Source.t

type t [@@deriving eq, ord, show]

val make :
  source:Source.t -> ?trans:Color.t -> ?width:int -> ?height:int -> unit -> t

val source : t -> Source.t
val trans : t -> Color.t option
val width : t -> int option
val height : t -> int option
