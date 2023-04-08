module Format : sig
  type t = [`Bmp | `Gif | `Jpg | `Png]

  include Sigs0.StdT with type t := t
end

type format = Format.t

module Source : sig
  type t = [`File of string | `Embed of Format.t * Data.t]

  include Sigs0.StdT with type t := t
end

type source = Source.t

include Sigs0.StdT

val make :
  source:Source.t -> ?trans:Color.t -> ?width:int -> ?height:int -> unit -> t

val source : t -> Source.t
val trans : t -> Color.t option
val width : t -> int option
val height : t -> int option
