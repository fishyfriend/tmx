type t

val argb : int -> int -> int -> int -> t
val rgb : int -> int -> int -> t
val a : t -> int
val r : t -> int
val g : t -> int
val b : t -> int
val of_int32 : int32 -> t
val to_int32 : t -> int32
val of_string : string -> t
val of_string_argb : string -> t
val of_string_rgb : string -> t
val to_string : t -> string
val to_string_argb : t -> string
val to_string_rgb : t -> string
val trans : t
val black : t

include Sigs.StdT with type t := t
