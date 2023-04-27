module Flags : sig
  include Sigs.StdT

  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val flip_horizontal : t
  val flip_vertical : t
  val flip_diagonal : t
  val rotate_60 : t
  val rotate_120 : t
  val all : t
  val empty : t
end

type t

val make : ?flags:Flags.t -> int -> t
val id : t -> int
val flags : t -> Flags.t
val of_int32 : int32 -> t
val to_int32 : t -> int32
val max_id : int

include Sigs.StdT with type t := t
