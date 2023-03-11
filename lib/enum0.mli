module Storagetype : sig
  type t = [`Int | `String] [@@deriving eq, ord, show]
end

type storagetype = Storagetype.t

type t [@@deriving eq, ord, show]

val make : storagetype:storagetype -> valuesasflags:bool -> string list -> t
val storagetype : t -> Storagetype.t
val valuesasflags : t -> bool
val values : t -> string list
