module Storagetype : sig
  type t = [`Int | `String]

  include Sigs0.StdT with type t := t
end

type storagetype = Storagetype.t

type t

val make : storagetype:storagetype -> valuesasflags:bool -> string list -> t
val storagetype : t -> Storagetype.t
val valuesasflags : t -> bool
val values : t -> string list

val read_as_string : t -> [> `String of string | `Int of int] -> string option

val read_as_int : t -> [> `String of string | `Int of int] -> int option

val read_as_alist :
  t -> [> `String of string | `Int of int] -> (string * bool) list option

include Sigs0.StdT with type t := t
