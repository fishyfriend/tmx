module Variant : sig
  type t = [`Class of Class0.t | `Enum of Enum0.t] [@@deriving eq, ord, show]
end

type variant = Variant.t

type t [@@deriving eq, ord, show]

val make : id:int -> name:string -> variant:variant -> t
val id : t -> int
val name : t -> string
val variant : t -> Variant.t
