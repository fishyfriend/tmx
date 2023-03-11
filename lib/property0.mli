type t [@@deriving eq, ord, show]

module Value : sig
  type nonrec t =
    [ `String of string
    | `Int of int
    | `Float of float
    | `Bool of bool
    | `Color of Color.t
    | `File of string
    | `Object of int
    | `Class of t list ]
  [@@deriving eq, ord, show]
end

type value = Value.t

val make : name:string -> ?propertytype:string -> value:Value.t -> unit -> t
val name : t -> string
val propertytype : t -> string option
val value : t -> Value.t
