type t = [`Invalid_arg of string] [@@deriving eq, ord, show]

type exn += Error of t
