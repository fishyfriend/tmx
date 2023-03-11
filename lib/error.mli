type t =
  [ `Invalid_arg of string
  | `Nested_template
  | `Tilecount of int * int
  | `Object_not_found of int
  | `Json_parse of string * string ]
[@@deriving eq, ord, show]

type exn += Error of t
