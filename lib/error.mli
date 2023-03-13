type t =
  [ `Invalid_arg of string * string
  | `Nested_template
  | `Tilecount of int * int
  | `Object_not_found of int
  | `Json_parse of string * string
  | `Xml_parse of string * string
  | `Base64 of string
  | `Gzip of string
  | `Zlib of string
  | `Zstd of string ]
[@@deriving eq, ord, show]

type exn += Error of t
