type t =
  [ `Invalid_arg of string * string
  | `Nested_template
  | `Json_parse of string list * string
  | `Xml_parse of string list * string
  | `Base64 of string
  | `Gzip of string
  | `Zlib of string
  | `Zstd of string
  | `Duplicate of string * string
  | `Not_found of string * string
  | `Other of exn ]
[@@deriving eq, ord, show]

type exn += Error of t
