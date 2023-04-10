type t = {name : string; propertytype : string option; value : value}

and value =
  [ `String of string
  | `Int of int
  | `Float of float
  | `Bool of bool
  | `Color of Color.t
  | `File of string
  | `Object of int
  | `Class of t list ]
[@@deriving eq, ord, show { with_path = false }]

module Value = struct type t = value [@@deriving eq, ord, show { with_path = false }] end

let make ~name ?propertytype ~value () =
  let value =
    match value with
    | `Class props -> `Class (List.sort_uniq compare props)
    | _ -> value in
  {name; propertytype; value}

let name t = t.name
let propertytype t = t.propertytype
let value t = t.value
