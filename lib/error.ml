type t =
  [ `Invalid_arg of string * string
  | `Nested_template
  | `Tilecount of int * int
  | `Object_not_found of int
  | `Json_parse of string * string
  | `Xml_parse of string * string ]
[@@deriving eq, ord, show]

type exn += Error of t

(* TODO: friendlier error messages *)

let print_exn exn : string option =
  match exn with Error t -> Some (show t) | _ -> None

let () = Printexc.register_printer print_exn
