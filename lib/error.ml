type t =
  [ `Invalid_arg of string * string
  | `Nested_template
  | `Object_not_found of int
  | `Json_parse of string list * string
  | `Xml_parse of string list * string
  | `Base64 of string
  | `Gzip of string
  | `Zlib of string
  | `Zstd of string ]

let equal t t' = t = t'
let compare t t' = compare t t'

let rec show_path segs ~top ~sep ~arr =
  match segs with
  | [] -> top
  | seg :: segs ->
      let bracket i = Format.sprintf "%c%d%c" arr.[0] i arr.[1] in
      ( match int_of_string_opt seg with
      | Some i -> show_path segs ~top ~sep ~arr ^ bracket i
      | None -> show_path segs ~top ~sep ~arr ^ sep ^ seg )

let show_xml_path = show_path ~top:"{root}" ~sep:"/" ~arr:"[]"
let show_json_path = show_path ~top:"$" ~sep:"." ~arr:"[]"

let show t =
  match t with
  | `Invalid_arg (arg, msg) -> Format.sprintf "Invalid %s argument: %s" arg msg
  | `Nested_template -> "Template's object may not have a template"
  | `Object_not_found id -> Format.sprintf "Object %d not found" id
  | `Json_parse (path, msg) ->
      Format.sprintf "JSON parse error at %s: %s" (show_json_path path) msg
  | `Xml_parse (path, msg) ->
      Format.sprintf "XML parse error at %s: %s" (show_xml_path path) msg
  | `Base64 msg -> Format.sprintf "Base64 error: %s" msg
  | `Gzip msg -> Format.sprintf "Gzip error: %s" msg
  | `Zlib msg -> Format.sprintf "Zlib error: %s" msg
  | `Zstd msg -> Format.sprintf "Zstd error: %s" msg

let pp ppf t = Format.pp_print_string ppf (show t)

type exn += Error of t

let print_exn exn : string option =
  match exn with Error t -> Some (show t) | _ -> None

let () = Printexc.register_printer print_exn
