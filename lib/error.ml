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

let equal t t' = t = t'
let compare t t' = compare t t'

let rec show_path segs ~sep =
  match segs with
  | [] -> ""
  | seg :: segs ->
    ( match int_of_string_opt seg with
    | Some i -> Format.sprintf "%s[%d]" (show_path segs ~sep) i
    | None -> Format.sprintf "%s%c%s" (show_path segs ~sep) sep seg )

let show t =
  let open Format in
  match t with
  | `Invalid_arg (arg, msg) -> sprintf "Invalid %s argument: %s" arg msg
  | `Nested_template -> "Template's object may not have a template"
  | `Json_parse (path, msg) ->
      sprintf "JSON parse error at %s: %s" (show_path ~sep:'/' path) msg
  | `Xml_parse (path, msg) ->
      sprintf "XML parse error at %s: %s" (show_path ~sep:'.' path) msg
  | `Base64 msg -> sprintf "Base64 error: %s" msg
  | `Gzip msg -> sprintf "Gzip error: %s" msg
  | `Zlib msg -> sprintf "Zlib error: %s" msg
  | `Zstd msg -> sprintf "Zstd error: %s" msg
  | `Duplicate (kind, name) -> sprintf "Duplicate %s: %s" kind name
  | `Not_found (kind, name) ->
      sprintf "%s not found: %s" (String.capitalize_ascii kind) name
  | `Other exn -> sprintf "Uncategorized error: %s" (Printexc.to_string exn)

let pp ppf t = Format.pp_print_string ppf (show t)

type exn += Error of t

let print_exn exn : string option =
  match exn with Error t -> Some (show t) | _ -> None

let () = Printexc.register_printer print_exn
