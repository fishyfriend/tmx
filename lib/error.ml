type t =
  [ `Invalid_arg of string * string
  | `Nested_template
  | `Json_parse of string option * string list * string
  | `Xml_parse of string option * string list * string
  | `Base64 of string
  | `Gzip of string
  | `Zlib of string
  | `Zstd of string
  | `Duplicate of string * string
  | `Not_found of string * string
  | `Other of exn ]

let equal t t' = t = t'
let compare t t' = compare t t'

let show_path segs ~sep =
  List.fold_left
    (fun s seg ->
      match int_of_string_opt seg with
      | Some i -> Format.sprintf "%s[%d]" s i
      | None -> if s = "" then seg else Format.sprintf "%s%c%s" s sep seg )
    "" segs

let show_file_opt fname = Option.value fname ~default:"<unknown>"

let show t =
  let open Format in
  match t with
  | `Invalid_arg (arg, msg) -> sprintf "Invalid %s argument: %s" arg msg
  | `Nested_template -> "Template's object may not have a template"
  | `Json_parse (fname, path, msg) ->
      sprintf "JSON parse error in file %s at %s: %s" (show_file_opt fname)
        (show_path ~sep:'/' path) msg
  | `Xml_parse (fname, path, msg) ->
      sprintf "XML parse error in file %s at %s: %s" (show_file_opt fname)
        (show_path ~sep:'.' path) msg
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
