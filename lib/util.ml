module Option_infix = struct
  let ( >>= ) o f = Option.bind o f
  let ( >|= ) o f = Option.map f o

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >|= )

  let (( and* ) as ( and+ )) =
   fun o o' ->
    let* x = o in
    let* x' = o' in
    Some (x, x')

  let ( |? ) o default = Option.value o ~default
end

(* TODO: move these somewhere, perhaps a submodule *)

let throw err = raise (Error.Error err)
let invalid_arg name value = throw (`Invalid_arg (name, value))
let nested_template () = throw `Nested_template
let tilecount n_exp n = throw (`Tilecount (n_exp, n))
let object_not_found id = throw (`Object_not_found id)

let json_parse json msg =
  let repr = Ezjsonm.value_to_string json in
  throw (`Json_parse (repr, msg))

let xml_parse (attrs, nodes) msg =
  let repr = Ezxmlm.to_string [`El ((("", "???"), attrs), nodes)] in
  throw (`Xml_parse (repr, msg))

let base64 msg = throw (`Base64 msg)

let gzip e =
  let msg = Format.asprintf "%a" Ezgzip.pp_error e in
  throw (`Gzip msg)

let zlib e =
  let msg = Format.asprintf "%a" Ezgzip.Z.pp_error e in
  throw (`Zlib msg)

let zstd () = throw (`Zstd "Zstd compression not implemented")
