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

let tap f x =
  ignore (f x) ;
  x

let protect_result f x =
  try Result.ok (f x) with
  | Error.Error e -> Result.error e
  | exn -> Result.error (`Other exn)

let protect_opt f x = try Some (f x) with _ -> None

let throw err = raise (Error.Error err)
let invalid_arg name value = throw (`Invalid_arg (name, value))
let nested_template () = throw `Nested_template
let base64 msg = throw (`Base64 msg)
let json_parse path msg = throw (`Json_parse (path, msg))
let xml_parse path msg = throw (`Xml_parse (path, msg))
let gzip e = throw (`Zlib (Format.asprintf "%a" Ezgzip.pp_error e))
let zlib e = throw (`Zlib (Format.asprintf "%a" Ezgzip.Z.pp_error e))
let zstd () = throw (`Zstd "Zstd compression not implemented")
let duplicate kind name = throw (`Duplicate (kind, name))
let not_found kind name = throw (`Not_found (kind, name))
let object_not_found id = not_found "object" (string_of_int id)
let file_not_found fname = not_found "file" fname
let other exn = throw (`Other exn)
