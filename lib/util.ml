let error err = raise (Error.Error err)
let invalid_arg arg = error (`Invalid_arg arg)
let nested_template () = error `Nested_template
let tilecount n_exp n = error (`Tilecount (n_exp, n))
let object_not_found id = error (`Object_not_found id)
let json_parse json msg =
  let repr = Ezjsonm.value_to_string json in
  error (`Json_parse (repr, msg))

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
