module Option = struct
  let protect f x = try Some (f x) with _ -> None

  module Infix = struct
    let ( >>= ) o f = Option.bind o f
    let ( >|= ) o f = Option.map f o
    let ( |? ) o default = Option.value o ~default
    let ( >>? ) o f = match o with Some _ -> o | None -> f ()
    let ( >|? ) o f = match o with Some x -> x | None -> f ()
  end
end

module Error = struct
  let protect f x =
    try Result.ok (f x) with
    | Error.Error e -> Result.error e
    | exn -> Result.error (`Other exn)

  let throw err = raise (Error.Error err)
  let invalid_arg name value = throw (`Invalid_arg (name, value))
  let nested_template () = throw `Nested_template
  let base64 msg = throw (`Base64 msg)
  let json_parse ?fname path msg = throw (`Json_parse (fname, path, msg))
  let xml_parse ?fname path msg = throw (`Xml_parse (fname, path, msg))
  let gzip e = throw (`Zlib (Format.asprintf "%a" Ezgzip.pp_error e))
  let zlib e = throw (`Zlib (Format.asprintf "%a" Ezgzip.Z.pp_error e))
  let zstd () = throw (`Zstd "Zstd compression not implemented")
  let duplicate kind name = throw (`Duplicate (kind, name))
  let not_found kind name = throw (`Not_found (kind, name))
  let object_not_found id = not_found "object" (string_of_int id)
  let file_not_found fname = not_found "file" fname
  let other exn = throw (`Other exn)
end

module Filename = struct
  let split path =
    assert (String.length Filename.dir_sep = 1) ;
    String.split_on_char Filename.dir_sep.[0] path
    |> List.fold_left
         (fun parts part ->
           if part = "" || part = Filename.current_dir_name then parts
           else if part = Filename.parent_dir_name then
             match parts with [] -> part :: parts | _ :: parts -> parts
           else part :: parts )
         []
    |> List.rev

  let reloc ~from_ ~to_ path =
    let path = Filename.concat from_ path in
    let xs, ys = (split path, split to_) in
    let rec aux xs ys =
      match (xs, ys) with
      | x :: xs, y :: ys when x = y -> aux xs ys
      | xs, _ :: ys -> Filename.concat ".." (aux xs ys)
      | xs, [] -> List.fold_left Filename.concat "" xs in
    aux xs ys
end
