module J = Ezjsonm

let rec property_value_of_json json =
  match json with
  | `String x -> `String x
  | `Float x -> `Float x
  | `Bool x -> `Bool x
  | `O dict ->
      let props =
        List.map
          (fun (name, json') ->
            let value = property_value_of_json json' in
            Property0.make ~name ~value () )
          dict in
      `Class props
  | _ -> Util.json_parse json "String, number, boolean, or object expected"

let property_of_json json =
  let open Util.Option_infix in
  let dict = J.get_dict json in
  let name = List.assoc "name" dict |> J.get_string in
  let propertytype = List.assoc_opt "propertytype" dict >|= J.get_string in
  let type_ = List.assoc_opt "type" dict >|= J.get_string |? "string" in
  let value =
    let json' = List.assoc_opt "value" dict in
    match type_ with
    | "string" -> `String (json' >|= J.get_string |? "")
    | "int" -> `Int (json' >|= J.get_int |? 0)
    | "float" -> `Float (json' >|= J.get_float |? 0.)
    | "bool" -> `Bool (json' >|= J.get_bool |? false)
    | "color" ->
        `Color (json' >|= J.get_string >|= Color.of_string |? Color.trans)
    | "file" -> `File (json' >|= J.get_string |? ".")
    | "object" -> `Object (json' >|= J.get_int |? 0)
    | "class" -> json' >|= property_value_of_json |? `Class []
    | _ -> Util.json_parse (`String type_) "Invalid type" in
  Property0.make ~name ?propertytype ~value ()

(* [get_dict] converting all keys to lowercase. Empirically, some undocumented
   Tiled JSON formats use camel case keys. *)
let get_dict_lc json =
  let dict = J.get_dict json in
  let lc_key (k, v) = (String.lowercase_ascii k, v) in
  List.map lc_key dict

let class_of_json json =
  let dict = get_dict_lc json in
  let useas =
    let of_json json =
      match J.get_string json with
      | "property" -> `Property
      | "map" -> `Map
      | "layer" -> `Layer
      | "object" -> `Object
      | "tile" -> `Tile
      | "tileset" -> `Tileset
      | "wangcolor" -> `Wangcolor
      | "wangset" -> `Wangset
      | _ -> Util.json_parse json "Invalid useas" in
    J.get_list of_json (List.assoc "useas" dict) in
  let members = J.get_list property_of_json (List.assoc "members" dict) in
  Class0.make ~useas ~members

let enum_of_json json =
  let dict = get_dict_lc json in
  let storagetype =
    let of_json json =
      match J.get_string json with
      | "int" -> `Int
      | "string" -> `String
      | s -> Util.invalid_arg s in
    of_json (List.assoc "storagetype" dict) in
  let valuesasflags = J.get_bool (List.assoc "valuesasflags" dict) in
  let values = J.get_strings (List.assoc "values" dict) in
  Enum0.make ~storagetype ~valuesasflags values

let wrap f json =
  try f json with J.Parse_error (json, msg) -> Util.json_parse json msg

let customtype_of_json =
  wrap @@ fun json ->
  let dict = get_dict_lc json in
  let id = J.get_int (List.assoc "id" dict) in
  let name = J.get_string (List.assoc "name" dict) in
  let variant =
    match J.get_string (List.assoc "type" dict) with
    | "class" -> `Class (class_of_json json)
    | "enum" -> `Enum (enum_of_json json)
    | s -> Util.json_parse (`String s) "Invalid type" in
  Customtype0.make ~id ~name ~variant
