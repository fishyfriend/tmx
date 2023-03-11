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
  let open Ezjsonm in
  let map_value f o ~default = Option.(value ~default (map f o)) in
  let name = get_string (find json ["name"]) in
  let propertytype = Option.map get_string (find_opt json ["propertytype"]) in
  let type_ = map_value get_string (find_opt json ["type"]) ~default:"string" in
  let value =
    let json = find_opt json ["value"] in
    match type_ with
    | "string" -> `String (map_value get_string json ~default:"")
    | "int" -> `Int (map_value get_int json ~default:0)
    | "float" -> `Float (map_value get_float json ~default:0.)
    | "bool" -> `Bool (map_value get_bool json ~default:false)
    | "color" ->
        let f json = Color.of_string (get_string json) in
        `Color (map_value f json ~default:Color.trans)
    | "file" -> `File (map_value get_string json ~default:".")
    | "object" -> `Object (map_value get_int json ~default:0)
    | "class" -> map_value property_value_of_json json ~default:(`Class [])
    | _ -> Util.json_parse (`String type_) "Invalid type" in
  Property0.make ~name ?propertytype ~value ()

(* [get_dict] converting all keys to lowercase. Empirically, some undocumented
   Tiled JSON formats use camel case keys. *)
let get_dict_lc json =
  let dict = Ezjsonm.get_dict json in
  let lc_key (k, v) = (String.lowercase_ascii k, v) in
  List.map lc_key dict

let class_of_json json =
  let dict = get_dict_lc json in
  let useas =
    let of_json json =
      match Ezjsonm.get_string json with
      | "property" -> `Property
      | "map" -> `Map
      | "layer" -> `Layer
      | "object" -> `Object
      | "tile" -> `Tile
      | "tileset" -> `Tileset
      | "wangcolor" -> `Wangcolor
      | "wangset" -> `Wangset
      | _ -> Util.json_parse json "Invalid useas" in
    Ezjsonm.get_list of_json (List.assoc "useas" dict) in
  let members = Ezjsonm.get_list property_of_json (List.assoc "members" dict) in
  Class0.make ~useas ~members

let enum_of_json json =
  let dict = get_dict_lc json in
  let storagetype =
    let of_json json =
      match Ezjsonm.get_string json with
      | "int" -> `Int
      | "string" -> `String
      | s -> Util.invalid_arg s in
    of_json (List.assoc "storagetype" dict) in
  let valuesasflags = Ezjsonm.get_bool (List.assoc "valuesasflags" dict) in
  let values = Ezjsonm.get_strings (List.assoc "values" dict) in
  Enum0.make ~storagetype ~valuesasflags values

let wrap f json =
  try f json with Ezjsonm.Parse_error (json, msg) -> Util.json_parse json msg

let customtype_of_json =
  wrap @@ fun json ->
  let dict = get_dict_lc json in
  let id = Ezjsonm.get_int (List.assoc "id" dict) in
  let name = Ezjsonm.get_string (List.assoc "name" dict) in
  let variant =
    match Ezjsonm.get_string (List.assoc "type" dict) with
    | "class" -> `Class (class_of_json json)
    | "enum" -> `Enum (enum_of_json json)
    | s -> Util.json_parse (`String s) "Invalid type" in
  Customtype0.make ~id ~name ~variant
