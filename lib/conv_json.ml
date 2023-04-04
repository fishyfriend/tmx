module J = Ezjsonm

open Basic
open Util.Option.Infix

type json = Ezjsonm.value

let wrap name f x =
  try f x with
  | Error.Error (`Json_parse (path, msg)) -> Util.json_parse (name :: path) msg
  | J.Parse_error (_, msg) -> Util.json_parse [name] msg
  | exn ->
      let msg = Printexc.to_string exn in
      Util.json_parse [name] ("parse error: " ^ msg)

let wrap_list f xs = List.mapi (fun i x -> wrap (string_of_int i) f x) xs

let with_json_from_channel ic = wrap "{root}" @@ fun f -> f (J.from_channel ic)

let attr k j f = wrap k (fun j' -> J.find j' [k] |> f) j
let attr_opt k j f = wrap k (fun j' -> J.find_opt j' [k] >|= wrap k f) j

let rec property_value_of_json json =
  match json with
  | `String x -> `String x
  | `Float x -> `Float x
  | `Bool x -> `Bool x
  | `O dict -> `Class (List.map property_of_dict_item dict)
  | _ -> Util.json_parse [] "string, number, boolean, or object expected"

and property_of_dict_item (name, json) =
  wrap name
    (fun json ->
      let value = property_value_of_json json in
      Property.make ~name ~value () )
    json

let property_type_of_json json =
  match J.get_string json with
  | "string" -> `String
  | "int" -> `Int
  | "float" -> `Float
  | "bool" -> `Bool
  | "color" -> `Color
  | "file" -> `File
  | "object" -> `Object
  | "class" -> `Class
  | s -> Util.json_parse [] ("invalid type: " ^ s)

let property_of_json json =
  let name = attr "name" json J.get_string in
  let propertytype = attr_opt "propertytype" json J.get_string in
  let type_ = attr_opt "type" json property_type_of_json |? `String in
  let value =
    let aux f = attr_opt "value" json f in
    match type_ with
    | `String -> `String (aux J.get_string |? "")
    | `Int -> `Int (aux J.get_int |? 0)
    | `Float -> `Float (aux J.get_float |? 0.)
    | `Bool -> `Bool (aux J.get_bool |? false)
    | `Color ->
        let f s = J.get_string s |> Color.of_string in
        `Color (aux f |? Color.trans)
    | `File -> `File (aux J.get_string |? ".")
    | `Object -> `Object (aux J.get_int |? 0)
    | `Class -> aux property_value_of_json |? `Class [] in
  Property.make ~name ?propertytype ~value ()

(* Empirically, some undocumented Tiled JSON formats use camel case keys. *)
let lowercase_keys json =
  let lc_key (k, v) = (String.lowercase_ascii k, v) in
  match json with `O dict -> `O (List.map lc_key dict) | _ -> json

let class_useas_of_json json =
  match J.get_string json with
  | "property" -> `Property
  | "map" -> `Map
  | "layer" -> `Layer
  | "object" -> `Object
  | "tile" -> `Tile
  | "tileset" -> `Tileset
  | "wangcolor" -> `Wangcolor
  | "wangset" -> `Wangset
  | s -> Util.json_parse [] ("invalid useas: " ^ s)

let class_of_json json =
  let json = lowercase_keys json in
  let useas =
    attr "useas" json @@ fun json' ->
    wrap_list class_useas_of_json (J.get_list Fun.id json') in
  let members =
    attr "members" json @@ fun json' ->
    wrap_list property_of_json (J.get_list Fun.id json') in
  Class.make ~useas ~members

let enum_storagetype_of_json json =
  match J.get_string json with
  | "int" -> `Int
  | "string" -> `String
  | s -> Util.json_parse [] ("invalid storagetype " ^ s)

let enum_of_json json =
  let json = lowercase_keys json in
  let storagetype = attr "storagetype" json enum_storagetype_of_json in
  let valuesasflags = attr "valuesasflags" json J.get_bool in
  let values = attr "values" json J.get_strings in
  Enum.make ~storagetype ~valuesasflags values

let customtype_type_of_json json =
  match J.get_string json with
  | "class" -> `Class
  | "enum" -> `Enum
  | s -> Util.json_parse [] ("invalid type " ^ s)

let customtype_of_json json =
  let json = lowercase_keys json in
  let id = attr "id" json J.get_int in
  let name = attr "name" json J.get_string in
  let variant =
    match attr "type" json customtype_type_of_json with
    | `Class -> `Class (class_of_json json)
    | `Enum -> `Enum (enum_of_json json) in
  Customtype.make ~id ~name ~variant

let customtypes_of_json json = J.get_list customtype_of_json json
