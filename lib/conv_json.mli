type json

val with_json_from_channel : in_channel -> (json -> 'a) -> 'a
val customtype_of_json : json -> Core_generic.Customtype.t
val customtypes_of_json : json -> Core_generic.Customtype.t list
