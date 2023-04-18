type t

val default : t

val tilesets : t -> (int * string * Basic.Tileset.t) list
val templates : t -> (string * Basic.Template.t) list
val customtypes : t -> Basic.Customtype.t list
val maps : t -> (string * Basic.Map.t) list
val files : t -> (string * string) list

val get_tileset : string -> t -> (int * Basic.Tileset.t) option
val get_template : string -> t -> Basic.Template.t option
val get_customtypes : string -> t -> Basic.Customtype.t list
val get_class : string -> t -> useas:Basic.Class.useas -> Basic.Class.t option
val get_map : string -> t -> Basic.Map.t option
val get_file : string -> t -> string option
val get_object_tile : Basic.Object.t -> Gid.t -> t -> Basic.Tile.t option

val add_tileset_exn : string -> Basic.Tileset.t -> t -> t
val add_template_exn : string -> Basic.Template.t -> t -> t
val add_customtype_exn : Basic.Customtype.t -> t -> t
val add_file_exn : string -> string -> t -> t
val add_map_exn : string -> Basic.Map.t -> t -> t

val remove_tileset : string -> t -> t
val remove_template : string -> t -> t
val remove_customtypes : string -> t -> t
val remove_class : string -> useas:Basic.Class.useas -> t -> t
val remove_file : string -> t -> t
val remove_map : string -> t -> t
