type t

val default : t

val tilesets : t -> (int * string * Types.tileset) list
val templates : t -> (string * Types.template) list
val customtypes : t -> Types.customtype list
val maps : t -> (string * Types.map) list
val files : t -> (string * string) list

val get_tileset : string -> t -> (int * Types.tileset) option
val get_template : string -> t -> Types.template option
val get_customtypes : string -> t -> Types.customtype list
val get_map : string -> t -> Types.map option
val get_file : string -> t -> string option
val get_tile : Gid.t -> t -> Types.tile option

val add_tileset_exn : string -> Types.tileset -> t -> t
val add_template_exn : string -> Types.template -> t -> t
val add_customtype_exn : Types.customtype -> t -> t
val add_file_exn : string -> string -> t -> t
val add_map_exn : string -> Types.map -> t -> t

val remove_tileset : string -> t -> t
val remove_template : string -> t -> t
val remove_customtypes : string -> t -> t
val remove_class : string -> useas:Types.useas -> t -> t
val remove_file : string -> t -> t
val remove_map : string -> t -> t
