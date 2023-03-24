type t

val empty : t

val tilesets : t -> (int * string * Tileset0.t) list
val templates : t -> (string * Template0.t) list
val customtypes : t -> Customtype0.t list
val maps : t -> (string * Map0.t) list
val files : t -> (string * string) list

val get_tileset : string -> t -> (int * Tileset0.t) option
val get_template : string -> t -> Template0.t option
val get_customtype : string -> t -> Customtype0.t option
val get_class : string -> t -> useas:Class0.useas -> Class0.t option
val get_file : string -> t -> string option

val get_tileset_exn : string -> t -> int * Tileset0.t
val get_template_exn : string -> t -> Template0.t
val get_customtype_exn : string -> t -> Customtype0.t
val get_class_exn : string -> t -> useas:Class0.useas -> Class0.t
val get_file_exn : string -> t -> string

val add_tileset_exn : string -> Tileset0.t -> t -> t
val add_template_exn : string -> Template0.t -> t -> t
val add_customtype_exn : Customtype0.t -> t -> t
val add_file_exn : string -> string -> t -> t
val add_map_exn : string -> Map0.t -> t -> t

val get_object_tile : Object0.t -> Gid.t -> t -> Tile0.t option
