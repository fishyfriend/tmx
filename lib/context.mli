type t

val default : t

val tilesets : t -> (int * string * Core_generic.Tileset.t) list
val templates : t -> (string * Core_generic.Template.t) list
val customtypes : t -> Core_generic.Customtype.t list
val maps : t -> (string * Core_generic.Map.t) list
val files : t -> (string * string) list

val get_tileset : string -> t -> (int * Core_generic.Tileset.t) option
val get_template : string -> t -> Core_generic.Template.t option
val get_customtypes : string -> t -> Core_generic.Customtype.t list
val get_class : string -> t -> useas:Core_generic.Class.Useas.t -> Types.class_ option
val get_map : string -> t -> Core_generic.Map.t option
val get_file : string -> t -> string option
val get_tile : Gid.t -> t -> Core_generic.Tile.t option

val add_tileset_exn : string -> Core_generic.Tileset.t -> t -> t
val add_template_exn : string -> Core_generic.Template.t -> t -> t
val add_customtype_exn : Core_generic.Customtype.t -> t -> t
val add_file_exn : string -> string -> t -> t
val add_map_exn : string -> Core_generic.Map.t -> t -> t

val remove_tileset : string -> t -> t
val remove_template : string -> t -> t
val remove_customtypes : string -> t -> t
val remove_class : string -> useas:Core_generic.Class.Useas.t -> t -> t
val remove_file : string -> t -> t
val remove_map : string -> t -> t

val make_getters : t ref -> (module Sigs.Getters)
