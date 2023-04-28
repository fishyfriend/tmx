module type S = sig
  include Core.S

  val tilesets : unit -> (string * Tileset.t) list
  val templates : unit -> (string * Template.t) list
  val files : unit -> (string * string) list
  val customtypes : unit -> Customtype.t list
  val maps : unit -> (string * Map.t) list

  val get_tileset : string -> Tileset.t option
  val get_template : string -> Template.t option
  val get_customtypes : string -> Customtype.t list
  val get_class : string -> useas:Class.useas -> Class.t option
  val get_map : string -> Map.t option
  val get_file : string -> string option
  val get_tile : Gid.t -> Tile.t option

  val get_tileset_exn : string -> Tileset.t
  val get_template_exn : string -> Template.t
  val get_class_exn : string -> useas:Class.useas -> Class.t
  val get_map_exn : string -> Map.t
  val get_file_exn : string -> string
  val get_tile_exn : Gid.t -> Tile.t

  val load_tileset_xml : string -> (Tileset.t, Error.t) result
  val load_template_xml : string -> (Template.t, Error.t) result
  val load_customtypes_json : string -> (Customtype.t list, Error.t) result
  val load_file : string -> (string, Error.t) result
  val load_map_xml : string -> (Map.t, Error.t) result

  val load_tileset_xml_exn : string -> Tileset.t
  val load_template_xml_exn : string -> Template.t
  val load_customtypes_json_exn : string -> Customtype.t list
  val load_file_exn : string -> string
  val load_map_xml_exn : string -> Map.t

  val unload_tileset : string -> unit
  val unload_template : string -> unit
  val unload_customtypes : string -> unit
  val unload_class : string -> useas:Class.useas -> unit
  val unload_file : string -> unit
  val unload_map : string -> unit
end

type t = (module S)

module type Intf = sig
  module type S = S

  type t = (module S)

  val make : ?load_file_props:bool -> root:string -> unit -> t
end
