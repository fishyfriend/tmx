module type S = sig
  module Class : Class_intf.S
  module Customtype : Customtype_intf.S
  module Enum : Enum_intf.S
  module Layer : Layer_intf.S
  module Map : Map_intf.S
  module Object : Object_intf.S
  module Property : Property_intf.S
  module Template : Template_intf.S
  module Tile : Tile_intf.S
  module Tileset : Tileset_intf.S

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

  val get_tileset : string -> Tileset.t option
  val get_template : string -> Template.t option
  val get_customtypes : string -> Customtype.t list
  val get_class : string -> useas:Class.useas -> Class.t option
  val get_map : string -> Map.t option
  val get_file : string -> string option
end

module type Intf = sig
  type t = (module S) val make : unit -> t
end
