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

  (* val load_tileset : string -> (Tileset0.t, Error.t) result *)
  val load_tileset_xml_exn : string -> Tileset0.t

  (* val load_template : string -> (Template0.t, Error.t) result *)
  val load_template_xml_exn : string -> Template0.t

  (* val load_customtypes : string -> (Customtype0.t list, Error.t) result *)
  val load_customtypes_json_exn : string -> Customtype0.t list

  (* val load_file : string -> (string, Error.t) result *)
  val load_file_exn : string -> string

  val load_map_xml_exn : string -> Map0.t
end

module type Intf = sig
  type t = (module S) val make : unit -> t
end
