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
end

module type Intf = sig
  type t = (module S) val make : unit -> t
end
