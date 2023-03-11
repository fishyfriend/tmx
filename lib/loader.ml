include Loader_intf

module Make () : S = struct
  module State = State.Make ()
  include State

  module Class = Class.Make (State)
  module Customtype = Customtype.Make (State)
  module Enum = Enum.Make (State)
  module Layer = Layer.Make (State)
  module Map = Map.Make (State)
  module Object = Object.Make (State)
  module Property = Property.Make (State)
  module Template = Template.Make (State)
  module Tile = Tile.Make (State)
  module Tileset = Tileset.Make (State)
end

type t = (module S)

let make () = (module Make () : S)
