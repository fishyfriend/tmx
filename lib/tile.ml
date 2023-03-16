include Tile_intf

module Make (State : State_intf.S) : S = struct
  include Tile0
  include Properties.Make (State) (struct include Tile0 let useas = `Tile end)
end
