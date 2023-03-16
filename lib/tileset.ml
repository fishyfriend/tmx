include Tileset_intf

module Make (State : State_intf.S) : S = struct
  include Tileset0
  include
    Properties.Make (State) (struct include Tileset0 let useas = `Tileset end)
end
