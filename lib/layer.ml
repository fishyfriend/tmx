include Layer_intf

module Make (State : State_intf.S) : S = struct
  include Layer0
  include
    Properties.Make (State) (struct include Layer0 let useas = `Layer end)
end
