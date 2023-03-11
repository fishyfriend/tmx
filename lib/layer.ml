include Layer_intf

module Make (State : State_intf.S) : S = struct include Layer0 end
