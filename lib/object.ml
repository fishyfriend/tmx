include Object_intf

module Make (State : State_intf.S) : S = struct include Object0 end
