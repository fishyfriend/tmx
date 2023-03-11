include Property_intf

module Make (State : State_intf.S) : S = struct include Property0 end
