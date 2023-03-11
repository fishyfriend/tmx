include Map_intf

module Make (State : State_intf.S) : S = struct include Map0 end
