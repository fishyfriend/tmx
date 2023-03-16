include Map_intf

module Make (State : State_intf.S) : S = struct
  include Map0
  include Properties.Make (State) (struct include Map0 let useas = `Map end)
end
