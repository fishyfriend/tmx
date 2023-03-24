include State_intf

module Make () : S = struct
  include State0

  let the_state = ref State0.empty

  let read f = f !the_state
  let update f = the_state := f !the_state
end
