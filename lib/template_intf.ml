module type S = sig
  include module type of Template0
end

module type Intf = sig
  module Make (_ : State.S) : S
end
