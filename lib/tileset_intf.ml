module type S = sig
  include module type of Tileset0
end

module type Intf = sig
  module Make (_ : State.S) : S
end
