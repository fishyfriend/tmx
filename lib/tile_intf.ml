module type S = sig
  include module type of Tile0 include Properties_intf.S with type t := t
end

module type Intf = sig
  module Make (_ : State.S) : S
end
