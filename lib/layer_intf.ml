module type S = sig
  include module type of Layer0 include Properties_intf.S with type t := t
end

module type Intf = sig
  module Make (_ : State.S) : S
end
