module type S = sig end

module type Intf = sig
  module type S = S module Make () : S
end
