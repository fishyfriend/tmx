module type S = sig
  include module type of State0

  val read : (t -> 'a) -> 'a
  val update : (t -> t) -> unit
end

module type Intf = sig
  module type S = S module Make () : S
end
