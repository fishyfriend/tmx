module type S = sig
  include module type of Object0
  include Properties_intf.S with type t := t

  val template_class : t -> string option
  val template_properties : t -> Property0.t list
  val tile : t -> Tile0.t option
  val tile_class : t -> string option
  val tile_properties : t -> Property0.t list
end

module type Intf = sig
  module Make (_ : State.S) : S
end
