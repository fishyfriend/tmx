module type PropsType = sig
  type t val property_lists : t -> Property0.t list list
end

module type ClassType = sig
  type t
  val useas : Class0.useas
  val class_ : t -> string option
  val properties : t -> Property0.t list
end

module type S = sig
  type t

  val properties : t -> Property0.t list
  val get_property : string -> t -> Property0.t option
  val get_property_exn : string -> t -> Property0.t
end

module type Intf = sig
  module Make0 (T : PropsType) : S with type t := T.t
  module Make0_strict (T : PropsType) : S with type t := T.t
  module Make (_ : State_intf.S) (T : ClassType) : S with type t := T.t
  module Make_strict (_ : State_intf.S) (T : ClassType) : S with type t := T.t

  (* Override the properties of [ts0] with [ts]. If [strict] is true, exclude
     properties in [ts] that are not in [ts0]. *)
  val merge_property_lists :
    strict:bool -> Property0.t list -> Property0.t list -> Property0.t list
end
