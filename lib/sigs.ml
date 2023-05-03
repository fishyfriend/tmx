module type EqT = sig
  type t

  val equal : t -> t -> bool
end

module type OrdT = sig
  type t

  val compare : t -> t -> int
end

module type ShowT = sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module type StdT = sig
  type t

  include EqT with type t := t
  include OrdT with type t := t
  include ShowT with type t := t
end

module type ClassT = sig
  type t

  val class_ : t -> string option
end

module type PropsT = sig
  type t
  type property

  val properties : t -> property list
  val get_property : string -> t -> property option
  val get_property_exn : string -> t -> property

  val own_properties : t -> property list
  val get_own_property : string -> t -> property option
  val get_own_property_exn : string -> t -> property
end
