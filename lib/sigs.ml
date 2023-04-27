module type T = sig
  type t
end

module type EqT = sig
  include T val equal : t -> t -> bool
end

module type OrdT = sig
  include T val compare : t -> t -> int
end

module type ShowT = sig
  include T val show : t -> string val pp : Format.formatter -> t -> unit
end

module type StdT = sig
  include T
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
end

module type ClassPropsT = sig
  type t

  include ClassT with type t := t
  include PropsT with type t := t
end
