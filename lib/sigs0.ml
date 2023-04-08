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
