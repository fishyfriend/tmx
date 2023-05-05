module type EqT = sig
  type t

  val equal : t -> t -> bool
end

module type OrdT = sig
  type t

  val compare : t -> t -> int
end

module type ShowT = sig
  (** Pretty-printers for debugging.

    These functions reveal data structure internals and may be removed in a
    future release. *)

  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module type StdT = sig
  type t

  include EqT with type t := t  (** @closed *)

  include OrdT with type t := t  (** @closed *)

  include ShowT with type t := t  (** @closed *)
end

module type ClassT = sig
  type t

  val class_ : t -> string option
end
