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

module type RelocT = sig
  include T val reloc : t -> from_:string -> to_:string -> t
end
