module type Eq = sig
  type t val equal : t -> t -> bool
end

module type Ord = sig
  type t val compare : t -> t -> int
end

module type Show = sig
  type t val pp : Format.formatter -> t -> unit val show : t -> string
end

module type Std = sig
  type t
  include Eq with type t := t
  include Ord with type t := t
  include Show with type t := t
end
