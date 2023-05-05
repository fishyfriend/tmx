module type S0 = sig
  type t
  type property

  val properties : t -> property list
  val get_property : string -> t -> property option
  val get_property_exn : string -> t -> property

  val own_properties : t -> property list
  val get_own_property : string -> t -> property option
  val get_own_property_exn : string -> t -> property
end

module type S = S0 with type property := Types.property

type 'a t = (module S with type t = 'a)

module type Intf = sig
  (* Helpers for implementing class and property lookup. *)

  module type S0 = S0
  module type S = S

  type 'a t := 'a t

  (* Create accessors.

     [property_lists] gives the lists of properties that should be inherited
     from. Lists to the left take priority. The leftmost list is considered
     "own" properties.

     When [strict] is true, the resulting property list only includes
     properties that appeared in the rightmost list. *)
  val make :
    strict:bool -> property_lists:('a -> Types.property list list) -> 'a t
end
