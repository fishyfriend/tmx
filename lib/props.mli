(* Helpers for implementing class and property lookup *)

module type S = Sigs.PropsT with type property := Types.property

type 'a t = (module S with type t = 'a)

(* Create accessors.

   [property_lists] gives the lists of properties that should be inherited
   from. Lists to the left take priority. The leftmost list is considered "own"
   properties.

   When [strict] is true, the resulting property list only includes properties
   that appeared in the rightmost list. *)
val make :
  strict:bool -> property_lists:('a -> Types.property list list) -> 'a t
