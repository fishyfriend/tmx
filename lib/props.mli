module type S = Sigs.PropsT with type property := Types.property

type 'a t = (module S with type t = 'a)

val make :
  strict:bool -> property_lists:('a -> Types.property list list) -> 'a t
