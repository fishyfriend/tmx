module type S = Sigs.PropsT with type property := Property0.t

type 'a t = (module S with type t = 'a)

val make_shallow : ('a -> Property0.t list) -> 'a t
val make_deep : strict:bool -> ('a -> Property0.t list list) -> 'a t
