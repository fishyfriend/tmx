type ('a, 'b) t

val return : 'a -> ('a, 'b) t
val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val map : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
val run : ('a, 'b) t -> 'b -> 'a * 'b
val get : unit -> ('a, 'a) t
val set : 'a -> (unit, 'a) t
val read : ('a -> 'b) -> ('b, 'a) t
val update : ('a -> 'a) -> (unit, 'a) t
val map_list : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
val iter_list : ('a -> (unit, 'b) t) -> 'a list -> (unit, 'b) t
val map_option : ('a -> ('b, 'c) t) -> 'a option -> ('b option, 'c) t
val iter_option : ('a -> (unit, 'b) t) -> 'a option -> (unit, 'b) t

module Infix : sig
  val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( >|= ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
end

module Syntax : sig
  include module type of Infix

  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
end
