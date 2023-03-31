module type S = sig
  include module type of State0

  module Monad : sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val map : 'a t -> ('a -> 'b) -> 'b t
    val run : 'a t -> State0.t -> 'a * State0.t
    val update : (State0.t -> State0.t) -> unit t
    val iter_list : ('a -> unit t) -> 'a list -> unit t
    val map_option : ('a -> 'b t) -> 'a option -> 'b option t

    module Infix : sig
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    end
  end

  val read : (t -> 'a) -> 'a
  val run : 'a Monad.t -> 'a
end

module type Intf = sig
  module type S = S

  module Make () : S
end
