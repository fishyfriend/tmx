module type S = sig
  include module type of Basic

  val run_context : ('a, Context.t) State.t -> 'a
  val read_context : (Context.t -> 'a) -> 'a
end

module Make () : S
