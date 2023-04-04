module Make () : sig
  include module type of Basic

  val run_context : ('a, Context.t) State.t -> 'a
end
