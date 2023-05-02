open Tmx__

module Make (Core : Core.S) : sig
  open Core

  val prop : string -> Property.value -> Property.t
  val prop' : string -> string -> Property.value -> Property.t
  val class_ : string -> Class.useas list -> Property.t list -> Customtype.t

  module type PropsT = Sigs.PropsT with type property := Property.t

  type 'a props = (module PropsT with type t = 'a)

  val check_prop : 'a props -> 'a -> string -> Property.Value.t -> unit
  val check_prop' :
    'a props -> 'a -> string -> string -> Property.Value.t -> unit
  val check_no_prop : 'a props -> 'a -> string -> unit
end

module Simple : module type of Make (Core.Simple)
