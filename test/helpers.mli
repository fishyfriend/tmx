open Tmx__

module Make (Core : Core.S) : sig
  open Core

  val prop : string -> Property.value -> Property.t
  val prop' : string -> string -> Property.value -> Property.t
  val class_ : string -> Class.useas list -> Property.t list -> Customtype.t

  val check_prop :
    (module PropsT with type t = 'a) ->
    'a ->
    string ->
    Property.Value.t ->
    unit

  val check_prop' :
    (module PropsT with type t = 'a) ->
    'a ->
    string ->
    string ->
    Property.Value.t ->
    unit

  val check_no_prop : (module PropsT with type t = 'a) -> 'a -> string -> unit
end

module Simple : module type of Make (Core.Simple)
