type t = [`Invalid_arg of string] [@@deriving eq, ord, show]

type exn += Error of t

(* TODO: friendlier error messages *)

let print_exn exn : string option =
  match exn with Error t -> Some (show t) | _ -> None

let () = Printexc.register_printer print_exn
