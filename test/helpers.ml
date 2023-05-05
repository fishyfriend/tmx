module A = Alcotest

open Tmx__

module Make (Core : Core.S) = struct
  open Core

  let prop name value = Property.make ~name ~value ()
  let prop' name pt value = Property.make ~name ~propertytype:pt ~value ()

  let class_ name useas members : Customtype.t =
    let variant = `Class (Class.make ~useas ~members) in
    Customtype.make ~id:1 ~name ~variant

  let check_prop0 (type t) (module T : PropsT with type t = t) (x : t)
      (k : string) (p_exp : Property.t option) : unit =
    let check p = A.(check (option (module Property))) "equal" p_exp p in
    T.get_property k x |> check ;
    T.properties x |> List.find_opt (fun p -> Property.name p = k) |> check

  let check_prop m x k v = check_prop0 m x k (Some (prop k v))
  let check_prop' m x k pt v = check_prop0 m x k (Some (prop' k pt v))
  let check_no_prop m x k = check_prop0 m x k None
end

module Simple = Make (Core.Simple)
