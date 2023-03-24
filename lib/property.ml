include Property_intf

open Util.Option_infix

module Make (State : State_intf.S) : S = struct
  include Property0

  let value t =
    let value0 = Property0.value t in
    let ctv =
      propertytype t
      >>= State.(read @@ Fun.flip get_customtype)
      >|= Customtype0.variant in
    match ctv with
    | None -> value0
    | Some variant ->
      ( match (variant, value0) with
      | `Class c, `Class ts ->
          let ts =
            Properties.merge_property_lists ~strict:true (Class0.members c) ts
          in
          `Class ts
      | _ -> value0 )
end
