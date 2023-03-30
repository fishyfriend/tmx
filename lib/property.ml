include Property_intf

module Make (State : State_intf.S) : S = struct
  include Property0

  include
    Properties.Make_strict
      (State)
      (struct
        type nonrec t = t

        let useas = `Property

        let class_ t =
          match value t with `Class _ -> propertytype t | _ -> None

        let properties t = match value t with `Class props -> props | _ -> []
      end)
end
