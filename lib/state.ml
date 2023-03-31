include State_intf

module Make () : S = struct
  include State0

  module Monad = struct
    type 'a t = State0.t -> 'a * State0.t

    let return x s = (x, s)
    let bind t f s = t s |> fun (x, s') -> f x s'
    let map t f = bind t @@ fun x -> return (f x)
    let run t s = t s
    let update f s = ((), f s)

    let rec iter_list f xs =
      match xs with
      | [] -> return ()
      | x :: xs -> bind (f x) @@ fun () -> iter_list f xs

    let map_option f o =
      match o with Some x -> map (f x) Option.some | None -> return None

    module Infix = struct
      let ( >>= ) t f = bind t f let ( >|= ) t f = map t f
    end
  end

  let the_state = ref State0.empty

  let read f = f !the_state

  let run m =
    let x, t = Monad.run m !the_state in
    the_state := t ;
    x
end
