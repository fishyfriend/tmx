type t = int32 [@@deriving eq, ord]

let argb a r g b =
  let open Int32 in
  let shift x n = of_int x |> min 0xffl |> max 0x0l |> Fun.flip shift_left n in
  let a, r, g, b = (shift a 24, shift r 16, shift g 8, shift b 0) in
  a |> logor r |> logor g |> logor b

let rgb r g b = argb 0xff r g b

let a t = Int32.(shift_right_logical t 24 |> logand 0xffl |> to_int)
let r t = Int32.(shift_right_logical t 16 |> logand 0xffl |> to_int)
let g t = Int32.(shift_right_logical t 8 |> logand 0xffl |> to_int)
let b t = Int32.(logand t 0xffl |> to_int)

let of_int32 t = t
let to_int32 t = t

let cmpt ic = Scanf.bscanf ic "%1x%1x" @@ fun x y -> (x lsl 0x4) lor y
let wrap f s = try f s with _ -> Util.invalid_arg s
let of_string_argb =
  wrap @@ fun s -> Scanf.sscanf s "%_1[#]%r%r%r%r%!" cmpt cmpt cmpt cmpt argb
let of_string_rgb =
  wrap @@ fun s -> Scanf.sscanf s "%_1[#]%r%r%r%!" cmpt cmpt cmpt rgb

let of_string s = try of_string_argb s with _ -> of_string_rgb s
let to_string_argb t = Printf.sprintf "#%08lx" t
let to_string_rgb t = Printf.sprintf "#%06lx" (Int32.logand t 0x00ffffffl)
let to_string t = if a t = 0xff then to_string_rgb t else to_string_argb t

let pp ppf t = Format.pp_print_string ppf (to_string t)
let show t = to_string t

let trans = argb 0 0 0 0
let black = rgb 0 0 0
