module type S = Sigs.Loader

type t = (module S)

val make : root:string -> t
