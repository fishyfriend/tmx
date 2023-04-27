module Getters = struct
  let get_tileset _ = None
  let get_template _ = None
  let get_customtypes _ = []
  let get_map _ = None
  let get_file _ = None
  let get_tile _ = None
end

include Core.Make (Getters)
