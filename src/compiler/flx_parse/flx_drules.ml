
module Drules = Map.Make(struct
  type t = string
  let compare = compare
end)
