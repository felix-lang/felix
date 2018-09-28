
module Drules = Map.Make(struct
  type t = string
  let compare = compare
end)

module DStringSet = Set.Make (
  struct
    type t = string
    let compare = compare
  end)

let string_set_of_list ls =
  List.fold_left (fun set elt -> DStringSet.add elt set) DStringSet.empty ls


