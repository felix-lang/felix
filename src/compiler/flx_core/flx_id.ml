open Flx_format

type t = string

let of_string id = id

let to_string id = id

let print ppf id = print_string ppf id
