open Format
open Flx_util

let print_string ppf s = fprintf ppf "%S" s

let print_big_int ppf i = pp_print_string ppf (Big_int.string_of_big_int i)

let print_opt p5lt ppf = function
  | Some x -> fprintf ppf "Some %a" p5lt x
  | None -> pp_print_string ppf "None"

let print_items1 ppf = uncurry2  (fprintf ppf "%a")
let print_items2 ppf = uncurry4  (fprintf ppf "%a,@ %a")
let print_items3 ppf = uncurry6  (fprintf ppf "%a,@ %a,@ %a")
let print_items4 ppf = uncurry8  (fprintf ppf "%a,@ %a,@ %a,@ %a")
let print_items5 ppf = uncurry10 (fprintf ppf "%a,@ %a,@ %a,@ %a,@ %a")
let print_items6 ppf = uncurry12 (fprintf ppf "%a,@ %a,@ %a,@ %a,@ %a,@ %a")
let print_items7 ppf = uncurry14 (fprintf ppf "%a,@ %a,@ %a,@ %a,@ %a,@ %a,@ %a")
let print_items8 ppf = uncurry16 (fprintf ppf "%a,@ %a,@ %a,@ %a,@ %a,@ %a,@ %a,@ %a")

let print_tuple0 ppf () = fprintf ppf "()"
let print_tuple1 ppf = curry2  (fprintf ppf "%a" print_items1)
let print_tuple2 ppf = curry4  (fprintf ppf "@[<hv1>(%a@])" print_items2)
let print_tuple3 ppf = curry6  (fprintf ppf "@[<hv1>(%a@])" print_items3)
let print_tuple4 ppf = curry8  (fprintf ppf "@[<hv1>(%a@])" print_items4)
let print_tuple5 ppf = curry10 (fprintf ppf "@[<hv1>(%a@])" print_items5)
let print_tuple6 ppf = curry12 (fprintf ppf "@[<hv1>(%a@])" print_items6)
let print_tuple7 ppf = curry14 (fprintf ppf "@[<hv1>(%a@])" print_items7)
let print_tuple8 ppf = curry16 (fprintf ppf "@[<hv1>(%a@])" print_items8)

(** Helper function to abstract printing variants. *)
let print_variantN print_items ppf name = fprintf ppf "@[<hv1>%s (@,%a@])" name print_items

let print_variant0 ppf name = fprintf ppf "%s" name
let print_variant1 ppf name = curry2  (fprintf ppf "@[<hv1>%s@ %a@]" name print_items1)
let print_variant2 ppf name = curry4  (print_variantN print_items2 ppf name)
let print_variant3 ppf name = curry6  (print_variantN print_items3 ppf name)
let print_variant4 ppf name = curry8  (print_variantN print_items4 ppf name)
let print_variant5 ppf name = curry10 (print_variantN print_items5 ppf name)
let print_variant6 ppf name = curry12 (print_variantN print_items6 ppf name)
let print_variant7 ppf name = curry14 (print_variantN print_items7 ppf name)
let print_variant8 ppf name = curry16 (print_variantN print_items8 ppf name)

let print_record1 ppf = fprintf ppf "@[<hv2>{ %s=%a@] }"
let print_record2 ppf = fprintf ppf "@[<hv2>{ %s=%a;@ %s=%a@] }"
let print_record3 ppf = fprintf ppf "@[<hv2>{ %s=%a;@ %s=%a;@ %s=%a@] }"
let print_record4 ppf = fprintf ppf "@[<hv2>{ %s=%a;@ %s=%a;@ %s=%a;@ %s=%a@] }"
