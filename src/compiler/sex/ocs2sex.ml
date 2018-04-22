open Sex_types
open Ocs_types

let rec ocs2sex (s:sval) : sexp_t =
  match s with
  | Sunbound
    -> failwith ("unmapped ocs type Sunbound " ^ Ocs_print.string_of_ocs s)
  | Seof
    -> failwith ("unmapped ocs type Seof " ^ Ocs_print.string_of_ocs s)
  | Sreal _
    -> failwith ("unmapped ocs type Sreal " ^ Ocs_print.string_of_ocs s)
  | Schar _
    -> failwith ("unmapped ocs type Schar " ^ Ocs_print.string_of_ocs s)
  | Sport _
    -> failwith ("unmapped ocs type Sport " ^ Ocs_print.string_of_ocs s)
  | Sprim _
    -> failwith ("unmapped ocs type Sprim " ^ Ocs_print.string_of_ocs s)
  | Svalues _
    -> failwith ("unmapped ocs type Svalues " ^ Ocs_print.string_of_ocs s)
  | Sesym _
    -> failwith ("unmapped ocs type Sesym " ^ Ocs_print.string_of_ocs s)
  | Swrapped _
    -> failwith ("unmapped ocs type Swrapped " ^ Ocs_print.string_of_ocs s)
  | Sunspec
    -> failwith ("unmapped ocs type Sunspec " ^ Ocs_print.string_of_ocs s)
  | Spromise _
    -> failwith ("unmapped ocs type Spromise " ^ Ocs_print.string_of_ocs s)
  | Sproc _
    -> failwith ("unmapped ocs type Sproc " ^ Ocs_print.string_of_ocs s)

  | Snull -> Lst []
  | Strue -> Id "true"
  | Sfalse -> Id "false"
  | Sstring s -> Str (Bytes.to_string s)
  | Ssymbol s -> Id s
  | Sint i -> Int (string_of_int i)
  | Spair _ ->
    let svs = Ocs_misc.list_to_caml s in
    let sexs = List.map ocs2sex svs in
    Lst sexs

  | Svector a -> Lst (List.map ocs2sex (Array.to_list a))

