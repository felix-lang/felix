
open Flx_util
open Flx_list
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bbdcl
open Flx_mtypes2
open Flx_name
open Flx_unify
open Flx_typing
open List
open Flx_print
open Flx_exceptions
open Flx_maps
open Flx_gen_shape

let find_references syms bsym_table index ts =
  let children =
    try
      Flx_bsym_table.find_children bsym_table index
    with Not_found -> Flx_types.BidSet.empty
  in
  let references = ref [] in

  Flx_types.BidSet.iter begin fun idx ->
    try
      let bsym = Flx_bsym_table.find bsym_table idx in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_val (vs,t,(`Val | `Var | `Ref)) ->
          if length ts <> length vs then begin
            failwith
            (
              "[find_references] entry " ^ string_of_bid index ^
              ", child " ^ Flx_bsym.id bsym ^ "<" ^ string_of_bid idx ^ ">" ^
              ", wrong number of args, expected vs = " ^
              si (length vs) ^
              ", got ts=" ^
              si (length ts)
            )
          end;
          let t = tsubst vs ts t in
          references := (idx,t) :: !references
      | _ -> ()
    with Not_found -> ()
  end children;

  !references


let gen_fun_offsets s syms bsym_table index vs ps ret ts instance props last_ptr_map : unit =
  let vars =  (find_references syms bsym_table index ts) in
  let vars = filter (fun (i, _) -> is_instantiated syms i ts) vars in
  let name = cpp_instance_name syms bsym_table index ts in
  let display = Flx_display.get_display_list bsym_table index in
  let offsets =
    (if mem `Requires_ptf props then
    ["FLX_EAT_PTF(offsetof(" ^ name ^ ",ptf)comma)"]
    else []
    )
    @
    (match ret with
      | BTYP_void -> [ ("offsetof(" ^ name ^ ",p_svc),");("offsetof(" ^ name ^ ",_caller),")    ]
      | _ -> []
    )
    @
    map
    (fun (didx, vslen) ->
    let dptr = "ptr" ^ cpp_instance_name syms bsym_table didx (list_prefix ts vslen) in
    "offsetof("^name^","^dptr^"),"
    )
    display
    @
    concat
    (
      map
      (fun (idx,typ)->
        let mem = cpp_instance_name syms bsym_table idx ts in
        let offsets = get_offsets syms bsym_table typ in
        map
        (fun offset ->
          "offsetof("^name^","^mem^")+" ^ offset
        )
        offsets
      )
      vars
    )
  in
  let n = length offsets in
  bcat s
  (
    "\n//OFFSETS for "^
    (match ret with BTYP_void -> "procedure " | _ -> "function ") ^
    name ^ "\n"
  );
  gen_offset_data s n name offsets true props None last_ptr_map


