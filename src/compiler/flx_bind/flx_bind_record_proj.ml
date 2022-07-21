open Flx_btype
open Flx_bexpr

(* This code finds cases where a name is applied to a value,
in which the value has type record, polyrecord, struct, cstruct, 
or pointer to any of these, and the name is a field 
of the record. 

If the name isn't a field it raises TryNext.
If the name isn't monomorphic it also raises TryNext 
because field names can't be polymorphic.
*)

exception Not_field

let handle_field_name state bsym_table build_env env rs be bt cal_apply bind_type' mkenv 
  sr e e2 name ts i ts' isptr
=
  let rt t = Flx_beta.beta_reduce "flx_bind_record_proj: handle_field_name" state.Flx_lookup_state.counter bsym_table sr t in
  let (_,t) as te = be e in
  let ttt = rt t in
  match Flx_lookup_state.hfind "Flx_bind_record_proj:handle_field_name" state.sym_table i with

  (* STRUCT *)
  | { Flx_sym.id=id; sr=sra; symdef=SYMDEF_struct (ls,_) }
  | { Flx_sym.id=id; sr=sra; symdef=SYMDEF_cstruct (ls,_,_) } ->
    let _,vs,_ = Flx_generic.find_split_vs state.sym_table bsym_table i in
    let cidx,ct =
      let rec scan i = function
      | [] -> raise Not_field
      | (vn,vat)::_ when vn = name -> i,vat
      | _:: t -> scan (i+1) t
      in scan 0 ls
    in
    let ct =
      let bvs = List.map
        (fun (n,i,mt) -> n, btyp_type_var (i, Flx_btype.bmt "Flx_bind_record_proj.handle_field_name" mt))
        (vs)
      in
      let env' = build_env state bsym_table (Some i) in
      bind_type' state bsym_table env' Flx_lookup_state.rsground sr ct bvs mkenv
    in
    let vs' = List.map (fun (s,i,tp) -> s,i, Flx_btype.bmt "Flx_dot" tp) vs in
    let ct = Flx_btype_subst.tsubst sr vs' ts' ct in
    let ct = if isptr then btyp_pointer ct else ct in
    (* messy .. we generalised get_n to accept any type instead
       of an integer selector. to replace integer n,
       we have to use case n of m where m is the number of
       cases: n is an int, whereas m is unitsum m' where m'
       is the number of cases.

       Note: bexpr_case is bugged! It can only be used
       for const constructors, the type of the case of T
       is always T. 
    *)
    bexpr_get_n ct cidx te
  | _ ->  raise Not_field



let try_bind_record_proj 
  bsym_table state build_env be bt env rs cal_apply bind_type' mkenv
  f' a' (ea,ta as a) sr name ts 
=
match unfold "flx_lookup" ta with 
(* record  value *)
| BTYP_record (es) ->
  if (ts != []) then raise Flx_exceptions.TryNext;
  let k = List.length es in
  let field_name = name in
  begin match Flx_list.list_index (List.map fst es) field_name with
  | Some n -> 
    let t = List.assoc field_name es in
    let t = bexpr_get_named t name a in
    t
  | None -> 
    raise Flx_exceptions.TryNext
  end

(* polyrecord value *)
| BTYP_polyrecord (es,s,v) ->
  if (ts != []) then raise Flx_exceptions.TryNext; 
  let k = List.length es in
  let field_name = name in
  begin match Flx_list.list_index (List.map fst es) field_name with
  | Some n -> 
    let t = List.assoc field_name es in
    let t = bexpr_get_named t name a in
    t
  | None -> 
    if name = s then begin
    let ss = List.map fst es in
    bexpr_remove_fields a ss
    end 
    else
      raise Flx_exceptions.TryNext
  end


(* pointer to record or polyrecord value *)
| BTYP_ptr (mode,(BTYP_record _ as r),[])
| BTYP_ptr (mode,(BTYP_polyrecord _ as r),[]) ->
  begin match unfold "flx_lookup" r with
  | BTYP_polyrecord (es,_,_) 
  | BTYP_record (es) ->
    if (ts != []) then raise Flx_exceptions.TryNext;
    let k = List.length es in
    let field_name = name in
    begin match Flx_list.list_index (List.map fst es) field_name with
    | Some n -> 
      let t = List.assoc field_name es in
      let t = bexpr_get_named (btyp_ptr mode t []) name a in
      t
    | None -> 
      raise Flx_exceptions.TryNext
    end
  | _ -> assert false (* can't happen due to double pattern match *)
  end

(* Instance type, possibly struct or cstruct.  *)
| BTYP_inst (_,i,ts',_) ->
  begin try
  handle_field_name state bsym_table build_env env rs 
    be bt cal_apply bind_type' mkenv 
    sr a' f' name ts i ts' false
  with Not_field -> raise Flx_exceptions.TryNext
  end

(* pointer to instance *)
| BTYP_ptr (mode,(BTYP_inst (_,i,ts',_) as r),[]) ->
(* NOTE: This may not work, unfold doesn't penetrate into a struct!
However, if the struct is complete but polymorphic, it should work
by unfolding the ts values ..
*)
  begin match unfold "flx_bind_record_proj" r with | BTYP_inst (_,i,ts',_) ->
    begin try
    handle_field_name state bsym_table build_env env rs 
      be bt cal_apply bind_type' mkenv 
      sr a' f' name ts i ts' true 
    with Not_field -> raise Flx_exceptions.TryNext
    end
  | _ -> assert false (* can't happen due to double pattern match *)
  end
| _ -> raise Flx_exceptions.TryNext


