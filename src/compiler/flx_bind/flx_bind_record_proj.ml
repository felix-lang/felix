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
  sr e e2 name ts i imode ts' pmode_opt 
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
    (* NOTE: viewification is done here BEFORE handling pointer projection caseL
       see notes below for why.
    *)
    let ct = match imode with | `N | `P -> ct | `V -> Flx_btype.viewify_type ct in 
    let ct = match pmode_opt with
      | None -> ct
      | Some pmode ->
        btyp_ptr pmode ct []
    in

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
| BTYP_inst (_,vmode, i,ts',_) ->
  begin try
  handle_field_name state bsym_table build_env env rs 
    be bt cal_apply bind_type' mkenv 
    sr a' f' name ts i vmode ts' None 
  with Not_field -> raise Flx_exceptions.TryNext
  end

(* pointer to instance *)
(* NOTE: if the instance type of a value projection of a view mode nominal type
i.e. a struct is applied, we should get the same result we would if it were a
record which has been viewified, in other words we do a normal projection first,
the viewify the result. Our canonical example is a nominally typed list node:

struct Node { data:int; next: &Node; }

We will write Node' for the viewified type which acts like

struct Node' { data:int; next: &<<Node; }

or maybe

struct Node' { data:int; next: &<<Node'; }

This second form would be required to get fixpoints right! But it isn't clear
we need that (the copy doesn't have to be recursive)

But what do we do with pointers?  We can store a viewified nominal type
in a variable and take a RW pointer to it. Modifying a component changes the
variable value .. it doesn't change the data structure it was copied out of!
So you could happily modify next. In fact, an algorithm doing a search
would in fact want to modify the whole variable! But it could just ignore
the data value and modify next. So a pointer to a view mode instance
can be RW, BUT whilst a  a pointer projection to a component 
RETAINS the pointer mode, the type poinnted at must be viewified.
So for examnple a RW pointer to Node' projected with "next" must
be a RW pointer to a View mode pointer to Node (the type of next
is a RW pointer in Node, but a V pointer in Node'.

In Summary: the mode of a pointer obtained from a pointer projection
remains the same as the source pointer, but the type POINTED AT
must be viewified if the source is View mode.
*)


| BTYP_ptr (pmode,(BTYP_inst (_,vmode, i,ts',_) as r),[]) ->
(* NOTE: This may not work, unfold doesn't penetrate into a struct!
However, if the struct is complete but polymorphic, it should work
by unfolding the ts values ..
*)
  begin match unfold "flx_bind_record_proj" r with | BTYP_inst (_,vmode,i,ts',_) ->
    begin try
    handle_field_name state bsym_table build_env env rs 
      be bt cal_apply bind_type' mkenv 
      sr a' f' name ts i vmode ts' (Some pmode) 
    with Not_field -> raise Flx_exceptions.TryNext
    end
  | _ -> assert false (* can't happen due to double pattern match *)
  end
| _ -> raise Flx_exceptions.TryNext


