open Flx_util
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_types
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_label
open Flx_bid
open Flx_btype_subst

module CS = Flx_code_spec

let hfind msg h k =
  try Hashtbl.find h k
  with Not_found ->
    print_endline ("flx_stack_calls Hashtbl.find failed " ^ msg);
    raise Not_found


(* first approximation: we can stack functions that have no
  function or procedure children AND no variables: later
  we will check the return type, for now just check
  the code generator works
*)

(* The Pure property is a bit weird. We consider a function pure
  if it doesn't need a stack frame, and can make do with
  individual variables. This allows the function to be modelled
  with an actual C function.

  A pure function must be top level and cannot have any
  child functions. This means it depends only on its parameters
  and globals -- globals are allowed because we pass the thread
  frame pointer in, even to C functions.

  We assume a non-toplevel function is a child of some other
  function for a reason -- to access that functions environment.
  Still .. we could pass the display in, just as we pass the
  thread frame pointer.

  What we really cannot allow is a child function, since we
  cannot pass IT our frame pointer, since we don't have one.

  Because of this weird notion, we can also mark procedures
  pure under the same conditions, and implement them as
  C functions as well.

  Note neither a function nor procedure can be pure unless
  it is also stackable, and the C function model can't be used
  for either if a heap closure is formed.
*)
let rec is_pure syms bsym_table i =
  let children =
    try Flx_bsym_table.find_children bsym_table i
    with Not_found -> BidSet.empty
  in
  let bsym_parent, bsym = Flx_bsym_table.find_with_parent bsym_table i in

  (* Checking purity *)
  match Flx_bsym.bbdcl bsym with
  | BBDCL_nominal_type_alias _ -> assert false
  | BBDCL_structural_type_alias _ -> assert false
  | BBDCL_instance_type _ -> assert false
  | BBDCL_virtual_type _ -> assert false
  | BBDCL_invalid -> assert false
  | BBDCL_module
  | BBDCL_val _
  | BBDCL_const_ctor _
  | BBDCL_nonconst_ctor _
  | BBDCL_external_code _
  | BBDCL_cstruct _
  | BBDCL_struct _
  | BBDCL_union _
  | BBDCL_external_type _
  | BBDCL_newtype _
  | BBDCL_external_const _
  | BBDCL_typeclass _
  | BBDCL_instance _
  | BBDCL_axiom
  | BBDCL_lemma 
  | BBDCL_reduce  ->
    (*
    print_endline (id ^ " is intrinsically pure");
    *)
    true

  (* not sure about this *)
  | BBDCL_label s -> true

  (* not sure if this is the right place for this check .. *)
  | BBDCL_external_fun (props,_,_,_,_,_,kind) ->
      begin match kind with
      | `Code CS.Virtual -> false
      | _ -> 
        if mem `Pure props then true else 
        if mem `ImPure props then false else true
      end

  | BBDCL_fun (props,_,_,_,_,exes) ->
    if mem `Pure props then true else 
    if mem `ImPure props then false else 
    let bsym_parent = Flx_bsym_table.find_parent bsym_table i in
    match bsym_parent with
    | Some _ ->
      (*
      print_endline (id ^ " is parented so Not pure");
      *)
      false

    | None ->
    try
      BidSet.iter begin fun kid ->
        if not (is_pure syms bsym_table kid)
        then begin
          (*
          print_endline ("Child " ^ si kid ^ " of " ^ id ^ " is not pure");
          *)
          raise Not_found
        end
        (*
        else begin
          print_endline ("Child " ^ si kid ^ " of " ^ id ^ " is pure");
        end
        *)
      end children;

      (*
      print_endline (id ^ " is Pure");
      *)
      true

    with
    | Not_found ->
      (*
      print_endline (id ^ " is checked Not pure");
      *)
      false


exception Found

(* A function is stackable provided it doesn't return
  a pointer to itself. There are only two ways this
  can happen: the function returns the address of
  a variable, or, it returns the closure of a child.

  We will check the return type for pointer or
  function types. If its a function, there
  has to be at least one child to grab our this
  pointer in its display. If its a pointer,
  there has to be either a variable, or any
  non-stackable child function, or any child
  procedure -- note that the pointer might address
  a variable in a child function or procedure,
  however it can't 'get out' of a function except
  by it being returned.

  Proposition: type variables cannot carry either
  pointers to a variable or a child function closure.

  Reason: type variables are all universally quantified
  and unconstrained. We would have v1 = &v2 for the pointer
  case, contrary to the current lack of constraints.
  Smly for functions. So we'll just ignore type variables.

  NOTE: a stacked frame is perfectly viable as a display
  entry -- a heaped child can still refer to a stacked
  parent frame: of course the child must not both persist
  after the frame dies and also refer to that frame.

  This means the display, not just the caller, must be nulled
  out of a routine when it loses control finally. Hmmm .. not
  sure I'm doing that. That means only *explicit* Felix pointers
  in the child refering to the parent frame can hold onto
  the frame. In this case the parent must be heaped if the child
  is, since the parent stacked frame is lost when control is lost.

  NEW: A function returning a pointer with variables in it
  is still pure if none of these variables are actually addressed.
  We should allow this case when the function has no functional
  or procedural children. Actually, even then it is ok if this
  property is also satisfied (along with the others).
*)

let has_var_children bsym_table children =
  try
    BidSet.iter begin fun i ->
      match Flx_bsym_table.find_bbdcl bsym_table i with
      | BBDCL_val (_,_,`Var) -> raise Found
      | _ -> ()
    end children;
    false
  with Found -> true

let has_fun_children bsym_table children =
  try
    BidSet.iter begin fun i ->
      match Flx_bsym_table.find_bbdcl bsym_table i with
      | BBDCL_fun _ -> raise Found
      | _ -> ()
    end children;
   false 
  with Found -> true
 
let has_proc_children bsym_table children =
  try
    BidSet.iter begin fun i ->
      match Flx_bsym_table.find_bbdcl bsym_table i with
      | BBDCL_fun (_,_,_,BTYP_void,_,_) -> raise Found
      | _ -> ()
    end children;
   false 
  with Found -> true


(* NOTE: this won't work for abstracted types like unions
   or structs ..
*)
exception Unsafe

let type_has_fn cache syms bsym_table children t =
  let rec aux limit t =
    if limit = 0 then begin
print_endline "Type has fun reached recursion limit, polymorphic recursion?";
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe
    end
    ;
    let check_components sr vs ts tlist =
      let varmap = mk_varmap sr vs ts in
      begin try
        iter
          (fun t ->
            let t = varmap_subst varmap t in
            aux (limit - 1) t
          )
        tlist;
        Hashtbl.replace cache t `Safe
      with Unsafe ->
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe
      end
    in
    try match Hashtbl.find cache t with
    | `Recurse -> ()
    | `Unsafe -> raise Unsafe
    | `Safe -> ()
    with Not_found ->
      Hashtbl.add cache t `Recurse;
      match t with
      | BTYP_function _ ->
        (* if has_fun bsym_table children then *)
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe

      | BTYP_inst (i,ts,_) ->
        let bsym  = Flx_bsym_table.find bsym_table i in
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_newtype _ -> () (* FIXME *)
        | BBDCL_external_type _ -> ()
        | BBDCL_union (vs,cs)->
          check_components (Flx_bsym.sr bsym) vs ts (map (fun (_,_,evs,d,c,gadt)->d) cs)

        | BBDCL_cstruct (vs,cs,_)
        | BBDCL_struct (vs,cs) ->
          check_components (Flx_bsym.sr bsym) vs ts (map snd cs)

        | _ -> assert false
        end
      | x ->
        try
          Flx_btype.flat_iter ~f_btype:(aux (limit - 1)) x;
          Hashtbl.replace cache t `Safe
        with Unsafe ->
          Hashtbl.replace cache t `Unsafe;
          raise Unsafe

  in try aux 20 t; false with Unsafe -> true

let type_has_ptr cache syms bsym_table children t =
  let rec aux limit t =
    if limit = 0 then begin
print_endline "Type has ptr reached recursion limit, polymorphic recursion?";
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe
    end
    ;
    let check_components sr vs ts tlist =
      let varmap = mk_varmap sr vs ts in
      begin try
        iter
          (fun t ->
            let t = varmap_subst varmap t in
            aux (limit - 1) t
          )
        tlist;
        Hashtbl.replace cache t `Safe
      with Unsafe ->
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe
      end
    in
    try match Hashtbl.find cache t with
    | `Recurse -> ()
    | `Unsafe -> raise Unsafe
    | `Safe -> ()
    with Not_found ->
      Hashtbl.add cache t `Recurse;
      match t with
      | BTYP_pointer _ ->
        (* encode the more lenient condition here!! *)
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe
      | BTYP_inst (i,ts,_) ->
        let bsym = Flx_bsym_table.find bsym_table i in
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_newtype _ -> () (* FIXME *)
        | BBDCL_external_type _ -> ()
        | BBDCL_union (vs,cs)->
          check_components (Flx_bsym.sr bsym) vs ts (map (fun (_,_,evs,d,c,gadt)->d) cs)

        | BBDCL_cstruct (vs,cs,_)
        | BBDCL_struct (vs,cs) ->
          check_components (Flx_bsym.sr bsym) vs ts (map snd cs)

        | _ -> assert false
        end
      | x ->
        try
          Flx_btype.flat_iter ~f_btype:(aux (limit - 1)) x;
          Hashtbl.replace cache t `Safe
        with Unsafe ->
          Hashtbl.replace cache t `Unsafe;
          raise Unsafe

  in try aux 20 t; false with Unsafe -> true

let can_stack_func syms bsym_table fn_cache ptr_cache i =
  let children =
    try Flx_bsym_table.find_children bsym_table i
    with Not_found -> BidSet.empty
  in
  let bsym = Flx_bsym_table.find bsym_table i in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,_,_,ret,_,_) ->
    if mem `Heap_closure props then false else
    let has_vars = has_var_children bsym_table children in
    let has_funs = has_fun_children bsym_table children in
    let returns_fun = type_has_fn fn_cache syms bsym_table children ret in
    let returns_ptr = type_has_ptr ptr_cache syms bsym_table children ret in
    let can_stack = 
      (* if we have a child function and return a function, it might be
       * the child so we cannot stack. If we have variables and return
       * a pointer, it might point at the variable so we cannot stack.
       * Finally, we might have a child function with a local variable
       * which returns its address, and we call it, so we have to
       * exclude the case where there is a child function and we
       * return a pointer as well.
       *
       * Otherwise stacking is safe. This condition is still way too
       * restrictive, but better than the last one. The main problem
       * is tracing if the address of a local variable is returned,
       * which requires data flow analysis.
      *)
      match has_vars, returns_ptr, has_funs, returns_fun with
          | _       , _          , true   , true 
          | _       , true       , true   , _ 
          | true    , true       , _      , _    -> false
          | _ -> true
    in
    can_stack

  | BBDCL_nonconst_ctor _
  | BBDCL_external_fun _
  | BBDCL_cstruct _
  | BBDCL_struct _
    -> false (* hack *)
  | _ -> failwith ("Unexpected non-function " ^ Flx_bsym.id bsym)

exception Unstackable

let rec can_stack_proc
  syms
  bsym_table
  fn_cache
  ptr_cache
  label_info
  i
  recstop
=
  let children =
    try Flx_bsym_table.find_children bsym_table i
    with Not_found -> BidSet.empty
  in
  let bsym = Flx_bsym_table.find bsym_table i in
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_fun (props,_,_,BTYP_fix (0,_),effects,exes) 
  | BBDCL_fun (props,_,_,BTYP_void,effects,exes) ->
    if mem `Heap_closure props then false else
    let is_nonreturn = match bbdcl with 
    | BBDCL_fun (_,_,_,BTYP_fix (0,_),_,_) -> true | _ -> false 
    in
    (* if a procedure has procedural children they can do anything naughty
     * a recursive check would be more aggressive
    *)
    if has_proc_children bsym_table children then false else
    let labels = Flx_label.get_labels_for_proc label_info i in
    begin try iter (fun exe ->
    match exe with

    | BEXE_axiom_check _ -> assert false
    | BEXE_svc _ ->
      begin
        (*
        print_endline (id ^ "Does service call");
        *)
        raise Unstackable
      end
    | BEXE_call (_,(BEXPR_closure (j,_),_),_)
    | BEXE_call_direct (_,j,_,_)

    (* this case needed for virtuals/typeclasses .. *)
    | BEXE_call_prim (_,j,_,_)
      ->
      if not (check_stackable_proc
        syms
        bsym_table
        fn_cache
        ptr_cache
        label_info
        j
        (i::recstop))
      then begin
        (*
        print_endline (id ^ " calls unstackable proc " ^ si j);
        *)
        raise Unstackable
      end

    (* assignments to a local variable are safe *)
    | BEXE_init (_,j,_)
    | BEXE_assign (_,(BEXPR_varname (j,_),_),_)
      when BidSet.mem j children -> ()

    (* assignments not involving pointers or functions are safe *)
    | BEXE_init (sr,_,(_,t))
    | BEXE_assign (sr,(_,t),_)
    | BEXE_storeat (sr,(_,t),_) ->
      if 
        let has_vars = has_var_children bsym_table children in
        let has_funs = has_fun_children bsym_table children in
        let returns_fun = type_has_fn fn_cache syms bsym_table children t in
        let returns_ptr = type_has_ptr ptr_cache syms bsym_table children t in
        let can_stack = 
          (* this is the similar to a function, except we're talking
           * about storing a value in an external variable instead
           * of about returning it. 
          *)
          (*
          let p = function | true -> "true" | false -> "false" in
          print_endline ("has_vars " ^ p has_vars ^ " ret ptr " ^ p returns_ptr
          ^ " has_funs " ^ p has_funs ^ " ret fun " ^ p returns_fun );
          *)
          match has_vars, returns_ptr, has_funs, returns_fun with
              | _       , _          , true   , true 
              | _       , true       , true   , _ 
              | true    , true       , _      , _    -> false
              | _ -> true
        in
        can_stack

      then 
        () 
      else 
        raise  Unstackable


    | BEXE_call _
    | BEXE_call_with_trap _
       ->
       (*
       print_endline (id ^ " does nasty call");
       *)
       raise Unstackable

    | BEXE_jump _
    | BEXE_jump_direct _
       ->
       (*
       print_endline (id ^ " does jump");
       *)
       raise Unstackable

    | BEXE_label (_,idx) ->
       let lkind =
         get_label_kind_from_index label_info.label_usage idx 
       in
       if lkind = `Far then
       begin
         (*
         print_endline (Flx_bsym.id bsym ^ " has non-local label");
         *)
         raise Unstackable
       end

    | BEXE_ifgoto (_,_,idx) 
    | BEXE_goto (_,idx) ->
      begin
        (* NOTE: if we do a goto to a Far label there are two cases:
           its Local or of Nonlocal. If its local, there's no
           objection to a stack. If it's Nonlocal, we have to do a
           far goto to reach it, which prohibits the stack.

           HOWEVER, if the label is marked Far, someone ELSE
           is jumping to it from outside, also prohibiting the stack.
           However saying this now is not useful, since no local
           jumps may go to the label, we have to check all the 
           labels anyhow!

           At this point we don't kow if the label is local.
           The point is, we don't care!
        *)
        match get_label_kind_from_index label_info.label_usage idx with
        | `Far -> raise Unstackable
        | `Unused -> assert false
        | `Near -> ()
      end

    (* A computed goto, without a constraint on targets,
       could go anywhere 
    *)
    | BEXE_cgoto _ -> raise Unstackable
    | BEXE_ifcgoto _ -> raise Unstackable

    | BEXE_yield _
    | BEXE_fun_return _ -> assert is_nonreturn 

    (* Assume these are safe .. ? *)
    | BEXE_code _
    | BEXE_nonreturn_code _

    | BEXE_call_stack _ (* cool *)
    | BEXE_halt _
    | BEXE_trace _
    | BEXE_comment _

    | BEXE_assert _
    | BEXE_assert2 _
    | BEXE_axiom_check2 _
    | BEXE_begin
    | BEXE_end
    | BEXE_nop _
    | BEXE_proc_return _
    | BEXE_try _
    | BEXE_catch _
    | BEXE_endtry _
      -> ()
    )
    exes;
    (*
    print_endline (id ^ " is stackable");
    *)
    true
    with Unstackable ->
      (*
      print_endline (id ^ " cannot be stacked ..");
      *)
      false
    | Not_found ->
      failwith "Not_found error unexpected!"
    end

  | _ -> assert false

and check_stackable_proc
  syms
  bsym_table
  fn_cache
  ptr_cache
  label_info
  i
  recstop
=
  if mem i recstop then true else
  let bsym = Flx_bsym_table.find bsym_table i in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_external_fun (props,_,_,_,_,_,kind) ->
    if mem `Heap_closure props then false else
    begin match kind with
    | `Code ct -> ct <> CS.Virtual
    | `Callback _ ->
        (* not sure if this is right .. *)
        false
    end
  | BBDCL_fun (props,vs,p,ret,effects,exes) ->
    begin match ret with
    | BTYP_void
    | BTYP_fix (0,_) ->
      if mem `Stackable props then true
      else if mem `Unstackable props then false
      else if can_stack_proc syms bsym_table fn_cache ptr_cache label_info  i recstop
      then begin
(*
        print_endline ("MARKING PROCEDURE " ^ Flx_bsym.id bsym ^ " stackable!");
*)
        let props = `Stackable :: props in
        let props =
          if is_pure syms bsym_table i then `Pure :: props else props
        in
        let bbdcl = bbdcl_fun (props,vs,p,ret,effects,exes) in
        Flx_bsym_table.update_bbdcl bsym_table i bbdcl;
        true
      end
      else begin
        let bbdcl = bbdcl_fun (`Unstackable :: props,vs,p,ret,effects,exes) in
        Flx_bsym_table.update_bbdcl bsym_table i bbdcl;
        false
      end
    | _ -> failwith ("[check_stackable_proc] Unexpected function " ^ Flx_bsym.id bsym)
    end
    | _ -> failwith ("[check_stackable_proc] Unexpected non-procedure " ^ Flx_bsym.id bsym)

let ident x = x
let tident t = t

(* this routine NORMALISES applications to one of the forms:
  apply_stack  -- apply on the stack
  apply_direct -- direct application
  apply_prim   -- apply primitive
  apply_struct -- apply struct, cstruct, or nonconst variant type constructor
  apply        -- general apply
*)
let rec enstack_applies syms bsym_table fn_cache ptr_cache x =
  let ea e = enstack_applies syms bsym_table fn_cache ptr_cache e in
  match Flx_bexpr.map ~f_bexpr:ea x with
  | (
       BEXPR_apply ((BEXPR_closure(i,ts),_),b),t
     | BEXPR_apply_direct (i,ts,b),t
    ) as x ->
(*
      print_endline ("Enstack_applies: Found apply closure " ^ sbe bsym_table x);
*)
      begin
        match Flx_bsym_table.find_bbdcl bsym_table i with
        | BBDCL_fun (props,_,_,_,_,_) ->
          if mem `Stackable props
          then bexpr_apply_stack t (i,ts,b)
          else bexpr_apply_direct t (i,ts,b)
        | BBDCL_external_fun _ -> bexpr_apply_prim t (i,ts,b)

        | BBDCL_cstruct _
        | BBDCL_struct _
        | BBDCL_nonconst_ctor  _ -> bexpr_apply_struct t (i,ts,b)
        | _ -> x
      end
  | x 
    -> 
(*
    print_endline ("Enstack_applies: Found other expr " ^ sbe bsym_table x);
*)
    x

let mark_stackable syms bsym_table fn_cache ptr_cache label_info =
  Flx_bsym_table.iter begin fun i _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,p,BTYP_fix (0,_),effects,exes)
    | BBDCL_fun (props,vs,p,BTYP_void,effects,exes) ->
        if mem `Stackable props || mem `Unstackable props then () else
        ignore(check_stackable_proc
          syms
          bsym_table
          fn_cache
          ptr_cache
          label_info
          i
          [])

    | BBDCL_fun (props,vs,p,ret,effects,exes) ->
        let props = ref props in
        if can_stack_func syms bsym_table fn_cache ptr_cache i then
        begin
          props := `Stackable :: !props;
          if is_pure syms bsym_table i then
          begin
            if mem `ImPure !props then
              failwith "flx_stack_calls: Function marked ImPure about to be labelled Pure, why?"
            else
              props := `Pure :: !props;
          end
        end;
        let props = !props in
        let bbdcl = bbdcl_fun (props,vs,p,ret,effects,exes) in
        Flx_bsym_table.update_bbdcl bsym_table i bbdcl

    | _ -> ()
  end bsym_table

let enstack_calls syms bsym_table fn_cache ptr_cache self exes =
(*
print_endline ("Enstack "^string_of_int self ^" exes = \n  " ^ catmap "\n  " (sbx bsym_table) exes);
*)
  let ea e = enstack_applies syms bsym_table fn_cache ptr_cache e in
  let id x = x in
  List.map begin fun exe ->
    let exe = match exe with
    | BEXE_call (sr,(BEXPR_closure (i,ts),_),a)
    | BEXE_call_direct (sr,i,ts,a) ->
        let bsym = Flx_bsym_table.find bsym_table i in
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_fun (props,vs,p,ret,effects,exes) ->
          begin match ret with
          | BTYP_void
          | BTYP_fix (0,_) ->
            if mem `Stackable props then begin
              if not (mem `Stack_closure props) then begin
                let props = `Stack_closure :: props in
                let bbdcl = bbdcl_fun (props,vs,p,ret,effects,exes) in
                Flx_bsym_table.update_bbdcl bsym_table i bbdcl
              end;

              bexe_call_stack (Flx_bsym.sr bsym,i,ts,a)
            end else
              bexe_call_direct (Flx_bsym.sr bsym,i,ts,a)
          | _ ->
            syserr sr ("[enstack calls] Call to function " ^ Flx_bsym.id bsym ^ "<" ^
              string_of_bid i ^ ">")
          end
        (* seems to work at the moment *)
        | BBDCL_external_fun (_,_,_,_,_,_,`Callback _) ->
            bexe_call_direct (Flx_bsym.sr bsym,i,ts,a)

        | BBDCL_external_fun (_,_,_,Flx_btype.BTYP_fix (0,_),_,_,_)
        | BBDCL_external_fun (_,_,_,Flx_btype.BTYP_void,_,_,_) ->
            bexe_call_prim (Flx_bsym.sr bsym,i,ts,a)

        | _ ->
            syserr sr ("Call to non-procedure " ^ Flx_bsym.id bsym ^ "<" ^
              string_of_bid i ^ ">")
        end

    | x -> x
    in
    Flx_bexe.map ~f_bexpr:ea exe
  end exes

let make_stack_calls
  syms
  bsym_table
  label_info
=
(*
print_endline ("Calculating stack calls\n");
*)
  let fn_cache, ptr_cache = Hashtbl.create 97 , Hashtbl.create 97 in
  let ea e = enstack_applies syms bsym_table fn_cache ptr_cache e in

  mark_stackable
    syms
    bsym_table
    fn_cache
    ptr_cache
    label_info;


  Flx_bsym_table.iter begin fun i _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,p,ret,effects,exes) ->
        let exes = enstack_calls
          syms
          bsym_table
          fn_cache
          ptr_cache
          i
          exes
        in
        let exes = Flx_cflow.final_tailcall_opt exes in
        begin match Flx_bsym_table.find_bbdcl bsym_table i with
        | BBDCL_fun (props,vs,p,ret,effects,_) ->
            let bbdcl = bbdcl_fun (props,vs,p,ret,effects,exes) in
            Flx_bsym_table.update_bbdcl bsym_table i bbdcl
        | _ -> assert false
        end

    | _ -> ()
  end bsym_table

