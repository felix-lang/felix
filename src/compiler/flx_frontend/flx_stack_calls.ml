open Flx_util
open Flx_ast
open Flx_types
open Flx_print
open Flx_set
open Flx_types
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open List
open Flx_unify
open Flx_treg
open Flx_generic
open Flx_maps
open Flx_exceptions

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
let rec is_pure syms (child_map, bbdfns) i =
  let children = try Hashtbl.find child_map i with Not_found -> [] in
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  (*
  print_endline ("Checking purity of " ^ id ^ "<" ^ si i ^ ">");
  *)
  match entry with
  | BBDCL_var _
  | BBDCL_ref _
  | BBDCL_val _
  | BBDCL_tmp _
  | BBDCL_nonconst_ctor _
  | BBDCL_callback _
  | BBDCL_insert _
  | BBDCL_cstruct _
  | BBDCL_struct _
  | BBDCL_union _
  | BBDCL_abs _
  | BBDCL_newtype _
  | BBDCL_const _
  | BBDCL_typeclass _
  | BBDCL_instance _
    ->
    (*
    print_endline (id ^ " is intrinsically pure");
    *)
    true

  (* not sure if this is the right place for this check .. *)
  | BBDCL_fun (_,_,_,_,ct,_,_)
  | BBDCL_proc (_,_,_,ct,_) ->
    ct <> CS_virtual

  | BBDCL_procedure (_,_,_,exes)   (* ALLOWED NOW *)
  | BBDCL_function (_,_,_,_,exes) ->
    match parent with
    | Some _ ->
      (*
      print_endline (id ^ " is parented so Not pure");
      *)
      false

    | None ->
    try
      iter (fun kid ->
        if not (is_pure syms (child_map, bbdfns) kid)
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
      )
      children
      ;
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

let has_var_children bbdfns children =
  try
    iter
    (fun i ->
      let id,parent,sr,entry = hfind "has_var_children" bbdfns i in
      match entry with
      | BBDCL_var _  -> raise Found
      | _ -> ()
    )
    children
    ;
    false
  with Found -> true

let has_fun_children bbdfns children =
  try
    iter
    (fun i ->
      let id,parent,sr,entry = hfind "has_fun_children" bbdfns i in
      match entry with
      | BBDCL_procedure _
      | BBDCL_function _ -> raise Found
      | _ -> ()
    )
    children
    ;
   false 
  with Found -> true
 
let has_proc_children bbdfns children =
  try
    iter
    (fun i -> 
      let id,parent,sr,entry = hfind "has_proc_children" bbdfns i in
      match entry with
      | BBDCL_procedure _ -> raise Found
      | _ -> ()
    )
    children
    ;
   false 
  with Found -> true



(* NOTE: this won't work for abstracted types like unions
   or structs ..
*)
exception Unsafe

let type_has_fn cache syms bbdfns children t =
  let rec aux t =
    let check_components vs ts tlist =
      let varmap = mk_varmap vs ts in
      begin try
        iter
          (fun t ->
            let t = varmap_subst varmap t in
            aux t
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
      | `BTYP_function _ ->
        (* if has_fun bbdfns children then *)
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe

      | `BTYP_inst (i,ts) ->
        let id,parent,sr,entry = hfind "type_has_fun: inst" bbdfns i in
        begin match entry with
        | BBDCL_newtype _ -> () (* FIXME *)
        | BBDCL_abs _ -> ()
        | BBDCL_union (vs,cs)->
          check_components vs ts (map (fun (_,_,t)->t) cs)

        | BBDCL_cstruct (vs,cs)
        | BBDCL_struct (vs,cs) ->
          check_components vs ts (map snd cs)

        | _ -> assert false
        end
      | x ->
        try
          iter_btype aux x;
          Hashtbl.replace cache t `Safe
        with Unsafe ->
          Hashtbl.replace cache t `Unsafe;
          raise Unsafe

  in try aux t; false with Unsafe -> true

let type_has_ptr cache syms bbdfns children t =
  let rec aux t =
    let check_components vs ts tlist =
      let varmap = mk_varmap vs ts in
      begin try
        iter
          (fun t ->
            let t = varmap_subst varmap t in
            aux t
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
      | `BTYP_pointer _ ->
        (* encode the more lenient condition here!! *)
        Hashtbl.replace cache t `Unsafe;
        raise Unsafe
      | `BTYP_inst (i,ts) ->
        let id,parent,sr,entry = hfind "type_has_ptr: inst" bbdfns i in
        begin match entry with
        | BBDCL_newtype _ -> () (* FIXME *)
        | BBDCL_abs _ -> ()
        | BBDCL_union (vs,cs)->
          check_components vs ts (map (fun (_,_,t)->t) cs)

        | BBDCL_cstruct (vs,cs)
        | BBDCL_struct (vs,cs) ->
          check_components vs ts (map snd cs)

        | _ -> assert false
        end
      | x ->
        try
          iter_btype aux x;
          Hashtbl.replace cache t `Safe
        with Unsafe ->
          Hashtbl.replace cache t `Unsafe;
          raise Unsafe

  in try aux t; false with Unsafe -> true

let can_stack_func fn_cache ptr_cache syms (child_map,bbdfns) i =
  let children = try Hashtbl.find child_map i with Not_found -> [] in
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  match entry with
  | BBDCL_function (_,_,_,ret,_) ->
    let has_vars = has_var_children bbdfns children in
    let has_funs = has_fun_children bbdfns children in
    let returns_fun = type_has_fn fn_cache syms bbdfns children ret in
    let returns_ptr = type_has_ptr ptr_cache syms bbdfns children ret in
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
  | BBDCL_fun _
  | BBDCL_callback _
  | BBDCL_cstruct _
  | BBDCL_struct _
    -> false (* hack *)
  | _ -> failwith ("Unexpected non-function " ^ id)

exception Unstackable

let rec can_stack_proc fn_cache ptr_cache syms (child_map,bbdfns) label_map label_usage i recstop =
  let children = try Hashtbl.find child_map i with Not_found -> [] in
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  (*
  print_endline ("Stackability Checking procedure " ^ id);
  *)
  match entry with
  | BBDCL_procedure (_,_,_,exes) ->
    (* if a procedure has procedural children they can do anything naughty
     * a recursive check would be more aggressive
    *)
    if has_proc_children bbdfns children then false else 
    let labels = hfind "label_map" label_map i in
    begin try iter (fun exe ->
    (*
    print_endline (string_of_bexe syms.dfns 0 exe);
    *)
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
      if not (check_stackable_proc fn_cache ptr_cache syms (child_map,bbdfns) label_map label_usage j (i::recstop))
      then begin
        (*
        print_endline (id ^ " calls unstackable proc " ^ si j);
        *)
        raise Unstackable
      end

    (* assignments to a local variable are safe *)
    | BEXE_init (_,j,_)
    | BEXE_assign (_,(BEXPR_name (j,_),_),_)
      when mem j children -> ()

    (* assignments not involving pointers or functions are safe *)
    | BEXE_init (sr,_,(_,t))
    | BEXE_assign (sr,(_,t),_)
      when 
        let has_vars = has_var_children bbdfns children in
        let has_funs = has_fun_children bbdfns children in
        let returns_fun = type_has_fn fn_cache syms bbdfns children t in
        let returns_ptr = type_has_ptr ptr_cache syms bbdfns children t in
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

      -> ()

    | BEXE_init _
    | BEXE_assign _ ->
      (*
      print_endline (id ^ " does foreign init/assignment");
      *)
      raise Unstackable

    | BEXE_call _
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
    | BEXE_loop _
       ->
       (*
       print_endline (id ^ " has loop?");
       *)
       raise Unstackable

    | BEXE_label (_,s) ->
       let  lno = hfind "labels" labels s in
       let lkind =
         Flx_label.get_label_kind_from_index label_usage lno
       in
       if lkind = `Far then
       begin
         print_endline (id ^ " has non-local label");
         raise Unstackable
       end

    | BEXE_goto (_,s) ->
      begin
        match Flx_label.find_label bbdfns label_map i s with
        | `Nonlocal _ ->
          (*
          print_endline (id ^ " does non-local goto");
          *)
          raise Unstackable
        | `Unreachable -> assert false
        | `Local _ -> ()
      end

    | BEXE_yield _
    | BEXE_fun_return _ -> assert false

    (* Assume these are safe .. ? *)
    | BEXE_code _
    | BEXE_nonreturn_code _

    | BEXE_call_stack _ (* cool *)
    | BEXE_halt _
    | BEXE_trace _
    | BEXE_comment _
    | BEXE_ifgoto _
    | BEXE_assert _
    | BEXE_assert2 _
    | BEXE_begin
    | BEXE_end
    | BEXE_nop _
    | BEXE_proc_return _
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

and check_stackable_proc fn_cache ptr_cache syms (child_map,bbdfns) label_map label_usage i recstop =
  if mem i recstop then true else
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  match entry with
  | BBDCL_callback _ -> false (* not sure if this is right .. *)
  | BBDCL_proc (_,_,_,ct,_) -> ct <> CS_virtual
  | BBDCL_procedure (props,vs,p,exes) ->
    if mem `Stackable props then true
    else if mem `Unstackable props then false
    else if can_stack_proc fn_cache ptr_cache syms (child_map,bbdfns) label_map label_usage i recstop
    then begin
      (*
      print_endline ("MARKING PROCEDURE " ^ id ^ " stackable!");
      *)
      let props = `Stackable :: props in
      let props =
        if is_pure syms (child_map,bbdfns) i then `Pure :: props else props
      in
      let entry : bbdcl_t = BBDCL_procedure (props,vs,p,exes) in
      Hashtbl.replace bbdfns i (id,parent,sr,entry);
      true
    end
    else begin
      let entry : bbdcl_t = BBDCL_procedure (`Unstackable :: props,vs,p,exes) in
      Hashtbl.replace bbdfns i (id,parent,sr,entry);
      false
    end
  | _ -> failwith ("Unexpected non-procedure " ^ id)
    (*
    assert false
    *)

let ident x = x
let tident t = t

(* this routine NORMALISES applications to one of the forms:
  apply_stack  -- apply on the stack
  apply_direct -- direct application
  apply_prim   -- apply primitive
  apply_struct -- apply struct, cstruct, or nonconst variant type constructor
  apply        -- general apply
*)
let rec enstack_applies fn_cache ptr_cache syms (child_map, bbdfns) x =
  let ea e = enstack_applies fn_cache ptr_cache syms (child_map, bbdfns) e in
  match map_tbexpr ident ea tident x with
  | (
       BEXPR_apply ((BEXPR_closure(i,ts),_),b),t
     | BEXPR_apply_direct (i,ts,b),t
    ) as x ->
      begin
        let _,_,_,entry = Hashtbl.find bbdfns i in
        match entry with
        | BBDCL_function (props,_,_,_,_) ->
          if mem `Stackable props
          then BEXPR_apply_stack (i,ts,b),t
          else BEXPR_apply_direct (i,ts,b),t
        | BBDCL_fun _
        | BBDCL_callback _ ->
          BEXPR_apply_prim(i,ts,b),t

        | BBDCL_cstruct _
        | BBDCL_struct _
        | BBDCL_nonconst_ctor  _ ->
          BEXPR_apply_struct(i,ts,b),t
        | _ -> x
      end
  | x -> x

let mark_stackable fn_cache ptr_cache syms (child_map,bbdfns) label_map label_usage =
  Hashtbl.iter
  (fun i (id,parent,sr,entry) ->
    match entry with
    | BBDCL_function (props,vs,p,ret,exes) ->
      let props: property_t list ref = ref props in
      if can_stack_func fn_cache ptr_cache syms (child_map,bbdfns) i then
      begin
        props := `Stackable :: !props;
        if is_pure syms (child_map,bbdfns) i then
        begin
          (*
          print_endline ("Function " ^ id ^ "<" ^ si i ^ "> is PURE");
          *)
          props := `Pure :: !props;
        end
        (*
        else
          print_endline ("Stackable Function " ^ id ^ "<" ^ si i ^ "> is NOT PURE")
        *)
      end
      (*
      else print_endline ("Function " ^ id ^ "<" ^ si i ^ "> is NOT STACKABLE")
      *)
      ;
      let props : property_t list = !props in
      let entry : bbdcl_t = BBDCL_function (props,vs,p,ret,exes) in
      Hashtbl.replace bbdfns i (id,parent,sr,entry)

    | BBDCL_procedure (props,vs,p,exes) ->
      if mem `Stackable props or mem `Unstackable props then ()
      else ignore(check_stackable_proc fn_cache ptr_cache syms (child_map,bbdfns) label_map label_usage i [])
    | _ -> ()
  )
  bbdfns

let enstack_calls fn_cache ptr_cache syms (child_map,bbdfns) self exes =
  let ea e = enstack_applies fn_cache ptr_cache syms (child_map, bbdfns) e in
  let id x = x in
  let exes =
    map (
      fun exe -> let exe = match exe with
      | BEXE_call (sr,(BEXPR_closure (i,ts),_),a)
      | BEXE_call_direct (sr,i,ts,a) ->
        let id,parent,sr,entry = Hashtbl.find bbdfns i in
        begin match entry with
        | BBDCL_procedure (props,vs,p,exes) ->
          if mem `Stackable props then
          begin
            if not (mem `Stack_closure props) then
              Hashtbl.replace bbdfns i (id,parent,sr,BBDCL_procedure (`Stack_closure::props,vs,p,exes))
            ;
            BEXE_call_stack (sr,i,ts,a)
          end
          else
          BEXE_call_direct (sr,i,ts,a)

        | BBDCL_proc _ -> BEXE_call_prim (sr,i,ts,a)

        (* seems to work at the moment *)
        | BBDCL_callback _ -> BEXE_call_direct (sr,i,ts,a)

        | _ -> syserr sr ("Call to non-procedure " ^ id ^ "<" ^ si i ^ ">")
        end

      | x -> x
      in
        map_bexe id ea id id id exe
    )
    exes
  in
  exes

let make_stack_calls syms (child_map, (bbdfns: fully_bound_symbol_table_t)) label_map label_usage =
  let fn_cache, ptr_cache = Hashtbl.create 97 , Hashtbl.create 97 in
  let ea e = enstack_applies fn_cache ptr_cache syms (child_map, bbdfns) e in
  mark_stackable fn_cache ptr_cache syms (child_map,bbdfns) label_map label_usage;
  Hashtbl.iter
  (fun i (id,parent,sr,entry) -> match entry with
    | BBDCL_procedure (props,vs,p,exes) ->
      let exes = enstack_calls fn_cache ptr_cache syms (child_map,bbdfns) i exes in
      let exes = Flx_cflow.final_tailcall_opt exes in
      let id,parent,sr,entry = Hashtbl.find bbdfns i in
      begin match entry with
      | BBDCL_procedure (props,vs,p,_) ->
        Hashtbl.replace bbdfns i (id,parent,sr,BBDCL_procedure (props,vs,p,exes))
      | _ -> assert false
      end

    | BBDCL_function (props,vs,p,ret,exes) ->
      let exes = enstack_calls fn_cache ptr_cache syms (child_map,bbdfns) i exes in
      let id,parent,sr,entry = Hashtbl.find bbdfns i in
      begin match entry with
      | BBDCL_function (props,vs,p,ret,_) ->
        Hashtbl.replace bbdfns i (id,parent,sr,BBDCL_function (props,vs,p,ret,exes))
      | _ -> assert false
      end

    | _ -> ()
  )
  bbdfns
