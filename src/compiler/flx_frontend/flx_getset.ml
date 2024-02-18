open Flx_bsym
open Flx_bsym_table
open Flx_bbdcl
open Flx_bid
open Flx_bexe
open Flx_bexpr
open Flx_btype
open Flx_fairy

let debug = 
  try let _ = Sys.getenv "FLX_COMPILER_DEBUG_UNIQ" in true  
  with Not_found -> false 

let show_uniq_getset_only = 
  try let _ = Sys.getenv "FLX_COMPILER_DEBUG_UNIQ_GETSET" in true  
  with Not_found -> false 

let show_share_getset_only = 
  try let _ = Sys.getenv "FLX_COMPILER_DEBUG_SHARE_GETSET" in true  
  with Not_found -> false 



let show_uniq_getset = debug || show_uniq_getset_only
let show_share_getset = debug || show_share_getset_only
 
(* This routine finds all the indexes of uniq expressions .. well
at the moment this is just uniq variables and constant projections
of variables to uniq components
*)

exception DuplicateGet of int * path_t

(* NOTE: this only checks if the domain of a function is a single parameter
of borrowed type, it does not work if the domain is a tuple with borrowed
components. The problem here is that the argument could be a tuple too,
and then we have to examine projections.

This creates problems, because could be passing an argument expression to
a *component* of a tuple. So if the expression is a unique variable it is
moved into the tuple, killing it, before we can check if the corresponding
parameter is a borrow. This CAN work, if the argument is explicitly a tuple.

It probably cannot work .. and should not .. if we first construct a tuple,
because that really IS moving the variable. For example

var x = box 1;
f (x,1);

should work, provided f's first parameter has borrowed type. But this
will fail:

var a = (x,1);
f a;

Here, x is killed by constructing the tuple a. However the function
call should still work and leave the first component of a live.

This should be handled by fairy variables but it requires unpacking
the parameter type as well as the argument type.
*)
let domain_is_borrow bsym_table idx = 
  try
    let bsym = Flx_bsym_table.find bsym_table idx in
    begin match bsym.bbdcl with
    | BBDCL_fun (_,_,params,_,_,_)  -> 
     begin match Flx_bparams.get_btype params with
     | BTYP_borrowed _ -> true
     | _ -> false
     end
    | BBDCL_external_fun (_,_,params, _,_,_,_) ->
      begin match params with
      | [BTYP_borrowed _] -> true
      | _ -> false
      end
    | _ -> failwith ("Flx_getset: expected " ^ string_of_int idx ^ " to be function")
    end
  with
    Not_found -> failwith ("Flx_getset: can't find index " ^ string_of_int idx ^ " in symbol table")
 

let rec find_once bsym_table (chain2ix:chain2ix_t) path (b:BidSet.t ref) (_,t as e) : unit =
(*
print_endline ("Find once for expresssion " ^ Flx_print.sbe bsym_table e ^ ", type = " ^ Flx_print.sbt bsym_table t);
*)
  match e with
  | BEXPR_varname (i,_),_ -> 
    let prefix = List.rev path in
    List.iter  (fun ((j,path),ix) ->
      if j = i then
        if Flx_list.has_prefix prefix path then 
          begin
(*
print_endline (" ** Found variable " ^ Flx_print.sbe bsym_table e );
*)
            if BidSet.mem ix !b then raise (DuplicateGet (i,path))
            else b := BidSet.add ix !b
          end
      )
    chain2ix

  | BEXPR_apply ( (BEXPR_prj (n,_,_),_), base ),_ ->
    let path = `Tup n :: path in
    find_once bsym_table chain2ix path b base 

  (* This will ONLY work correctly if coercions on tuples have
     been expanded ...
  | BEXPR_coerce ((_,BTYP_uniq _) _), BTYP_borrowed _),_ 
  *)
  | BEXPR_coerce ((_,BTYP_uniq t1), BTYP_borrowed t2),_  
    when t1 = t2
    ->  
    begin
      if t1 = t2 then
        print_endline ("Skipping expression type " ^Flx_print.sbt bsym_table t1 ^ " coerced to borrowed: " ^ Flx_print.sbe bsym_table e)
    else
        print_endline ("Skipping WEIRD expression coerced to borrowed or shared")
    end;
    ()
(*
  | BEXPR_coerce ((_,BTYP_uniq t1), t2),_  
    when t1 = t2
    -> 
    begin
      if t1 = t2 then
        print_endline ("Skipping expression type uniq " ^Flx_print.sbt bsym_table t1 ^ " coerced to shared: " ^ Flx_print.sbe bsym_table e)
    else
        print_endline ("Skipping WEIRD expression coerced to borrowed or shared")
    end;
    ()
*)
(* 
  | BEXPR_apply_prim (i,_,(_,argt)),_
  | BEXPR_apply_stack (i,_,(_,argt)),_
  | BEXPR_apply_direct (i,_,(_,argt)),_
  | BEXPR_apply ((BEXPR_closure (i,_),_),(_,argt)),_ when domain_is_borrow bsym_table i ->
    (* print_endline ("Function with domain of type borrow t, argument of type uniq would be borrowed"); *)
    ()
*)

  | x -> 
    Flx_bexpr.flat_iter ~f_bexpr:(find_once bsym_table chain2ix path b) x

exception DuplicateSet of int * path_t

let rec find_ponce bsym_table (chain2ix:chain2ix_t) path (b:BidSet.t ref) e : unit =
(*
print_endline ("Find pointers to once for expresssion " ^ Flx_print.sbe bsym_table e);
*)
  match e with
  | BEXPR_wref (i,_),_  
  | BEXPR_ref (i,_),_ -> 
    let prefix = List.rev path in
    List.iter  (fun ((j,path),ix) ->
      if j = i then
        if Flx_list.has_prefix prefix path then 
          begin
            if BidSet.mem ix !b then raise (DuplicateSet (i,path))
            else b := BidSet.add ix !b;
          end
      )
    chain2ix

  | BEXPR_apply ( (BEXPR_prj (n,_,_),_), base ),_ ->
    let path = `Tup n :: path in
    find_ponce bsym_table chain2ix path b base 

  | x -> Flx_bexpr.flat_iter ~f_bexpr:(find_ponce bsym_table chain2ix path b) x

let rec find_share bsym_table (chain2ix:chain2ix_t) path (b:BidSet.t ref) e : unit =
(*
print_endline ("Find once for expresssion " ^ Flx_print.sbe bsym_table e);
*)
  match e with
  | BEXPR_varname (i,_),_ -> 
    let prefix = List.rev path in
    List.iter  (fun ((j,path),ix) ->
      if j = i then
        if Flx_list.has_prefix prefix path then 
          b := BidSet.add ix !b
      )
    chain2ix

  (* the guard is required because prj projections are used for structs and cstructs too 
     but we don't split these into fairies at the moment
  *)
  | BEXPR_apply ( (BEXPR_prj (n,d,_),_), base ),_ when (match d with | BTYP_inst _ -> false  | _ -> true) ->
    let path = `Tup n :: path in
    find_share bsym_table chain2ix path b base 

  | BEXPR_apply ( (BEXPR_rprj (n,_,d,_),_), base ),_  ->
    let path = `Rec n :: path in
    find_share bsym_table chain2ix path b base 


  | x -> Flx_bexpr.flat_iter ~f_bexpr:(find_share bsym_table chain2ix path b) x


let rec find_pshare bsym_table (chain2ix:chain2ix_t) path (b:BidSet.t ref) e : unit =
  match e with
  | BEXPR_wref (i,_),_  
  | BEXPR_ref (i,_),_ -> 
    let prefix = List.rev path in
    List.iter  (fun ((j,path),ix) ->
      if j = i then
        if Flx_list.has_prefix prefix path then 
          b := BidSet.add ix !b;
      )
    chain2ix

  (* the guard is required because prj projections are used for structs and cstructs too 
     but we don't split these into fairies at the moment
  *)
  | BEXPR_apply ( (BEXPR_prj (n,BTYP_ptr(`RW,d,_),_),_), base ),_ 
  | BEXPR_apply ( (BEXPR_prj (n,BTYP_ptr(`W,d,_),_),_), base ),_ 
    when (match d with | BTYP_inst _ -> false  | _ -> true) ->
    let path = `Tup n :: path in
    find_pshare bsym_table chain2ix path b base 

  (* Note: doesn't correctly account for duplicate fields, ignores index ..
     the `Rec string constructor is inadequate ..
   *)
  | BEXPR_apply ( (BEXPR_rprj (n,_,BTYP_ptr (`RW,d,_),_),_), base ),_
  | BEXPR_apply ( (BEXPR_rprj (n,_,BTYP_ptr (`W,d,_),_),_), base ),_ ->
    let path = `Rec n :: path in
    find_pshare bsym_table chain2ix path b base 

  | x -> Flx_bexpr.flat_iter ~f_bexpr:(find_pshare bsym_table chain2ix path b) x



(* Get and Set detectors for instructions *)

let once_get_sets bsym_table chain2ix ix2chain bexe =
  let bidset = ref BidSet.empty in
  let f_bexpr e = 
    try find_once bsym_table chain2ix [] bidset e 
    with DuplicateGet (i,ix) ->
      print_endline ("Flx_once: Duplicate Get " ^ id_of_index bsym_table i ^" in "^Flx_print.sbe bsym_table e);
      print_endline ("Instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe); 
      let sr = Flx_bexe.get_srcref bexe in
      print_endline (Flx_srcref.long_string_of_src sr);
      failwith ("Flx_once: Duplicate Get of unique variable")
  in
  begin match bexe with 
  | BEXE_assign (_,i,(_,vt)) 
  | BEXE_init (_,i,(_,vt)) -> f_bexpr (bexpr_varname vt (i,[]))
  | BEXE_storeat (_,l,r) -> 
    begin try
      find_ponce bsym_table chain2ix [] bidset l
    with DuplicateSet (i,ix) ->
      print_endline ("Flx_once: Duplicate Set " ^ id_of_index bsym_table i ^" in "^ Flx_print.string_of_bexe bsym_table 0 bexe);
      print_endline ("Instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe); 
      let sr = Flx_bexe.get_srcref bexe in
      print_endline (Flx_srcref.long_string_of_src sr);
      failwith ("Flx_once: Duplicate Set of unique variable")
    end
  | _ ->  () 
  end;

  if show_uniq_getset && not (BidSet.is_empty (!bidset)) then
    print_endline ("  UNIQ SETS: " ^ string_of_vars bsym_table ix2chain (!bidset));
  !bidset

(* In this routine ALL W or RW address taking is counted as a write.
The reason is we have cases in the library in which a procedure closure is formed
in an expression by a function application, which captures a RW pointer
that is later called, causing a write: in particular, unravelling the read
primitive seems to do this. 

We need to do something better, but for now there is no choice if the existing
code is to pass.
*)
let share_get_sets bsym_table chain2ix ix2chain bexe =
  let bidset = ref BidSet.empty in
  let f_bexpr e = 
    find_pshare bsym_table chain2ix [] bidset e 
  in
  (* HACK because i couldn't figure the types .. *)
  let f_lval e = 
    find_share bsym_table chain2ix [] bidset e 
  in
  begin match bexe with 
  | BEXE_assign(_,i,(_,vt as e))
  | BEXE_init (_,i,(_,vt as e)) -> f_lval (bexpr_varname vt (i,[])); f_bexpr e
(*
  | BEXE_storeat (_,l,r) -> 
      find_pshare bsym_table chain2ix [] bidset l;
      f_bexpr r

  (* if we pass a RW or WO pointer to a routine, count it as a set *)
  (* Note this is all covered by the wildcard branch below anyhow .. *)
  | BEXE_call_with_trap (_,_,e)
  | BEXE_call_direct (_,_,_,e)
  | BEXE_jump_direct (_,_,_,e)
  | BEXE_call_stack(_,_,_,e)
  | BEXE_call_prim(_,_,_,e)
  | BEXE_jump (_,_,e)
  | BEXE_call (_,_,e) ->
      find_pshare bsym_table chain2ix [] bidset e
*)
  | _ -> Flx_bexe.iter ~f_bexpr bexe 
  end;

  if show_share_getset && not (BidSet.is_empty (!bidset)) then
    print_endline ("  SHARE SETS: " ^ string_of_vars bsym_table ix2chain (!bidset));
  !bidset

let once_get_gets bsym_table chain2ix ix2chain bexe = 
(*
  print_endline ("\nGet gets in Instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe); 
*)
  let bidset = ref BidSet.empty in
  let f_bexpr e = 
     match e with
     | BEXPR_ref (i,_),_ 
     | BEXPR_wref (i,_),_ -> ()
     | _ -> 
       try find_once bsym_table chain2ix [] bidset e 
       with DuplicateGet (i,ix) ->
        print_endline ("Flx_once: Duplicate Get variable " ^id_of_index bsym_table i  ^" in "^Flx_print.sbe bsym_table e);
        print_endline ("Instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe); 
        let sr = Flx_bexe.get_srcref bexe in
        print_endline (Flx_srcref.long_string_of_src sr);
        failwith ("Flx_once: Duplicate Get of unique variable")
  in
  begin match bexe with 
  (* storing at a pointer is still a get on the pointer! *)
  | BEXE_storeat (_,l,e)  -> f_bexpr l; f_bexpr e

  (* if the target of an assignment is a variable, is not a get *)
  (* nor is the target of an initialisation *)
  | BEXE_assign (_,_,e) 
  | BEXE_init (_,_,e) -> f_bexpr e
  | _ -> Flx_bexe.iter ~f_bexpr bexe
  end;

  if show_uniq_getset && not (BidSet.is_empty (!bidset)) then
    print_endline ("  UNIQ GETS: " ^ string_of_vars bsym_table ix2chain (!bidset));
  !bidset


let share_get_gets bsym_table chain2ix ix2chain bexe = 
  let bidset = ref BidSet.empty in
  let f_bexpr e = 
     match e with
     | BEXPR_ref (i,_),_ -> ()
     | _ -> 
       find_share bsym_table chain2ix [] bidset e 
  in
  begin match bexe with 
  (* storing at a pointer is still a get on the pointer! *)
  | BEXE_storeat (_,l,e)  -> (* f_bexpr l; *) f_bexpr e

  (* if the target of an assignment is a variable, is not a get *)
  (* nor is the target of an initialisation *)
  | BEXE_assign (_,_,e)
  | BEXE_init (_,_,e) -> f_bexpr e
  | _ -> Flx_bexe.iter ~f_bexpr bexe
  end;

  if show_share_getset && not (BidSet.is_empty (!bidset)) then
    print_endline ("  SHARE GETS: " ^ string_of_vars bsym_table ix2chain (!bidset));
  !bidset

(* Full unravelling removes expressions from instructions so all arguments
are variables, inserting assignments to temporaries before the instruction.
All such assignments are then either assigning a constant or variable,
or assigning the result of a single application to a constant or variable.

We have to do a partial unravel because the flow algorithm cannot remember
where control is up to during expression evaluation. However an assignment
to a variable of an application is more or less equivalent to a procedure
call. In other words the serialisation provides locations in the control
flow path allowing the algorithm to keep track of the current continuation
and liveness.

Unravelling of compositions is sound due to eager evaluation. 
However unravelling of products is only sound for unique variables
not shared ones. The reason is: two reads on a unique variable
can occur in a product; if the initial state is dead, its an error.
If the initial state is live, the first read changes it to dead,
which causes the second read to fail with an error.


*)


(* Bottom up analysis pushes innermost expression temporaries first
so the resulting list head is the outermost application. This is the order
we want because we construct the modified executable list by parsing
the list from the head, producing a list in reverse order and inserting
the already backwards temporary assignments first, then the stripped
instruction.
*)

let rec unrav sr counter extra e = 
  match Flx_bexpr.map ~f_bexpr:(unrav sr counter extra) e with
  | (BEXPR_apply ((BEXPR_closure (fidx, ts),_), (_,argt as arg)),rt as rhs) 
  | (BEXPR_apply_stack (fidx, ts, (_,argt as arg)),rt as rhs)
  | (BEXPR_apply_direct (fidx, ts, (_,argt as arg)),rt as rhs)
  ->
    let v = !counter in (* new temporary index *)
    incr counter;
    let lhs = bexpr_varname rt (v,ts) in
    let instr = bexe_assign (sr, v, rhs) in
    extra := instr :: !extra;
    lhs  (* return variable *)
  | x -> x

let unravel sr counter e = 
  let extra = ref [] in
  let x = unrav sr counter extra e in
  List.rev !extra, x

(* test case only, later do more instructions *)
let unravel_exe bsym_table counter exe =
  match exe with
  | BEXE_fun_return (sr,e) -> 
(* print_endline ("Unravelling return " ^ Flx_print.string_of_bexe bsym_table 2 exe); *)
    let extra,x = unravel sr counter e in
    let result = bexe_fun_return (sr,x) :: extra in
(*
    if List.length result > 1 then
    begin
      print_endline ("Unravlled function return: " ^ Flx_print.string_of_bexe bsym_table 2 exe);
      let r = List.rev result in
      List.iter (fun exe -> print_endline (Flx_print.string_of_bexe bsym_table 3 exe)) r
    end;
*)
    result
 
  | _ -> [exe]

let unravel_exes bsym_table counter exes =
   let exe_chunks = List.rev_map (unravel_exe bsym_table counter) exes in
   let rexes = List.concat exe_chunks in
   List.rev rexes


type once_data_t = {gets: BidSet.t; sets: BidSet.t}
type augexe_t = Flx_bexe.t * once_data_t 


(* Augment exes with gets and sets *)
let make_augexes bsym_table counter chain2ix ix2chain get_sets get_gets bexes : augexe_t list=
  let bexes = unravel_exes bsym_table counter bexes in
  List.map 
  (
    fun bexe -> 
      if show_share_getset_only || show_uniq_getset_only then
        print_endline ("instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
      bexe, 
      {
        sets=get_sets bsym_table chain2ix ix2chain bexe; 
        gets=get_gets bsym_table chain2ix ix2chain bexe
      }
  ) 
  bexes 


