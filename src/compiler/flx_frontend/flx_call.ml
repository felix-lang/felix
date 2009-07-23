open Flx_ast
open Flx_types
open Flx_set
open List
open Flx_exceptions
open Flx_maps
open Flx_util
open Flx_print
open Flx_mtypes2

(* NOTE: THIS CODE LARGELY DUPLICATES CODE IN flx_use.ml *)

type usage_table_t =  (bid_t, (bid_t * Flx_srcref.t) list) Hashtbl.t
type usage_t =  usage_table_t * usage_table_t

let add (h:usage_table_t) k j sr =
  (*
  print_endline ("Adding use of " ^ si j ^ " in " ^ si k);
  *)
  Hashtbl.replace h k
  (
    (j,sr)
    ::
    (
      try Hashtbl.find h k
      with Not_found -> []
    )
  )

let rec uses_type h k sr t =
  let ut t = uses_type h k sr t in
  match t with
  | BTYP_inst (i,ts)
    ->
      add h k i sr;
      iter ut ts

  | _ -> iter_btype ut t

let faulty_req syms i =
  match Hashtbl.find syms.dfns i with {id=id; sr=sr } ->
  clierr sr (id ^ " is used but has unsatisfied requirement")

let rec process_expr h k sr e =
  let ue e = process_expr h k sr e in
  let ui i = add h k i sr in
  let ut t = uses_type h k sr t in
  iter_tbexpr ui ignore ut e

and cal_exe_usage syms h k exe =
  (*
  print_endline ("Checking uses in " ^ si k ^ ", exe: " ^ string_of_bexe syms.dfns 2 exe);
  *)
  let sr = src_of_bexe exe in
  let ue e = process_expr h k sr e in
  let ui i = add h k i sr in
  let ut t = uses_type h k sr t in
  iter_bexe ui ue ut ignore ignore exe

let cal_expr_usage syms h k sr e =
  process_expr h k sr e

let uses_production h k sr p =
  let uses_symbol (_,nt) = match nt with
  | `Nonterm jj -> iter (fun i -> add h k i sr) jj
  | `Term _ -> () (* HACK! This is a union constructor name  we need to 'use' the union type!! *)
  in
  iter uses_symbol p

let cal_param_usage syms uses sr parent {pindex=child;ptyp=t} =
  uses_type uses parent sr t;
  add uses parent child sr

let call_data syms (bbdfns:fully_bound_symbol_table_t):usage_t =
  let uses = Hashtbl.create 97 in
  let usedby = Hashtbl.create 97 in
  let usage = uses,usedby in
  let cal_req_usage sr parent reqs =
    let ur (j,ts) =
      if j = 0 then faulty_req syms parent
      else add uses parent j sr
    in
    iter ur reqs
  in
  Hashtbl.iter
  (fun k (_,_,sr,entry) ->
  let ut t = uses_type uses k sr t in

  match entry with
  | BBDCL_typeclass _ -> ()

  | BBDCL_procedure (_,_,(ps,_),exes)
  | BBDCL_function (_,_,(ps,_),_,exes) ->
    iter (cal_param_usage syms uses sr k) ps;
    iter (cal_exe_usage syms uses k) exes

  | BBDCL_newtype (_,t) -> ut t
  | BBDCL_abs (_,_,_,reqs) -> cal_req_usage sr k reqs
  | BBDCL_const (_,_,t,_,reqs) -> cal_req_usage sr k reqs
  | BBDCL_proc (_,_,ps,_, reqs)  -> cal_req_usage sr k reqs; iter ut ps
  | BBDCL_fun (_,_,ps,ret,_, reqs,_)  -> cal_req_usage sr k reqs; iter ut ps; ut ret
  | BBDCL_insert (_,_,_,reqs)  -> cal_req_usage sr k reqs
  | BBDCL_instance (_,_,cons,i,ts) ->
    (* we dont add the type constraint, since it
    is only used for instance selection
    *)
    add uses k i sr; iter ut ts

  | BBDCL_nonconst_ctor (_,_,unt,_,ct, evs, etraint) ->
    ut unt; ut ct

  | BBDCL_union _  -> ()

  | BBDCL_cstruct (_,ps)
  | BBDCL_struct (_,ps) ->
    iter ut (map snd ps)

  | BBDCL_val (_,t)
  | BBDCL_var (_,t)
  | BBDCL_tmp (_,t) -> ut t
  | BBDCL_ref (_,t) -> ut (BTYP_pointer t)
  | BBDCL_callback (_,_,ps_cf, ps_c, _, ret, reqs,_) ->
    iter ut ps_cf;
    iter ut ps_c;
    ut ret; cal_req_usage sr k reqs

  )
  bbdfns
  ;
  (* invert uses table to get usedby table *)
  Hashtbl.iter
  (fun k ls ->
    iter
    (fun (i,sr) -> add usedby i k sr)
    ls
  )
  uses
  ;
  usage

(* closure of i, excluding i unless it is recursive! *)
let cls h i =
  let c = ref IntSet.empty in
  let rec add j =
    if not (IntSet.mem j !c) then
    begin
      c := IntSet.add j !c;
      let x = try Hashtbl.find h j with Not_found -> [] in
      iter (fun (j,_) -> add j) x
    end
  in
    let x = try Hashtbl.find h i with Not_found -> [] in
    iter (fun (j,_) -> add j) x
    ;
    !c

let is_recursive_call h caller callee = IntSet.mem caller (cls h callee)
let is_recursive h i = is_recursive_call h i i

let use_closure h i = cls h i

(* this calculates the use closure of i, eliminating recursive
  calls to the base function by restricting references
  to some set k. Note this means the usage of k is also
  not included.

  If k is set to the children of some function f,
  then this routine will not report usage of any
  variables in f via calls to f, only direct
  uses in some child which is called; in particular
  calls to outside the child tree of f are not tracked
  since they can't call any children of f,
  so they can only use them via a call to f.
  This would spawn a new stack frame, and so
  refer to different copies of variables.

  This routine is used to find which variables
  in f an expression in f can use via a call to a child.

  OUCH OUCH OUCH. I THINK THIS IDEA MUST BE BUGGED!

  Here's the problem. Given

  fun A(){
    fun B { fun C() {} return C; }
    fun D(f) { f 1; }
    D (B());
  }

  function B is returning a closure of C,
  which is being passed into D and called.
  Note D cannot see the function C.

  The inliner should handle this correctly:
  B is inlined to return a *clone* C' of C which
  is nested in A, then D is inlined, resulting
  in the call C' 1 (which can now be inlined too).

  The problem is that the assumption "calls outside
  the child tree of f are not tracked since they can't
  call any children of f" is wrong. A call outside
  the tree can still execute something inside
  the tree via a closure .. however how does the
  closure get out .. it has to be 'made' by someon
  who can see it ..

*)

let child_use_closure k h i =
  let c = ref IntSet.empty in
  let rec add j =
    if not (IntSet.mem j !c) && IntSet.mem j k then
    begin
      c := IntSet.add j !c;
      let x = try Hashtbl.find h j with Not_found -> [] in
      iter (fun (j,_) -> add j) x
    end
  in
    let x = try Hashtbl.find h i with Not_found -> [] in
    iter (fun (j,_) ->  add j) x
    ;
    !c

let call_report syms bbdfns (uses,usedby) f k =
  let si = string_of_int in
  let catmap = Flx_util.catmap in
  let w s = output_string f s in
  let isr = is_recursive uses k in
  let id,_,sr,entry = Hashtbl.find bbdfns k in
  w (si k ^ ": ");
  w (if isr then "recursive " else "");
  w
    begin match entry with
    | BBDCL_function _ -> "fun "
    | BBDCL_procedure _ -> "proc "
    | BBDCL_var _ -> "var "
    | BBDCL_val _ -> "val "
    | _ -> assert false
    end
  ;
  w (id ^ " uses: ");
  let u = try Hashtbl.find uses k with Not_found -> [] in
  let x = ref [] in
  iter
  (fun (i,_) ->
    if not (mem i !x) then
    try match Hashtbl.find bbdfns i with
      | _,_,_,BBDCL_procedure _
      | _,_,_,BBDCL_function _
      | _,_,_,BBDCL_var _
      | _,_,_,BBDCL_val _ -> x := i::!x
      | _ -> ()
    with Not_found -> ()
  )
  u;
  let u = sort compare !x in
  w (catmap "," si u);
  w "; usedby: ";
  let u = try Hashtbl.find usedby k with Not_found -> [] in
  let x = ref [] in
  iter (fun (i,_) -> if not (mem i !x) then x := i::!x) u;
  let u = sort compare !x in
  w (catmap "," si u);
  w "\n"

let print_call_report' syms bbdfns usage f =
  let x = ref [] in
  Hashtbl.iter
  (fun k (id,_,sr,entry) ->
    match entry with
    | BBDCL_procedure _
    | BBDCL_function _
    | BBDCL_var _
    | BBDCL_val _
      -> x := k :: !x
    | _ -> ()
  )
  bbdfns
  ;
  iter
    (call_report syms bbdfns usage f)
    (sort compare (!x))

let print_call_report syms bbdfns f =
  let usage = call_data syms bbdfns in
  print_call_report' syms bbdfns usage f

let expr_uses_unrestricted syms descend usage e =
  let u = ref IntSet.empty in
  let add u i = u := IntSet.add i !u in
  iter_tbexpr (add u) ignore ignore e;


  (*
  print_string ("Direct usage of expr " ^ sbe syms.dfns e ^ ": ");
  IntSet.iter (fun i -> print_string (si i^" ")) !u;
  print_endline "";


  print_string ("Restrict =  ");
  IntSet.iter (fun i -> print_string (si i^" ")) restrict;
  print_endline "";
  *)

  let u = IntSet.fold
    (fun i cls -> IntSet.union cls (
     let cl = child_use_closure descend usage i in
     (*
     print_string ("Closure of " ^ si i ^ " is: ");
     IntSet.iter (fun i -> print_string (si i ^ " ")) cl;
     print_endline "";
     *)
     cl
    ))
    !u
    !u
  in
  u

let expr_uses syms descend usage restrict e =
  let u = expr_uses_unrestricted syms descend usage e in
  IntSet.inter restrict u
