open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_set
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
      List.iter ut ts

  | _ -> iter_btype ut t

let faulty_req bsym_table i =
  let bsym = Flx_bsym_table.find bsym_table i in
  clierr
    bsym.Flx_bsym.sr
    (bsym.Flx_bsym.id ^ " is used but has unsatisfied requirement")

let rec process_expr h k sr e =
  let ue e = process_expr h k sr e in
  let ui i = add h k i sr in
  let ut t = uses_type h k sr t in
  iter_tbexpr ui ignore ut e

and cal_exe_usage h k exe =
  (*
  print_endline ("Checking uses in " ^ si k ^ ", exe: " ^ string_of_bexe syms.sym_table 2 exe);
  *)
  let sr = Flx_bexe.get_srcref exe in
  let ue e = process_expr h k sr e in
  let ui i = add h k i sr in
  let ut t = uses_type h k sr t in
  iter_bexe ui ue ut ignore ignore exe

let cal_expr_usage h k sr e =
  process_expr h k sr e

let uses_production h k sr p =
  let uses_symbol (_,nt) = match nt with
  | `Nonterm jj -> List.iter (fun i -> add h k i sr) jj
  | `Term _ -> () (* HACK! This is a union constructor name  we need to 'use' the union type!! *)
  in
  List.iter uses_symbol p

let cal_param_usage uses sr parent {pindex=child;ptyp=t} =
  uses_type uses parent sr t;
  add uses parent child sr

let cal_req_usage bsym_table uses sr parent reqs =
  let ur (j,ts) =
    if j = dummy_bid then faulty_req bsym_table parent
    else add uses parent j sr
  in
  List.iter ur reqs

let call_data_for_symbol bsym_table uses k bsym =
  let ut t = uses_type uses k bsym.Flx_bsym.sr t in

  match bsym.Flx_bsym.bbdcl with
  | BBDCL_module -> ()
  | BBDCL_typeclass _ -> ()

  | BBDCL_procedure (_,_,(ps,_),exes)
  | BBDCL_function (_,_,(ps,_),_,exes) ->
      List.iter (cal_param_usage uses bsym.Flx_bsym.sr k) ps;
      List.iter (cal_exe_usage uses k) exes

  | BBDCL_newtype (_,t) -> ut t
  | BBDCL_abs (_,_,_,reqs) ->
      cal_req_usage bsym_table uses bsym.Flx_bsym.sr k reqs
  | BBDCL_const (_,_,t,_,reqs) ->
      cal_req_usage bsym_table uses bsym.Flx_bsym.sr k reqs
  | BBDCL_proc (_,_,ps,_, reqs) ->
      cal_req_usage bsym_table uses bsym.Flx_bsym.sr k reqs;
      List.iter ut ps
  | BBDCL_fun (_,_,ps,ret,_, reqs,_) ->
      cal_req_usage bsym_table uses bsym.Flx_bsym.sr k reqs;
      List.iter ut ps;
      ut ret
  | BBDCL_insert (_,_,_,reqs) ->
      cal_req_usage bsym_table uses bsym.Flx_bsym.sr k reqs
  | BBDCL_instance (_,_,cons,i,ts) ->
      (* we dont add the type constraint, since it
      is only used for instance selection
      *)
      add uses k i bsym.Flx_bsym.sr;
      List.iter ut ts

  | BBDCL_nonconst_ctor (_,_,unt,_,ct, evs, etraint) ->
      ut unt;
      ut ct

  | BBDCL_union _  -> ()

  | BBDCL_cstruct (_,ps)
  | BBDCL_struct (_,ps) ->
      List.iter ut (List.map snd ps)

  | BBDCL_val (_,t)
  | BBDCL_var (_,t)
  | BBDCL_tmp (_,t) -> ut t
  | BBDCL_ref (_,t) -> ut (btyp_pointer t)
  | BBDCL_callback (_,_,ps_cf, ps_c, _, ret, reqs,_) ->
      List.iter ut ps_cf;
      List.iter ut ps_c;
      ut ret;
      cal_req_usage bsym_table uses bsym.Flx_bsym.sr k reqs

let call_data bsym_table =
  let uses = Hashtbl.create 97 in

  (* Figure out all the calls of the symbol table. *)
  Flx_bsym_table.iter (call_data_for_symbol bsym_table uses) bsym_table;

  (* invert uses table to get usedby table *)
  let usedby = Hashtbl.create 97 in
  Hashtbl.iter begin fun k ls ->
    List.iter (fun (i,sr) -> add usedby i k sr) ls
  end uses;

  uses, usedby

(* closure of i, excluding i unless it is recursive! *)
let cls h i =
  let c = ref BidSet.empty in
  let rec add j =
    if not (BidSet.mem j !c) then begin
      c := BidSet.add j !c;
      let x = try Hashtbl.find h j with Not_found -> [] in
      List.iter (fun (j,_) -> add j) x
    end
  in
  let x = try Hashtbl.find h i with Not_found -> [] in
  List.iter (fun (j,_) -> add j) x;
  !c

let is_recursive_call h caller callee = BidSet.mem caller (cls h callee)
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
  let c = ref BidSet.empty in
  let rec add j =
    if not (BidSet.mem j !c) && BidSet.mem j k then
    begin
      c := BidSet.add j !c;
      let x = try Hashtbl.find h j with Not_found -> [] in
      List.iter (fun (j,_) -> add j) x
    end
  in
    let x = try Hashtbl.find h i with Not_found -> [] in
    List.iter (fun (j,_) ->  add j) x
    ;
    !c

let call_report syms bsym_table (uses,usedby) f k =
  let si = string_of_int in
  let catmap = Flx_util.catmap in
  let w s = output_string f s in
  let isr = is_recursive uses k in
  let bsym = Flx_bsym_table.find bsym_table k in
  w (string_of_bid k ^ ": ");
  w (if isr then "recursive " else "");
  w
    begin match bsym.Flx_bsym.bbdcl with
    | BBDCL_function _ -> "fun "
    | BBDCL_procedure _ -> "proc "
    | BBDCL_var _ -> "var "
    | BBDCL_val _ -> "val "
    | _ -> assert false
    end
  ;
  w (bsym.Flx_bsym.id ^ " uses: ");
  let u = try Hashtbl.find uses k with Not_found -> [] in
  let x = ref [] in
  List.iter begin fun (i,_) ->
    if not (List.mem i !x) then
    try match Flx_bsym_table.find_bbdcl bsym_table i with
      | BBDCL_procedure _
      | BBDCL_function _
      | BBDCL_var _
      | BBDCL_val _ -> x := i::!x
      | _ -> ()
    with Not_found -> ()
  end
  u;
  let u = List.sort compare !x in
  w (catmap "," string_of_bid u);
  w "; usedby: ";
  let u = try Hashtbl.find usedby k with Not_found -> [] in
  let x = ref [] in
  List.iter (fun (i,_) -> if not (List.mem i !x) then x := i::!x) u;
  let u = List.sort compare !x in
  w (catmap "," string_of_bid u);
  w "\n"

let print_call_report' syms bsym_table usage f =
  let x = ref [] in
  Flx_bsym_table.iter begin fun k bsym ->
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_procedure _
    | BBDCL_function _
    | BBDCL_var _
    | BBDCL_val _ -> x := k :: !x
    | _ -> ()
  end bsym_table;
  List.iter
    (call_report syms bsym_table usage f)
    (List.sort compare (!x))

let print_call_report syms bsym_table f =
  let usage = call_data bsym_table in
  print_call_report' syms bsym_table usage f

let expr_uses_unrestricted syms descend usage e =
  let u = ref BidSet.empty in
  let add u i = u := BidSet.add i !u in
  iter_tbexpr (add u) ignore ignore e;


  (*
  print_string ("Direct usage of expr " ^ sbe syms.sym_table e ^ ": ");
  BidSet.iter (fun i -> print_string (si i^" ")) !u;
  print_endline "";


  print_string ("Restrict =  ");
  BidSet.iter (fun i -> print_string (si i^" ")) restrict;
  print_endline "";
  *)

  let u = BidSet.fold
    (fun i cls -> BidSet.union cls (
     let cl = child_use_closure descend usage i in
     (*
     print_string ("Closure of " ^ si i ^ " is: ");
     BidSet.iter (fun i -> print_string (si i ^ " ")) cl;
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
  BidSet.inter restrict u
