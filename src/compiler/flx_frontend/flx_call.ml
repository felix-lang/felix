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
open Flx_bid

(* NOTE: THIS CODE LARGELY DUPLICATES CODE IN flx_use.ml *)

type usage_table_t = (bid_t, (bid_t * Flx_srcref.t) list) Hashtbl.t
type usage_t = usage_table_t * usage_table_t

let add uses sr parent bid =
  (*
  print_endline ("Adding use of " ^ si j ^ " in " ^ si k);
  *)
  if bid <> 0 then
  Hashtbl.replace uses parent
  (
    (bid,sr)
    ::
    (
      try Hashtbl.find uses parent
      with Not_found -> []
    )
  )

let rec uses_type uses sr parent t =
  let f_btype t = uses_type uses sr parent t in

  (* We only care about type instances. *)
  match t with
  | BTYP_inst (i,ts,_) ->
      add uses sr parent i;
      List.iter f_btype ts

  | _ -> Flx_btype.flat_iter ~f_btype t

let rec cal_expr_usage uses sr parent bexpr =
  Flx_bexpr.iter
    ~f_bid:(add uses sr parent)
    ~f_btype:(uses_type uses sr parent)
    bexpr

and cal_exe_usage uses parent bexe =
  let sr = Flx_bexe.get_srcref bexe in

  Flx_bexe.iter
    ~f_bid:(add uses sr parent)
    ~f_btype:(uses_type uses sr parent)
    ~f_bexpr:(cal_expr_usage uses sr parent)
    bexe

let cal_param_usage uses sr parent {pindex=child;ptyp=t} =
  uses_type uses sr parent t;
  add uses sr parent child

let cal_bsym_usage bsym_table uses parent _ bsym =
  let sr = Flx_bsym.sr bsym in

  Flx_bbdcl.iter
    ~f_bid:(add uses sr parent)
    ~f_btype:(uses_type uses sr parent)
    ~f_bexpr:(cal_expr_usage uses sr parent)
    ~f_bexe:(cal_exe_usage uses parent)
    (Flx_bsym.bbdcl bsym)

let call_data bsym_table =
  let uses = Hashtbl.create 97 in

  (* Figure out all the calls of the symbol table. *)
  Flx_bsym_table.iter (cal_bsym_usage bsym_table uses) bsym_table;

  (* invert uses table to get usedby table *)
  let usedby = Hashtbl.create 97 in
  Hashtbl.iter begin fun bid ls ->
    List.iter (fun (parent,sr) -> add usedby sr parent bid) ls
  end uses;

  uses, usedby

(* closure of i, excluding i unless it is recursive! *)
let cls uses i =
  let c = ref BidSet.empty in
  let rec add j =
    if not (BidSet.mem j !c) then begin
      c := BidSet.add j !c;
      let x = try Hashtbl.find uses j with Not_found -> [] in
      List.iter (fun (j,_) -> add j) x
    end
  in
  let x = try Hashtbl.find uses i with Not_found -> [] in
  List.iter (fun (j,_) -> add j) x;
  !c

let is_recursive_call uses caller callee = BidSet.mem caller (cls uses callee)
let is_recursive uses i = is_recursive_call uses i i

let use_closure uses i = cls uses i

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


  ANOTHER BUG: it doesn't handle indirect use through
  virtual functions. If the function is itself inside
  a instance it can call into the virtual and map back
  into the same instance. If the function got cloned,
  the reference through the virtual will still be
  to the *original* uncloned function.
*)

let child_use_closure k uses i =
  let c = ref BidSet.empty in
  let rec add j =
    if not (BidSet.mem j !c) && BidSet.mem j k then begin
      c := BidSet.add j !c;
      let x = try Hashtbl.find uses j with Not_found -> [] in
      List.iter (fun (j,_) -> add j) x
    end
  in
  let x = try Hashtbl.find uses i with Not_found -> [] in
  List.iter (fun (j,_) -> add j) x;
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
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (_,_,_,BTYP_void,_,_) -> "proc "
    | BBDCL_fun (_,_,_,_,_,_) -> "fun "
    | BBDCL_val (_,_,`Once) -> "once "
    | BBDCL_val (_,_,`Val) -> "val "
    | BBDCL_val (_,_,`Var) -> "var "
    | BBDCL_label _ -> "label "
    | _ -> assert false
    end
  ;
  w (Flx_bsym.id bsym ^ " uses: ");
  let u = try Hashtbl.find uses k with Not_found -> [] in
  let x = ref [] in
  List.iter begin fun (i,_) ->
    if not (List.mem i !x) then
    try match Flx_bsym_table.find_bbdcl bsym_table i with
      | BBDCL_label _
      | BBDCL_fun _
      | BBDCL_val (_,_,(`Val | `Var | `Once)) -> x := i :: !x
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
  Flx_bsym_table.iter begin fun k _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_label _
    | BBDCL_fun _
    | BBDCL_val (_,_,(`Val | `Var | `Once )) -> x := k :: !x
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
  Flx_bexpr.iter ~f_bid:(add u) e;


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

