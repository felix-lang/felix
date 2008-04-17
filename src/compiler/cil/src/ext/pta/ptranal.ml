(*
 *
 * Copyright (c) 2001-2002, 
 *  John Kodumal        <jkodumal@eecs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
exception Bad_return
exception Bad_function
exception UnknownLocation = Olf.UnknownLocation


open Cil

module H = Hashtbl

module A = Olf

type access = A.lvalue * bool

type access_map = (lval, access) H.t

(** a mapping from varinfo's back to fundecs *)
module VarInfoKey =
struct
  type t = varinfo
  let compare v1 v2 = Pervasives.compare (v1.vid) (v2.vid)
end

module F = Map.Make(VarInfoKey)

(***********************************************************************)
(*                                                                     *)
(* Global Variables                                                    *)
(*                                                                     *)
(***********************************************************************)

let print_constraints = A.print_constraints
let debug_constraints = A.debug_constraints
let debug_aliases = A.debug_aliases
let smart_aliases = A.smart_aliases
let debug = A.debug
let analyze_mono = A.analyze_mono
let no_flow = A.no_flow
let no_sub = A.no_sub
let fun_ptrs_as_funs = ref false
let show_progress = ref false
let debug_may_aliases = ref false

let found_undefined = ref false

let conservative_undefineds = ref false

let current_fundec : fundec option ref = ref None

let fun_access_map : (fundec, access_map) H.t = H.create 64

(* A mapping from varinfos to fundecs *)
let fun_varinfo_map = ref (F.empty)

let current_ret : A.tau option ref = ref None

let lvalue_hash : (varinfo,A.lvalue) H.t = H.create 64

let expressions : (exp,A.tau) H.t = H.create 64

let lvalues : (lval,A.lvalue) H.t = H.create 64

let count : int ref = ref 0

let fresh_index () : int = 
  incr count;
  !count
  
let alloc_names = ["malloc";"calloc";"realloc";"xmalloc";"__builtin_alloca";
		   "alloca";"kmalloc"]

let all_globals : varinfo list ref = ref []
let all_functions : fundec list ref = ref []

(***********************************************************************)
(*                                                                     *)
(* Utility Functions                                                   *)
(*                                                                     *)
(***********************************************************************)

let pointer_destroying_unop op : bool =
  false

let pointer_destroying_binop op : bool = 
  match op with
    | PlusA -> false                             
    | PlusPI -> false                            
    | IndexPI -> false                           
    | MinusA -> false                             
    | MinusPI -> false                             
    | MinusPP -> false                              
    | Mod -> false         
    | BXor -> false     
    | _ -> true

let is_undefined_fun = function
  | Lval (lh,o) ->
	if (isFunctionType (typeOfLval (lh,o))) 
	then
	  match lh with
	    | Var v -> v.vstorage = Extern
	    | _ -> false
	else
	  false
    | _ -> false

let is_alloc_fun = function 
    | Lval (lh,o) ->
	if (isFunctionType (typeOfLval (lh,o))) 
	then
	  match lh with
	    | Var v -> List.mem v.vname alloc_names
	    | _ -> false
	else
	  false
    | _ -> false

let next_alloc = function
  | Lval (Var v,o) ->
      let name = Printf.sprintf "%s@%d" (v.vname) (fresh_index())
      in A.address (A.make_lvalue false name (Some v)) (* check *)
  | _ -> raise Bad_return

let is_effect_free_fun = function
  | Lval (lh,o) when (isFunctionType (typeOfLval (lh,o))) ->
      begin match lh with
	| Var v -> begin
            try ("CHECK_" = String.sub v.vname 0 6)
            with Invalid_argument _ -> false
          end
	| _ -> false
      end
  | _ -> false
      

(***********************************************************************)
(*                                                                     *)
(* AST Traversal Functions                                             *)
(*                                                                     *)
(***********************************************************************)

(* should do nothing, might need to worry about Index case *)
(* let analyzeOffset (o : offset ) : A.tau = A.bottom () *)

let analyze_var_decl (v : varinfo ) : A.lvalue =
  try
    H.find lvalue_hash v
  with
    | Not_found -> 
	begin
	  let is_global = 
	    if (isFunctionType(v.vtype)) then false else v.vglob  
	  in
	  let lv = A.make_lvalue false v.vname (Some v)
	  in
	    H.add lvalue_hash v lv;
	    lv
	end

let isFunPtrType (t : typ) : bool = 
  match t with
    | TPtr (t,_) ->
	isFunctionType t
    | _ -> false
  
let rec analyze_lval (lv : lval ) : A.lvalue =
  let result = 
    match lv with
      | Var v,_ -> (* instantiate every syntactic occurrence of a function *)
	  begin
	    let alv = 
	      if (isFunctionType (typeOfLval lv)) 
	      then A.instantiate (analyze_var_decl v) (fresh_index())
	      else analyze_var_decl v
	    in
	      match (!current_fundec) with
		| None -> alv
		| Some f ->
		    let accesses = H.find fun_access_map f in
		      if (H.mem accesses lv) then alv
		      else
			begin
			  H.add accesses lv (alv,true);
			  alv
			end
	  end
      | Mem e,_ -> 
	  begin 
	    (* assert (not (isFunctionType(typeOf(e))) ); *)
	    let alv = 
	      if (!fun_ptrs_as_funs && isFunPtrType (typeOf e))
	      then
		analyze_expr_as_lval e
	      else
		A.deref (analyze_expr e)
	    in
	      match (!current_fundec) with
		| None -> alv
		| Some f ->
		    let accesses = H.find fun_access_map f in
		      if (H.mem accesses lv) 
		      then alv
		      else
			begin
			  H.add accesses lv (alv,false);
			  alv
			end
	  end
  in
    H.add lvalues lv result;
    result


and analyze_expr_as_lval (e : exp) : A.lvalue = 
  match e with
    | Lval l ->  analyze_lval l 
    | _ -> assert(false) (* todo -- other kinds of expressions? *)

and analyze_expr (e : exp ) : A.tau = 
(**  if (!debug_may_aliases & (not (H.mem expressions e) )) then
    H.add expressions e e;*)
  let result = 
    match e with
      | Const c -> A.bottom()
      | Lval l -> A.rvalue (analyze_lval l)
      | SizeOf _ -> A.bottom()
      | SizeOfStr _ -> A.bottom()
      | AlignOf _ -> A.bottom()
      | UnOp (op,e,t) -> 
	  begin
	    if (pointer_destroying_unop op)
	    then 
	      A.bottom ()
	    else
	      analyze_expr e
	  end
      | BinOp (op,e,e',t) ->
	  begin
	    if (pointer_destroying_binop op)
	    then
	      A.bottom ()
	    else
	      A.join (analyze_expr e) (analyze_expr e')
	  end
      | CastE (t,e) ->
	  analyze_expr(e)
      | AddrOf l ->  
	  if (!fun_ptrs_as_funs && isFunctionType (typeOfLval(l)) ) then
	    A.rvalue (analyze_lval l)
	  else
	    A.address (analyze_lval l)
      | StartOf l -> A.address (analyze_lval l)
      | AlignOfE _ -> A.bottom()
      | SizeOfE _ -> A.bottom() 
  in
    H.add expressions e result;
    result
     

(* check *)
let rec analyze_init (i : init ) : A.tau = 
  match i with 
    | SingleInit e ->
	analyze_expr(e)
    | CompoundInit (t,oi) ->
	A.join_inits (List.map (function (_,i) -> analyze_init(i)) oi)

let analyze_instr (i : instr ) : unit =
  match i with
    | Set (lval,rhs,l) -> 
	A.assign (analyze_lval lval) (analyze_expr rhs)
    | Call (res,fexpr,actuals,l) ->
	if ( not (isFunctionType(typeOf(fexpr))) ) 
	then () (* todo : is this a varargs ?? *)
	else if (is_alloc_fun fexpr)
	then 
	  begin
	    if (!debug)
	    then
	      begin
		print_string "Found allocation function..."; 
		print_newline()
	      end;
	    match res with
	      | Some r -> A.assign (analyze_lval r) (next_alloc (fexpr))
	      | None -> ()
	  end
        else if (is_effect_free_fun fexpr)
        then 
          begin 
            List.iter (fun e -> ignore (analyze_expr e)) actuals; 
          end
	else (* todo : check to see if the thing is an undefined function *)
	  begin
	    let 
	      fnres,site = 
	      if (is_undefined_fun fexpr & !conservative_undefineds) then
		begin
		  A.apply_undefined (List.map analyze_expr actuals) 
		end
	      else
		A.apply (analyze_expr fexpr) 
		  (List.map analyze_expr actuals)
	    in
	      match res with 
		| Some r ->
		    begin
		      A.assign_ret site (analyze_lval (r)) fnres;
		      found_undefined := true;
		    end
		| None -> ()
	  end
    | Asm _ -> ()
	
let rec analyze_stmt (s : stmt ) : unit = 
  match s.skind with
    | Instr il -> List.iter analyze_instr il
    | Return (eo,l) ->
	begin
	  match eo with
	    | Some e ->
		begin
		  match (!current_ret)
		  with
		    | Some ret ->
			A.return (ret) (analyze_expr e)
		    | None -> raise Bad_return
		end
	    | None -> ()
	end
    | Goto (s',l) ->() (* analyze_stmt(!s') *)
    | If (e,b,b',l) ->
	begin
	  (* ignore the expression e; expressions can't be side-effecting *)
	  analyze_block b;
	  analyze_block b'
	end
    | Switch (e,b,sl,l) ->
	begin
	  analyze_block b;
	  List.iter analyze_stmt sl
	end
    | Loop (b,l,_,_) -> analyze_block b
    | Block b -> analyze_block(b)
    | TryFinally (b, h, _) -> analyze_block b; analyze_block h
    | TryExcept (b, (il, _), h, _) -> 
        analyze_block b; 
        List.iter analyze_instr il;
        analyze_block h
    | Break l -> ()
    | Continue l -> ()
	

and analyze_block (b : block ) : unit = 
  List.iter analyze_stmt b.bstmts

let analyze_function (f : fundec ) : unit = 
  let oldlv = analyze_var_decl f.svar in
  let ret = A.make_fresh (f.svar.vname ^ "_ret") in
  let formals = List.map analyze_var_decl f.sformals in
  let locals = List.map analyze_var_decl f.slocals in
  let newf = A.make_function (f.svar.vname) formals ret in
    begin 
      if (!show_progress) 
      then
	begin
	  Printf.printf "Analyzing function %s" f.svar.vname;
	  print_newline()
	end;
      fun_varinfo_map := F.add f.svar f (!fun_varinfo_map);
      current_fundec := Some f;
      H.add fun_access_map f (H.create 8);
      A.assign oldlv newf;
      current_ret := Some ret;
      analyze_block(f.sbody)
    end

let analyze_global (g : global ) : unit = 
  match g with 
    | GVarDecl (v,l) -> (* ignore (analyze_var_decl(v)) -- no need *) ()
    | GVar (v,init,l) -> 
	begin
	  all_globals := v :: (!all_globals);
	  match init.init with
	    | Some i ->
		A.assign (analyze_var_decl(v)) (analyze_init(i))
	    | None ->
		ignore (analyze_var_decl(v))
	end
    | GFun (f,l) ->
	begin
	  all_functions := f :: (!all_functions);
	  analyze_function(f)
	end
    | _ -> ()

let analyze_file (f : file) : unit = 
  iterGlobals f analyze_global 

(***********************************************************************)
(*                                                                     *)
(* High-level Query Interface                                          *)
(*                                                                     *)
(***********************************************************************)

(* Same as analyze_expr, but no constraints. *)
let rec traverse_expr (e : exp) : A.tau = 
  H.find expressions e
(*
  match e with
    | Const c -> A.bottom()
    | Lval l ->	A.rvalue (traverse_lval l)
    | SizeOf _ -> A.bottom()
    | SizeOfStr _ -> A.bottom()
    | AlignOf _ -> A.bottom()
    | UnOp (op,e,t) -> 
	begin
	  if (pointer_destroying_unop op)
	  then 
	    A.bottom ()
	  else
	    traverse_expr e
	end
    | BinOp (op,e,e',t) ->
	begin
	  if (pointer_destroying_binop op)
	  then
	    A.bottom ()
	  else
	    A.join (traverse_expr e) (traverse_expr e')
	end
    | CastE (t,e) ->
	traverse_expr(e)
    | AddrOf l ->  
	if (!fun_ptrs_as_funs && isFunctionType (typeOfLval(l)) ) then
	  A.rvalue (traverse_lval l)
	else
	  A.address (traverse_lval l)
    | StartOf l -> A.address (traverse_lval l)
    | AlignOfE _ -> A.bottom()
    | SizeOfE _ -> A.bottom() 
*)

and traverse_expr_as_lval (e : exp) : A.lvalue = 
  match e with
    | Lval l -> traverse_lval l 
    | _ -> assert(false) (* todo -- other kinds of expressions? *)

and traverse_lval (lv : lval ) : A.lvalue = 
  H.find lvalues lv
(*
  match lv with
    | Var v,_ -> analyze_var_decl v
    | Mem e,_ -> 
	if (!fun_ptrs_as_funs && isFunPtrType (typeOf e))
	then
	  traverse_expr_as_lval e
	else
	  A.deref (traverse_expr e)
					    *)

let may_alias (e1 : exp) (e2 : exp) : bool = 
  let tau1,tau2 = traverse_expr e1, traverse_expr e2 in
  let result = A.may_alias tau1 tau2 in
    if ((!debug_may_aliases)) then
      begin
	let doc1 = d_exp () e1 in
	let doc2 = d_exp () e2 in
	let s1 = Pretty.sprint ~width:30 doc1 in
	let s2 = Pretty.sprint ~width:30 doc2 in
	  Printf.printf "%s and %s may alias? %s\n" s1 s2 (if result then "yes" else "no")
      end;
    result
            
let resolve_lval (lv : lval) : varinfo list = A.points_to (traverse_lval lv)

let resolve_exp (e : exp) : varinfo list = A.epoints_to (traverse_expr e)

let resolve_funptr (e : exp) : fundec list =
  let varinfos = A.epoints_to (traverse_expr e) in
    List.fold_left (fun fdecs -> fun vinf ->
		      try 
			(F.find vinf (!fun_varinfo_map)) :: fdecs
		      with
			| Not_found -> fdecs ) [] varinfos

let count_hash_elts h =
  let result = ref 0 in
    begin
      H.iter (fun _ -> fun _ -> incr result) lvalue_hash;
      !result
    end

(** Make the most pessimistic assumptions about globals if an undefined
  function is present. Such a function can write to every global variable *)
let hose_globals () : unit = 
  List.iter (fun vd -> A.assign_undefined (analyze_var_decl vd)) (!all_globals)
  
let show_progress_fn (counted : int ref) (total : int) : unit = 
  begin
    incr counted;
    if (!show_progress) then
      begin
	Printf.printf "Computed flow for %d of %d sets" (!counted) total;
	print_newline()
      end
    else ()
  end


let compute_may_aliases (b : bool) : unit = 
  let rec compute_may_aliases_aux (exps : exp list) =
    match (exps) with
      | h :: t -> ignore (List.map (may_alias h) t); compute_may_aliases_aux t 
      | [] -> ()
  in
  let exprs : exp list ref = ref [] in
    H.iter (fun e -> fun _ -> exprs := e :: (!exprs)) expressions;
    compute_may_aliases_aux (!exprs)


let compute_results (show_sets : bool) : unit = 
  if (show_sets) then
    begin
      print_string "Computing points-to sets...";
      print_newline();
    end;
  let 
    total_pointed_to = ref 0 in
  let
    total_lvalues = count_hash_elts lvalue_hash in
  let 
    counted_lvalues = ref 0 in
  let print_result (name,set) =
      let rec print_set s = 
	match s with
	  | h :: [] -> print_string h
	  | h :: t -> print_string (h ^ ", "); print_set t 
	  | [] -> ()
      in
	total_pointed_to := !total_pointed_to + (List.length set);
	if (show_sets) then
	  begin
	    let ptsize = List.length set in
	      if (ptsize > 0) then
		begin
		  print_string 
		    (name ^ "(" ^ (string_of_int ptsize) ^ ") -> ");
		  print_set set;
		  print_newline ()
		end
	  end
	else ()
  in
  let lval_elts : (string * (string list)) list ref = ref [] 
  in 
    if (!conservative_undefineds & !found_undefined) then hose_globals ();
    A.finished_constraints();
    Hashtbl.iter (fun vinf -> fun lv -> 
		    begin
		      (show_progress_fn counted_lvalues total_lvalues);
		      try
			lval_elts := (vinf.vname, A.points_to_names lv) :: (!lval_elts)
		      with
			| A.UnknownLocation -> ()
		    end
		 ) lvalue_hash;
    List.iter print_result (!lval_elts); 
    if (show_sets) then
      Printf.printf "Total number of things pointed to: %d\n" !total_pointed_to
    ;
    if (!debug_may_aliases) then
      begin
	Printf.printf "Printing may alias relationships\n";
	compute_may_aliases(true)
      end
  
let print_types () : unit =
  print_string "Printing inferred types of lvalues...";
  print_newline();
  Hashtbl.iter (fun vi -> fun lv ->
		  Printf.printf "%s : %s\n" vi.vname (A.string_of_lvalue lv)
	       ) lvalue_hash 

  

(** Alias queries. For each function, gather sets of locals, formals, and 
  globals. Do n^2 work for each of these functions, reporting whether or not
  each pair of values is aliased. Aliasing is determined by taking points-to
  set intersections.
*)
let compute_aliases = compute_may_aliases
(*
  let f_counted = ref 0 in
  let f_total = List.length (!all_functions) in
  let _ = print_string "Computing aliases..."; print_newline() in
  let s_count = ref 0 in
  let a_count = ref 0 in
  let a_total = ref 0 in
  let alias_query (vl : varinfo list) = 
    begin
      let (naive,smart) = A.alias_query b (List.map analyze_var_decl vl)
      in
      incr f_counted;
      a_count := (!a_count) + naive;
      s_count := (!s_count) + smart;
      a_total := (!a_total) + ( (List.length vl) * (List.length vl) )
    end
  in 
  let a_analyze_fundec (f : fundec) = 
    if (!show_progress) then begin
      Printf.printf "Finished %d of %d functions" (!f_counted) f_total;
      print_newline();
      Printf.printf "Scanning function %s" f.svar.vname;
      print_newline()
    end;
    alias_query ((!all_globals) @ f.sformals @ f.slocals)
    (* alias_query (!all_globals) *)
  in
    compute_may_aliases b;
    List.iter a_analyze_fundec (!all_functions)
    (* 
    Printf.printf "Naive queries : %d of %d possible\n" (!a_count) (!a_total);
    Printf.printf "Smart queries : %d of %d possible\n" (!s_count) (!a_total)
    *)
*)

(*
let compute_alias_frequency () : unit = 
  let f_counted = ref 0 in
  let f_total = List.length (!all_functions) in
  let _ = print_string "Computing alias frequency..."; print_newline() in
  let s_count = ref 0 in
  let a_count = ref 0 in
  let a_total = ref 0 in
  let alias_frequency (am : access_map) =
    begin
      let accesses = H.fold (fun _ -> fun acc -> fun res -> 
			       acc :: res
			    ) am []
      in
      let (naive,smart) = (A.alias_frequency accesses)
      in
	incr f_counted ;
	a_count := (!a_count) + naive;
	s_count := (!s_count) + smart;
	a_total := (!a_total) + 
	( (List.length accesses) * (List.length accesses) )
    end
  in
  let af_analyze_fundec (f : fundec) = 
    if (!show_progress) then begin
      Printf.printf "Finished %d of %d functions" (!f_counted) f_total;
      print_newline();
      Printf.printf "Scanning function %s" f.svar.vname;
      print_newline()
    end;
    alias_frequency (H.find fun_access_map f)
  in
    List.iter af_analyze_fundec (!all_functions);
    Printf.printf "Naive queries : %d of %d possible\n" (!a_count) (!a_total);
    Printf.printf "Smart queries : %d of %d possible\n" (!s_count) (!a_total)

*)


(***********************************************************************)
(*                                                                     *)
(* Abstract Location Interface                                         *)
(*                                                                     *)
(***********************************************************************)
type absloc = A.absloc

let rec lvalue_of_varinfo (vi : varinfo) : A.lvalue =
  H.find lvalue_hash vi

let lvalue_of_lval = traverse_lval
let tau_of_expr = traverse_expr

(** return an abstract location for a varinfo, resp. lval *)
let absloc_of_varinfo vi = A.absloc_of_lvalue (lvalue_of_varinfo vi)
let absloc_of_lval lv = A.absloc_of_lvalue (lvalue_of_lval lv)

let absloc_e_points_to e = A.absloc_epoints_to (tau_of_expr e)
let absloc_lval_aliases lv = A.absloc_points_to (lvalue_of_lval lv)

(* all abslocs that e transitively points to *)
let absloc_e_transitive_points_to (e : Cil.exp) : absloc list =
  let rec lv_trans_ptsto (worklist : varinfo list) (acc : varinfo list) : absloc list =
    match worklist with 
      | [] -> List.map absloc_of_varinfo acc
      | vi::wklst'' -> 
          if List.mem vi acc then 
            lv_trans_ptsto wklst'' acc
          else
            lv_trans_ptsto ( List.rev_append 
                               (A.points_to (lvalue_of_varinfo vi)) 
                               wklst'' ) 
              (vi::acc)
  in
    lv_trans_ptsto (A.epoints_to (tau_of_expr e)) []

let absloc_eq a b = A.absloc_eq(a,b)


let ptrAnalysis = ref false
let ptrResults = ref false
let ptrTypes = ref false





(** Turn this into a CIL feature *)
let feature : featureDescr = 
  { fd_name = "ptranal";
    fd_enabled = ptrAnalysis;
    fd_description = "alias analysis";
    fd_extraopt = [
    ("--ptr_may_aliases", 
     Arg.Unit (fun _ -> debug_may_aliases := true),
     "Print out results of may alias queries");
    ("--ptr_unify", Arg.Unit (fun _ -> no_sub := true),
     "Make the alias analysis unification-based");
    ("--ptr_conservative", 
     Arg.Unit (fun _ -> conservative_undefineds := true),
     "Treat undefineds conservatively in alias analysis");
    ("--ptr_results", Arg.Unit (fun _ -> ptrResults := true),
                     "print the results of the alias analysis"); 
    ("--ptr_mono", Arg.Unit (fun _ -> analyze_mono := true),
                    "run alias analysis monomorphically"); 
    ("--ptr_types",Arg.Unit (fun _ -> ptrTypes := true),
                    "print inferred points-to analysis types");];
    fd_doit = 
    (function (f: file) -> 
      analyze_file f;
      compute_results (!ptrResults);
(*       compute_aliases true;
*)    if (!ptrTypes) then 
         print_types ()
    );
    fd_post_check = false; (* No changes *)
  } 
