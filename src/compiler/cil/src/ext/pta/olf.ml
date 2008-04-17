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

(***********************************************************************)
(*                                                                     *)
(* Exceptions                                                          *)
(*                                                                     *)
(***********************************************************************)
exception Inconsistent (* raised if constraint system is inconsistent *)
exception WellFormed   (* raised if types are not well-formed *)
exception NoContents   
exception APFound      (* raised if an alias pair is found, a control
			  flow exception *)
exception ReachedTop   (* raised if top (from an undefined function)
			  flows to a c_absloc during the flow step *)
exception UnknownLocation

open Cil

module U = Uref 
module S = Setp
module H = Hashtbl
module Q = Queue

(** Generic bounds *)
type 'a bound = {info : 'a U.uref} 

module Bound =
struct 
  type 'a t = 'a bound
  let compare (x : 'a t) (y : 'a t) = 
    Pervasives.compare (U.deref x.info) (U.deref y.info)
end

module B = S.Make(Bound)

type 'a boundset = 'a B.t

(** Abslocs, which identify elements in points-to sets *)
(** jk : I'd prefer to make this an 'a absloc and specialize it to varinfo
    for use with the Cil frontend, but for now, this will do *)
type absloc = int * string * (Cil.varinfo option)

module Absloc =
struct
  type t = absloc

  let compare ((xid,_,_) : t) ((yid,_,_) : t) =
    Pervasives.compare xid yid
end
module C = Set.Make(Absloc)

(** Sets of abslocs. Set union is used when two c_abslocs containing 
  absloc sets are unified *)
type abslocset = C.t


type c_abslocinfo = {
  mutable l_name: string;   (** name of the location *)
  loc : absloc;
  l_stamp : int;
  mutable l_top : bool;
  mutable aliases: abslocset;
  mutable lbounds: c_abslocinfo boundset;
  mutable ubounds: c_abslocinfo boundset;
  mutable flow_computed : bool;
}

and c_absloc = c_abslocinfo U.uref

(** The type of lvalues. *)
type lvalue = {
  l: c_absloc; 
  contents: tau
}

and vinfo = {
  v_stamp : int;
  v_name : string;
  mutable v_top : bool;
  mutable v_lbounds : tinfo boundset;
  mutable v_ubounds : tinfo boundset;
}

and rinfo = {
  r_stamp : int;
  rl : c_absloc;
  points_to : tau;
}

and finfo = {
  f_stamp : int;
  fl : c_absloc;
  ret : tau;
  mutable args : tau list;
}

and pinfo = {
  p_stamp : int;
  ptr : tau;
  lam : tau;
}
and tinfo = Var of vinfo
	    | Ref of rinfo
	    | Fun of finfo
	    | Pair of pinfo

and tau = tinfo U.uref

type tconstraint = Unification of tau * tau
		   | Leq of tau * tau

(** Association lists, used for printing recursive types. The first element 
  is a type that has been visited. The second element is the string
  representation of that type (so far). If the string option is set, then 
  this type occurs within itself, and is associated with the recursive var
  name stored in the option. When walking a type, add it to an association 
  list. 

  Example : suppose we have the constraint 'a = ref('a). The type is unified
  via cyclic unification, and would loop infinitely if we attempted to print
  it. What we want to do is print the type u rv. ref(rv). This is accomplished
  in the following manner:
  
  -- ref('a) is visited. It is not in the association list, so it is added
  and the string "ref(" is stored in the second element. We recurse to print
  the first argument of the constructor.

  -- In the recursive call, we see that 'a (or ref('a)) is already in the
  association list, so the type is recursive. We check the string option,
  which is None, meaning that this is the first recurrence of the type. We
  create a new recursive variable, rv and set the string option to 'rv. Next,
  we prepend u rv. to the string representation we have seen before, "ref(",
  and return "rv" as the string representation of this type.

  -- The string so far is "u rv.ref(". The recursive call returns, and we
  complete the type by printing the result of the call, "rv", and ")"

  In a type where the recursive variable appears twice, e.g. 'a = pair('a,'a),
  the second time we hit 'a, the string option will be set, so we know to 
  reuse the same recursive variable name.
*)
type association = tau * string ref * string option ref

(** The current state of the solver engine 
    either adding mor econstraints, or finished adding constraints
    and querying graph 
*)
type state = 
  | AddingConstraints
  | FinishedConstraints

(***********************************************************************)
(*                                                                     *)
(* Global Variables                                                    *)
(*                                                                     *)
(***********************************************************************)
(** A count of the constraints introduced from the AST. Used for debugging. *)
let toplev_count = ref 0

let solver_state : state ref = ref AddingConstraints

(** Print the instantiations constraints. *)
let print_constraints : bool ref = ref false

(** If true, print all constraints (including induced) and show additional 
 debug output. *)
let debug = ref false

(** Just debug all the constraints (including induced) *)
let debug_constraints = ref false

(** Debug the flow step *)
let debug_flow_step = ref false

(** Compatibility with GOLF *)
let debug_aliases = ref false
let smart_aliases = ref false
let no_flow = ref false
let analyze_mono = ref false

(** If true, disable subtyping (unification at all levels) *)
let no_sub = ref false

(** A counter for generating unique integers. *)
let counter : int ref = ref 0

let stamp : int ref = ref 0

(** A list of equality constraints. *)
let eq_worklist : tconstraint Q.t = Q.create()

(** A list of leq constraints. *)
let leq_worklist : tconstraint Q.t = Q.create()

(** A hashtable containing stamp pairs of c_abslocs that must be aliased. *)
let cached_aliases : (int * int,unit) H.t = H.create 64

(** A hashtable mapping pairs of tau's to their join node. *)
let join_cache : (int * int, tau) H.t = H.create 64

(***********************************************************************)
(*                                                                     *)
(* Utility Functions                                                   *)
(*                                                                     *)
(***********************************************************************)
let die s = 
  begin
    Printf.printf "*******\nAssertion failed: %s\n*******" s;
    print_newline(); 
    assert(false)
  end

let insist b s =
  if (not b) then
    die s
  else ()


let can_add_constraints () = (!solver_state = AddingConstraints) 

let can_query_graph () = (!solver_state = FinishedConstraints)

let finished_constraints () = 
  begin
    insist (!solver_state = AddingConstraints) "inconsistent states";
    solver_state := FinishedConstraints
  end

let find = U.deref

(** return the prefix of the list up to and including the first element
  satisfying p. if no element satisfies p, return the empty list *)
let rec keep_until p l = 
  match l with
    | x :: xs -> if (p x) then [x] else x :: (keep_until p xs)
    | [] -> []
  

(** Generate a unique integer. *)
let fresh_index () : int = 
  incr counter; 
  !counter

let fresh_stamp () : int = 
  incr stamp;
  !stamp

(** Return a unique integer representation of a tau *)
let get_stamp (t : tau) : int = 
  match find t with
    | Var v -> v.v_stamp
    | Ref r -> r.r_stamp
    | Pair p -> p.p_stamp
    | Fun f -> f.f_stamp

(** Consistency checks for inferred types *)
let  pair_or_var (t : tau) = 
  match (find t) with
    | Pair _ -> true
    | Var _ -> true
    | _ -> false

let ref_or_var (t : tau) =
   match (find t) with
     | Ref _ -> true
     | Var _ -> true
     | _ -> false

let fun_or_var (t : tau) =
  match (find t) with
    | Fun _ -> true
    | Var _ -> true
    |  _ -> false


(** Apply [f] structurally down [t]. Guaranteed to terminate, even if [t]
  is recursive *)
let iter_tau f t = 
  let visited : (int,tau) H.t = H.create 4 in
  let rec iter_tau' t =
    if (H.mem visited (get_stamp t)) then () else
      begin
	f (t);
	H.add visited (get_stamp t) t;
	match find t with
	  |Pair p -> 
	      begin 
		iter_tau' p.ptr;
		iter_tau' p.lam
	      end
	  | Fun f ->
	      begin
		List.iter iter_tau' (f.args);
		iter_tau' f.ret;
	      end
	  | Ref r ->
	      begin
		iter_tau' r.points_to
	      end
	  | _ -> ()
      end
  in
    iter_tau' t

let equal_absloc = function 
  | ((i,_,_),(i',_,_)) ->  (i = i')
      
let equal_c_absloc l l' = 
  (find l).l_stamp = (find l').l_stamp
    

let equal_tau (t : tau) (t' : tau) =
  (get_stamp t) = (get_stamp t')

let top_c_absloc l = (find l).l_top

let get_flow_computed l = (find l).flow_computed

let set_flow_computed l = (find l).flow_computed <- true

let rec top_tau (t : tau) = 
  match find t with
    | Pair p -> top_tau (p.ptr) || top_tau (p.lam)
    | Ref r -> top_c_absloc r.rl 
    | Fun f -> top_c_absloc f.fl
    | Var v -> v.v_top

let get_c_absloc_stamp (l : c_absloc) : int =
  (find l).l_stamp

let set_top_c_absloc (l : c_absloc) (b: bool) : unit =
  begin
    (find l).l_top <- b
  end

let get_aliases (l : c_absloc) = 
  if (top_c_absloc l)
  then raise ReachedTop
  else (find l).aliases

(***********************************************************************)
(*                                                                     *)
(* Printing Functions                                                  *)
(*                                                                     *)
(***********************************************************************)

(** Convert a c_absloc to a string, short representation *)
let string_of_c_absloc (l : c_absloc) : string =
  "\"" ^ (find l).l_name ^ (if (top_c_absloc l) then "(top)" else "") ^ "\""

(** Return true if the element [e] is present in the association list,
 according to uref equality *)
let rec assoc_list_mem (e : tau) (l : association list) =
  match l with
    | [] -> None
    | (h,s,so) :: t -> 
	if (U.equal (h,e)) then (Some (s,so)) else assoc_list_mem e t

(** Given a tau, create a unique recursive variable name. This should always
  return the same name for a given tau *)
let fresh_recvar_name (t : tau) : string =	    
  match find t with
    | Pair p -> "rvp" ^ string_of_int(p.p_stamp) 
    | Ref r -> "rvr" ^ string_of_int(r.r_stamp) 
    | Fun f -> "rvf" ^ string_of_int(f.f_stamp)
    | _ -> die "fresh_recvar_name" false

	
(** Return a string representation of a tau, using association lists. *)
let string_of_tau (t : tau ) : string = 
  let tau_map : association list ref = ref [] in
  let rec string_of_tau' t = 
    match (assoc_list_mem t (!tau_map)) with
      | Some (s,so) -> (* recursive type. see if a var name has been set *)
	  begin
	    match (!so) with
	      | None ->
		  begin
		    let rv = fresh_recvar_name(t) in
		      s := "u " ^ rv ^ "." ^ (!s);
		      so := Some (rv);
		      rv
		  end
	      | Some rv -> rv
	  end
      | None -> (* type's not recursive. Add it to the assoc list and cont. *)
	  let s = ref "" in
	  let so : string option ref = ref None in
	    begin
	      tau_map := (t,s,so) :: (!tau_map);

	      (match (find t) with
		 | Var v -> s := v.v_name;
		| Pair p -> 
		    begin
		      insist (ref_or_var(p.ptr)) "wellformed";
		      insist (fun_or_var(p.lam)) "wellformed";
		      s := "{";
		      s := (!s) ^ (string_of_tau' p.ptr);
		      s := (!s) ^ ",";
		      s := (!s) ^ (string_of_tau' p.lam);
		      s := (!s) ^"}"
		
		    end
		| Ref r ->
		    begin
		      insist (pair_or_var(r.points_to)) "wellformed";
		      s := "ref(|";
		      s := (!s) ^ (string_of_c_absloc r.rl);
		      s := (!s) ^ "|,";
		      s := (!s) ^ (string_of_tau' r.points_to);
		      s := (!s) ^ ")"
		    
		    end
		| Fun f ->
		    begin
		      insist (pair_or_var(f.ret)) "wellformed";
		      let rec string_of_args = function
			| h :: [] ->
			    begin
			      insist (pair_or_var(h)) "wellformed";
			      s := (!s) ^ (string_of_tau' h)
			    end
			| h :: t -> 
			    begin
			      insist (pair_or_var(h)) "wellformed";
			      s := (!s) ^ (string_of_tau' h) ^ ",";
			      string_of_args t
			    end
			| [] -> ()
		      in
			s := "fun(|";
			s := (!s) ^ (string_of_c_absloc f.fl);
			s := (!s) ^ "|,";
			s := (!s) ^ "<";
			if (List.length (f.args) > 0) 
			then
			  string_of_args (f.args)
			else
			  s := (!s) ^ "void";
			s := (!s) ^">,";
			s := (!s) ^ (string_of_tau' f.ret);
			s := (!s) ^ ")"
		    end);
	      tau_map := List.tl (!tau_map);
	      !s
	    end
  in
    string_of_tau' t

(** Convert an lvalue to a string *)	
let rec string_of_lvalue (lv : lvalue) : string =
  let contents = (string_of_tau(lv.contents)) in
  let l = (string_of_c_absloc lv.l) in
    insist (pair_or_var(lv.contents)) "inconsistency at string_of_lvalue"; 
    (* do a consistency check *)
    Printf.sprintf "[%s]^(%s)" contents l

(** Print a list of tau elements, comma separated *)
let rec print_tau_list (l : tau list) : unit = 
  let t_strings = List.map string_of_tau l in
  let rec print_t_strings = function
    | h :: [] -> print_string h; print_newline();
    | h :: t -> print_string h; print_string ", "; print_t_strings t
    | [] -> ()
  in
    print_t_strings t_strings

let print_constraint (c : tconstraint) =
  (
    match c with
      | Unification (t,t') ->
	  let lhs = string_of_tau t in
	  let rhs = string_of_tau t' in 
	    Printf.printf "%s == %s" lhs rhs
      | Leq (t,t') ->
	  let lhs = string_of_tau t in
	  let rhs = string_of_tau t' in
	    Printf.printf "%s <= %s" lhs  rhs
  ); print_newline()

(***********************************************************************)
(*                                                                     *)
(* Type Operations -- these do not create any constraints              *)
(*                                                                     *)
(***********************************************************************)

(** Create an lvalue with c_absloc [lbl] and tau contents [t]. *)
let make_lval (loc,t : c_absloc * tau) : lvalue = 
  {l = loc; contents = t}

let make_c_absloc_int (is_top : bool) (name : string)
  (vio : Cil.varinfo option) : c_absloc = 
  let my_absloc  = 
    (fresh_index(),name,vio) in
  let locc = 
    C.add my_absloc C.empty
  in
    U.uref {
      l_name = name;
      l_top = is_top;
      l_stamp = fresh_stamp();
      loc = my_absloc;
      aliases = locc;
      ubounds = B.empty;   
      lbounds = B.empty; 
      flow_computed = false;
    }

(** Create a new c_absloc with name [name]. Also adds a fresh absloc
 with name [name] to this c_absloc's aliases set. *)  
let make_c_absloc (is_top : bool) (name : string) (vio : Cil.varinfo option) =
  make_c_absloc_int is_top name (vio)

let fresh_c_absloc (is_top : bool) : c_absloc =
  let index = fresh_index() in
    make_c_absloc_int is_top ("l_" ^ (string_of_int index)) None

(** Create a fresh bound (edge in the constraint graph). *)
let make_bound (a : c_absloc) : c_abslocinfo bound = 
  {info = a }

let make_tau_bound (t : tau) : tinfo bound = 
  {info = t }

(** Create a fresh named variable with name '[name]. *)
let make_var (is_top : bool) (name : string) : tau =
  U.uref (Var {v_name = ("'" ^name);
	       v_top = is_top;
	       v_stamp = fresh_index();
	       v_lbounds = B.empty;
	       v_ubounds = B.empty;}
	 )
    
let fresh_var  (is_top : bool) : tau = 
  make_var is_top ("fi" ^ (string_of_int (fresh_index())) )

(** Create a fresh unnamed variable (name will be 'fi). *)
let fresh_var_i (is_top : bool) : tau = 
  make_var is_top ("fi" ^ (string_of_int (fresh_index())) )

(** Create a Fun constructor. *)
let make_fun (lbl,a,r : c_absloc * (tau list) * tau) : tau =
  U.uref (Fun {fl = lbl; 
	       f_stamp = fresh_index();
	       args = a; 
	       ret = r }
	 )

(** Create a Ref constructor. *)
let make_ref (lbl,pt : c_absloc * tau) : tau =
  U.uref (Ref {rl = lbl; 
	       r_stamp = fresh_index();
	       points_to = pt}
	 )

(** Create a Pair constructor. *)
let make_pair (p,f : tau * tau) : tau =
  U.uref (Pair {ptr = p; 
		p_stamp = fresh_index();
		lam = f}
	 )
    
(** Copy the toplevel constructor of [t], putting fresh variables in each
  argement of the constructor. *)
let copy_toplevel (t : tau) : tau = 
  match find t with
    | Pair _ -> 
	make_pair (fresh_var_i false, fresh_var_i false)
    | Ref  _ -> 
	make_ref (fresh_c_absloc false,fresh_var_i false)
    | Fun  f -> 
	let fresh_fn = fun _ -> fresh_var_i false
	in
	  make_fun (fresh_c_absloc false, 
		    List.map fresh_fn (f.args) , fresh_var_i false)
    | _ -> die "copy_toplevel" false

let has_same_structure (t : tau) (t' : tau) =
  match find t, find t' with
    | Pair _, Pair _ -> true
    | Ref _, Ref _ -> true
    | Fun _, Fun _ -> true
    | Var _, Var _ -> true
    | _ -> false

let pad_args (f, f' : finfo * finfo) : unit =
  let padding = ref ((List.length f.args) - (List.length f'.args))
  in
    if (!padding == 0) then ()
    else
      let to_pad = if (!padding > 0) then f' else (padding := -(!padding);f)
      in
	for i = 1 to (!padding) do
	  to_pad.args <- to_pad.args @ [fresh_var false]
	done
	
let pad_args2 (fi, tlr : finfo * tau list ref) : unit = 
  let padding = ref ((List.length fi.args) - (List.length (!tlr)))
  in
    if (!padding == 0) then ()
    else 
      if (!padding > 0) then 
	for  i = 1 to (!padding) do
	  tlr := (!tlr) @ [fresh_var false]
	done
      else
	begin
	  padding := -(!padding);
	  for i = 1 to (!padding) do
	    fi.args <- fi.args @ [fresh_var false]
	  done
	end

(***********************************************************************)
(*                                                                     *)
(* Constraint Generation/ Resolution                                   *)
(*                                                                     *)
(***********************************************************************)

let set_top (b : bool) (t : tau) : unit =
  let set_top_down t =
    match (find t) with
      | Var v -> v.v_top <- b
      | Ref r -> set_top_c_absloc r.rl b
      | Fun f -> set_top_c_absloc f.fl b
      | Pair p -> ()
  in
    iter_tau set_top_down t

let rec unify_int (t, t' : tau * tau) : unit =
  if (equal_tau t t') then ()
  else
    let ti,ti' = find t, find t' in
      begin
	U.unify combine (t,t');
	match ti,ti' with
	  | Var v, Var v' ->
	      begin
		set_top (v.v_top || v'.v_top) t';
		merge_v_lbounds (v,v');
		merge_v_ubounds (v,v')
	      end
	  | Var v, _ ->
	      begin
		set_top (v.v_top || top_tau t') t';
		notify_vlbounds t v;
		notify_vubounds t v
	      end
	  | _, Var v ->
	      begin
		set_top (v.v_top || top_tau t) t;
		notify_vlbounds t' v;
		notify_vubounds t' v
	      end
	  | Ref r, Ref r' ->
	      begin
		unify_ref (r,r')
	      end
	  | Fun f, Fun f' ->
	      begin
		unify_fun (f,f')
	      end
	  | Pair p, Pair p' ->
	      begin
		unify_pair(p,p')
	      end
	  |  _ -> raise Inconsistent
      end

and notify_vlbounds (t : tau) (vi : vinfo) : unit =
  let notify bounds = 
    List.iter (fun b -> 
		 add_constraint (Unification (b.info,copy_toplevel t));
		 add_constraint (Leq (b.info,t))
	      ) bounds
  in
    notify (B.elements vi.v_lbounds);

and notify_vubounds (t : tau) (vi : vinfo) : unit =
  let notify bounds = 
    List.iter (fun b -> 
		 add_constraint (Unification (b.info,copy_toplevel t));
		 add_constraint (Leq (t,b.info))
	      ) bounds
  in
    notify (B.elements vi.v_ubounds);

and unify_ref (ri,ri' : rinfo * rinfo) : unit =
  begin
    unify_c_abslocs(ri.rl,ri'.rl);
    add_constraint (Unification (ri.points_to, ri'.points_to))
  end

and unify_fun (fi, fi' : finfo * finfo) : unit =
  let rec union_args  = function
    | _, [] -> false
    | [], _ -> true
    | h :: t, h' :: t' -> 
	add_constraint (Unification (h,h')); union_args(t,t') 
  in
    begin
      unify_c_abslocs(fi.fl,fi'.fl);
      add_constraint (Unification (fi.ret,fi'.ret));
      if (union_args(fi.args,fi'.args)) 
      then fi.args <- fi'.args;
    end

and unify_pair (pi,pi' : pinfo * pinfo) : unit = 
  begin
    add_constraint (Unification (pi.ptr,pi'.ptr));
    add_constraint (Unification (pi.lam,pi'.lam))
  end

and unify_c_abslocs (l,l' : c_absloc * c_absloc) : unit = 
  let pick_name (li,li' : c_abslocinfo * c_abslocinfo) = 
    if ( (String.length li.l_name) > 1 && (String.sub (li.l_name) 0 2) = "l_") 
    then 
      li.l_name <- li'.l_name
    else ()
  in
  let combine_c_absloc (li,li' : c_abslocinfo *c_abslocinfo) : c_abslocinfo =
      begin
	pick_name(li,li');
	li.l_top <- li.l_top || li'.l_top;
	li.aliases <- C.union (li.aliases) (li'.aliases);
	li.ubounds <- B.union li.ubounds (li'.ubounds);
	li.lbounds <- B.union li.lbounds (li'.lbounds);
	li
      end
  in
    (if (!debug_constraints) then
       Printf.printf "%s == %s\n" (string_of_c_absloc l) (string_of_c_absloc l'));
    U.unify combine_c_absloc (l,l')

and merge_v_lbounds (vi,vi' : vinfo * vinfo) : unit =
  vi'.v_lbounds <- B.union vi.v_lbounds vi'.v_lbounds;

and merge_v_ubounds (vi,vi' : vinfo * vinfo) : unit =
  vi'.v_ubounds <- B.union vi.v_ubounds vi'.v_ubounds;

(** Pick the representative info for two tinfo's. This function prefers the
  first argument when both arguments are the same structure, but when
  one type is a structure and the other is a var, it picks the structure. 
  All other actions (e.g., updating the info) is done in unify_int *)
and combine (ti,ti' : tinfo * tinfo) : tinfo = 
  match ti,ti' with
    | Var _, _ -> ti'
    | _,_ -> ti 

and leq_int (t,t') : unit = 
  if (equal_tau t t') then ()
  else
    let ti,ti' = find t, find t' in
      begin
	match ti,ti' with
	  | Var v, Var v' ->
	      begin
		v.v_ubounds <- B.add (make_tau_bound(t')) v.v_ubounds;
		v'.v_lbounds <- B.add (make_tau_bound(t)) v'.v_lbounds
	      end
	  | Var v, _ ->
	      begin
		add_constraint (Unification (t, copy_toplevel t'));
		add_constraint (Leq (t,t'))
	      end
	  | _, Var v ->
	      begin
		add_constraint (Unification (t', copy_toplevel t));
		add_constraint (Leq (t,t'))
	      end
	  | Ref r, Ref r' ->
	      begin
		leq_ref(r,r')
	      end
	  | Fun f, Fun f' -> (** TODO : check, why not do subtyping here? *)
	      add_constraint (Unification (t,t'))
	  | Pair pr, Pair pr' ->
	      begin
		add_constraint (Leq (pr.ptr,pr'.ptr)); 
		add_constraint (Leq (pr.lam,pr'.lam))
	      end
	  | _ -> raise Inconsistent
      end

and leq_ref (ri,ri') : unit = 
  begin
    leq_c_absloc(ri.rl,ri'.rl);
    add_constraint (Unification (ri.points_to,ri'.points_to))
  end

and leq_c_absloc (l,l') : unit = 
  if (!debug_constraints) then
    Printf.printf "%s <= %s\n"
      (string_of_c_absloc l)  (string_of_c_absloc l');
  let li,li' = find l,find l' in
    if ( U.equal (l,l')) then () 
    else
      begin
	li.ubounds <- B.add (make_bound(l')) li.ubounds;
	li'.lbounds <- B.add (make_bound(l)) li'.lbounds
      end

and add_constraint_int (c : tconstraint) (toplev : bool) =
  if (!debug_constraints && toplev)
  then  
    begin
      Printf.printf "%d:>" (!toplev_count); 
      print_constraint c;
      incr toplev_count
    end
  else
    if (!debug_constraints)
    then 
      print_constraint c
    else (); 
  begin
    insist ( can_add_constraints () ) "can't add constraints";
    match c with
      | Unification _ ->
	  Q.add c eq_worklist
      | Leq _ ->
	  Q.add c leq_worklist
  end;
  solve_constraints() (* solve online *)

and add_constraint (c : tconstraint) = 
  add_constraint_int c false

and add_toplev_constraint (c : tconstraint) = 
  if (!print_constraints && not (!debug_constraints)) 
  then 
    ((Printf.printf "%d:>" (!toplev_count));
     incr toplev_count; print_constraint c) else ();
  add_constraint_int c true

and fetch_constraint () : tconstraint option =
  try
    Some (Q.take eq_worklist)
  with
    | Q.Empty ->
	begin
	  try 
	    Some (Q.take leq_worklist)
	  with
	    | Q.Empty ->
		None
	end
	
(** The main solver loop. *)
and solve_constraints () : unit = 
  match fetch_constraint () with
    | Some c ->
	begin
	  (match c with
	     | Unification (t,t') -> unify_int (t,t')
	     | Leq (t,t') -> 
		 if (!no_sub) 
		 then unify_int(t,t') 
		 else
		   leq_int(t,t')
	  );
	  solve_constraints()
	end
    | None -> ()

(***********************************************************************)
(*                                                                     *)
(* Interface Functions                                                 *)
(*                                                                     *)
(***********************************************************************)

(** Return the contents of the lvalue. *)
let rvalue (lv : lvalue) : tau = 
  lv.contents

(** Dereference the rvalue. If it does not have enough structure to support
  the operation, then the correct structure is added via new unification
  constraints. *)
let rec deref (t : tau) : lvalue =
  match find t with 
    | Pair p ->
	(
	  match find (p.ptr) with
	    | Var _ ->
		begin 
		  let is_top = top_tau p.ptr in
		  let points_to = fresh_var is_top in 
		  let l = fresh_c_absloc is_top in
		  let r = make_ref(l,points_to)
		  in
		    add_toplev_constraint (Unification (p.ptr,r));
		    make_lval(l, points_to)
		end
	    | Ref r -> make_lval(r.rl, r.points_to)
	    | _ -> raise WellFormed
	)
    | Var v -> 
	begin
	  let is_top = top_tau t in
	    add_toplev_constraint 
	      (Unification (t,make_pair(fresh_var is_top,
					fresh_var is_top)) );
	    deref t
	end
    | _ -> raise WellFormed


(** Form the union of [t] and [t'], if it doesn't exist already. *)
let join (t : tau) (t' : tau) : tau = 
 let found = H.mem join_cache (get_stamp t, get_stamp t') in	
  begin
    if (found) then 
        H.find join_cache (get_stamp t, get_stamp t')
    else
     begin
        let t''  = fresh_var false in
        add_toplev_constraint (Leq (t,t''));
        add_toplev_constraint (Leq (t',t''));
        H.add join_cache (get_stamp t, get_stamp t') t'';
        t''
     end
  end

(** Form the union of a list [tl], expected to be the initializers of some
  structure or array type. *)
let join_inits (tl : tau list) : tau = 
  let t' = fresh_var false in
    begin
      List.iter (function t -> add_toplev_constraint (Leq(t,t'))) tl;
      t'
    end

(** Take the address of an lvalue. Does not add constraints. *)
let address (lv  : lvalue) : tau = 
  make_pair (make_ref (lv.l, lv.contents), fresh_var false )

(** No instantiation in this analysis *)
let instantiate (lv : lvalue) (i : int) : lvalue = 
  lv

(** Constraint generated from assigning [t] to [lv]. *)
let assign (lv : lvalue) (t : tau) : unit = 
  add_toplev_constraint (Leq (t,lv.contents))

let assign_ret (i : int) (lv : lvalue) (t : tau) : unit =
  add_toplev_constraint (Leq (t,lv.contents))

(** Project out the first (ref) component or a pair. If the argument [t] has
  no discovered structure, raise NoContents. *)
let proj_ref (t : tau) : tau = 
  match find t with
    | Pair p -> p.ptr
    | Var v -> raise NoContents
    | _ ->  raise WellFormed

(* Project out the second (fun) component of a pair. If the argument [t] has
 no discovered structure, create it on the fly by adding constraints. *)
let proj_fun (t : tau) : tau = 
  match find t with
    | Pair p -> p.lam
    | Var v -> 
	let p,f = fresh_var false, fresh_var false in
	  add_toplev_constraint (Unification (t,make_pair(p,f)));
	  f
    | _ -> raise WellFormed

let get_args (t : tau) : tau list =
  match find t with 
    | Fun f -> f.args
    | _ -> raise WellFormed

let get_finfo (t : tau) : finfo =
  match find t with
    | Fun f -> f
    | _ -> raise WellFormed

(** Function type [t] is applied to the arguments [actuals]. Unifies the 
  actuals with the formals of [t]. If no functions have been discovered for 
  [t] yet, create a fresh one and unify it with t. The result is the return
  value of the function plus the index of this application site. 
  
  For this analysis, the application site is always 0 
*)
let apply (t : tau) (al : tau list) : (tau * int) = 
  let f = proj_fun(t) in
  let actuals = ref al in
  let fi,ret = 
    match find f with
      | Fun fi -> fi,fi.ret
      | Var v -> 
	  let new_l,new_ret,new_args = 
	    fresh_c_absloc false, fresh_var false, 
	    List.map (function _ -> fresh_var false) (!actuals) 
	  in
	  let new_fun = make_fun(new_l,new_args,new_ret) in
	    add_toplev_constraint (Unification(new_fun,f));
	    (get_finfo new_fun,new_ret)
      | _ -> raise WellFormed
  in
    pad_args2(fi,actuals);
    List.iter2 (fun actual -> fun formal -> 
		  add_toplev_constraint (Leq (actual,formal))
	       ) !actuals fi.args;
    (ret,0)

let make_undefined_lvalue () = 
  make_lval(make_c_absloc false "undefined" None, make_var true "undefined")

let make_undefined_rvalue () = 
  make_var true "undefined"

let assign_undefined (lv : lvalue) : unit = 
  assign lv (make_undefined_rvalue ())

let apply_undefined (al : tau list) : (tau * int) = 
  begin
    List.iter 
      (fun actual ->  assign (make_undefined_lvalue ()) actual) al;
   (fresh_var true,0)
  end

(** Create a new function type with name [name], list of formal arguments 
  [formals], and return value [ret]. Adds no constraints. *)
let make_function (name : string) (formals : lvalue list) (ret : tau) : tau = 
  let 
    f = make_fun (make_c_absloc false name None, List.map (fun x -> rvalue x
						     ) formals,ret) 
  in
    make_pair(fresh_var false,f)

(** Create an lvalue. *)
let make_lvalue (b : bool ) (name : string) (vio : Cil.varinfo option) =
  make_lval(make_c_absloc false name vio, make_var false name) 
 

(** Create a fresh named variable. *)
let make_fresh (name : string) : tau =
  make_var false name

(** The default type for abslocs. *)
let bottom () : tau = 
  make_var false ("bottom")

(** Unify the result of a function with its return value. *)
let return (t : tau) (t' : tau) =
    add_toplev_constraint (Leq (t',t))

(***********************************************************************)
(*                                                                     *)
(* Query/Extract Solutions                                             *)
(*                                                                     *)
(***********************************************************************)

(** todo : reached_top !! *)
let collect_ptset_fast (l : c_absloc) : abslocset = 
  let onpath : (int, int) H.t = H.create 4 in
  let path : c_absloc list ref = ref [] in
  let compute_path (i :int) = 
    keep_until (fun l -> i = (get_c_absloc_stamp l))  (!path) 
  in
  let collapse_cycle (cycle : c_absloc list) = 
    match cycle with
      | l :: ls -> 
	  List.iter (fun l' -> unify_c_abslocs (l,l')) ls; C.empty
      | [] -> die "collapse cycle"
  in
  let rec flow_step (l : c_absloc) : abslocset = 
    if (H.mem onpath (get_c_absloc_stamp l)) (* already seen *)
    then collapse_cycle (compute_path (get_c_absloc_stamp l)) 
    else
      let li = find l in
	begin
	  H.add onpath (get_c_absloc_stamp l) (get_c_absloc_stamp l);
	  path := l :: (!path);
	  B.iter (fun lb -> li.aliases <- C.union li.aliases
		    (flow_step lb.info) ) li.lbounds;
	  path := List.tl (!path);
	  H.remove onpath (get_c_absloc_stamp l);
	  li.aliases
	end
  in
    insist (can_query_graph ()) "flow_step can't query graph";
    if (get_flow_computed l) then (get_aliases l)
    else
      begin
	set_flow_computed l;
	flow_step l
      end

(** this is a quadratic flow step. keep it for debugging 
  the fast version above. 
*)
let collect_ptset_slow (l : c_absloc) : abslocset = 
  let onpath : (int,int) H.t = H.create 4 in
  let rec flow_step (l : c_absloc) : abslocset = 
    if (top_c_absloc l) 
    then raise ReachedTop
    else 
      begin
	if (H.mem onpath (get_c_absloc_stamp l))
	then C.empty 
	else 
	  let li = find l in
	    begin
	      H.add onpath (get_c_absloc_stamp l) (get_c_absloc_stamp l);
	      B.iter 
		(fun lb -> li.aliases <- C.union li.aliases
		   (flow_step lb.info) ) li.lbounds;
	      li.aliases
	    end
      end
  in
    begin
      insist (can_query_graph ()) "collect_ptset_slow can't query graph";
      if (get_flow_computed l) then
	(get_aliases l)
      else  
	begin
	  set_flow_computed l;
	  flow_step l
	end
    end  

let collect_ptset = 
     collect_ptset_slow 
(* if (!debug_flow_step) then collect_ptset_slow
  else collect_ptset_fast
*)

let may_alias (t1 : tau) (t2 : tau) : bool =
  try 
    let l1 =
      begin
	match (find (proj_ref t1)) with
	  | Ref r -> r.rl
	  | Var v -> raise NoContents
	  | _ ->  raise WellFormed
      end
    in
    let l2 =
      begin  
	match (find (proj_ref t2)) with
	  | Ref r -> r.rl
	  | Var v -> raise NoContents
	  | _ ->raise WellFormed
      end
    in
      (equal_c_absloc l1 l2) or (not (C.is_empty (C.inter (collect_ptset l1) (collect_ptset l2))))
  with
    | NoContents -> false
    | ReachedTop -> raise UnknownLocation

let points_to_aux ( t : tau) : absloc list = 
  try
    (match (find (proj_ref t)) with
      | Var v -> []
      | Ref r -> 
	  begin
	    (C.elements (collect_ptset r.rl))
	  end
      | _ -> raise WellFormed)
  with 
    | NoContents -> []
    | ReachedTop -> raise UnknownLocation

let points_to (lv : lvalue) : Cil.varinfo list = 
  let rec get_vinfos l : Cil.varinfo list = match l with
    | (_,_,Some h) :: t -> h :: (get_vinfos t) 
    | (_,_,None) :: t -> (get_vinfos t)
    | [] -> []
  in
    get_vinfos (points_to_aux lv.contents)

let epoints_to (t : tau) : Cil.varinfo list =
  let rec get_vinfos l : Cil.varinfo list = match l with
    | (_,_,Some h) :: t -> h :: (get_vinfos t) 
    | (_,_,None) :: t -> (get_vinfos t)
   | [] -> []
  in
    get_vinfos (points_to_aux t)

(* let points_to_names (lv : lvalue) : string list =
  List.map (fun (_,str,_) -> str) (points_to_aux lv.contents)
*)
let points_to_names (lv : lvalue) : string list =
  List.map (fun v -> v.vname) (points_to lv)

let absloc_points_to (lv : lvalue) : absloc list = 
  points_to_aux (lv.contents)
  
let absloc_epoints_to (t : tau) : absloc list = points_to_aux t

let absloc_of_lvalue (lv : lvalue) : absloc = (find lv.l).loc

let absloc_eq = equal_absloc



