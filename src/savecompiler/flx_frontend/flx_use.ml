open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexe
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_unify
open Flx_exceptions
open Flx_bid

(* These routines find the absolute use closure of a symbol,
in particular they include variables which are initialised
but never used: these routine are intended to be used
to extract all the bound symbol table entries required
to process a set of roots.

Contrast with the 'Flx_call' usage routines, which
find some symbols which are useful, this excludes
types, and it excludes LHS vals and perhaps vars,
which are not used in some expression.

It seems a pity these routines are almost identical
(and the lot gets repeated yet again in the instantiator,
and weakly in the 'useless call eliminator', we hope
to find a better code reuse solution.. for now,
remember to update all three sets of routines when
changing the data structures.

*)

exception NotFoundDefn of int

let is_param bsym_table idx =
  let parent,bsym = Flx_bsym_table.find_with_parent bsym_table idx in
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_val _ ->
    begin match parent with
    | Some p when p <> 0 ->
      let bbdcl = Flx_bsym_table.find_bbdcl bsym_table p in
      begin match bbdcl with
      | BBDCL_fun (_,_,bps,_,_,_) ->
        let bids = Flx_bparams.get_bids bps in
        List.mem idx bids
      | _ -> false
      end
    | _ -> false
    end
  | _ -> false

let is_once_param bsym_table idx =
  let parent,bsym = Flx_bsym_table.find_with_parent bsym_table idx in
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_val (_,t,_) ->
    Flx_btype.contains_uniq t && is_param bsym_table idx 
  | _ -> false


let rec istriv t = match Flx_btype.trivorder t with
  | Some _ -> true
  | None -> false

let rec uses_btype add bsym_table count_inits t =
  let f_btype t = uses_btype add bsym_table count_inits t in
 
  (* We only care about insts. *)
  match t with
  | BTYP_inst (i,ts,_) ->
      add i;
      List.iter f_btype ts

  | _ -> Flx_btype.flat_iter ~f_btype t

and uses_bexe' add bsym_table count_inits exe =
  let sr = Flx_bexe.get_srcref exe in
  let f_bexpr e = uses_bexpr add bsym_table count_inits e in

  let rec chkl e = 
    match e with
    | _,t when istriv t -> ()

    | BEXPR_deref ((BEXPR_ref _),_ as p),_ -> 
      print_endline "Deref a ref in lcontext: variable not considered used";
      print_endline ("In assignment " ^ string_of_bexe bsym_table 0 exe);
      ()

    | BEXPR_deref _,_ -> 
      (*
      print_endline "Can't handle deref yet";
      print_endline ("In assignment " ^ string_of_bexe bsym_table 0 exe);
      *) 
      f_bexpr e

    | BEXPR_ref _,_ -> 
      print_endline "Can't handle address yet, assume variable is used";
      print_endline ("In assignment " ^ string_of_bexe bsym_table 0 exe);
      assert false;
      f_bexpr e

    | BEXPR_case _,_ -> () (* case used as projection *)
    | BEXPR_varname _,_ -> ()

    | BEXPR_prj (j,_,_),_ -> ()
    | BEXPR_inj (j,_,_),_ -> ()

    | BEXPR_apply ((BEXPR_closure (i,_),_),_),_ 
    | BEXPR_apply_prim (i,_,_),_ -> 
      let bsym = Flx_bsym_table.find bsym_table i in
      let bbdcl = Flx_bsym.bbdcl bsym in 
      begin match  bbdcl with
      | Flx_bbdcl.BBDCL_external_fun (props,_,_,_,_,_,_) ->
        begin 
          clierrx "[flx_frontend/flx_use.ml:92: E369] " (Flx_bexe.get_srcref exe)
          ("Flx_use: In statement " ^ string_of_bexe bsym_table 0 exe ^ "\n" ^
          "Expected primitive " ^ Flx_bsym.id bsym ^ "<" ^ si i ^ "> to have property lvalue")
        end
      | _ -> 
          clierrx "[flx_frontend/flx_use.ml:97: E370] " (Flx_bexe.get_srcref exe)
          ("Flx_use: In statement " ^ string_of_bexe bsym_table 0 exe ^ "\n" ^
          "variable required on LHS of operation "^ Flx_bsym.id bsym)
      end

    | BEXPR_apply ((BEXPR_prj _,_),b),_ -> chkl b
    | BEXPR_apply ((BEXPR_aprj (ix,_,_),_),b),_ -> f_bexpr ix; chkl b
 
    | BEXPR_apply (a,b),_ -> 
      print_endline ("[Flx_use.uses_bexe:assign:lhs] Unexpected apply " ^ sbe bsym_table e);
      print_endline ("In assignment " ^ string_of_bexe bsym_table 0 exe);
      assert false;
      f_bexpr a; f_bexpr b

    | _,BTYP_tuple [] ->
      print_endline ("[Flx_use] Unexpected unit assignment " ^ string_of_bexe bsym_table 0 exe);

    | _ -> 
      print_endline ("[Flx_use] Unexpected " ^ sbe bsym_table e);
      print_endline ("[Flx_use] In assignment " ^ string_of_bexe bsym_table 0 exe);
      print_endline ("[flx_use] In\n" ^ Flx_srcref.long_string_of_src sr);

  in
  match exe,count_inits with
  | BEXE_init (_,i,rhs),_ 
  | BEXE_assign (_,(BEXPR_varname (i,[]),_),rhs),_ ->
    if count_inits || is_once_param bsym_table i then add i;
    f_bexpr rhs 

  | BEXE_assign (_,lhs,rhs),_ ->
print_endline ("Flx_use: Assign to non variable detected!");
print_endline (sbx bsym_table exe);
assert false;
    (* check is a term is a tuple projection of a variable *)
    (*if count_inits then *) f_bexpr lhs
    (* else chkl lhs *)
    ;
    f_bexpr rhs

  | BEXE_label _,false -> ()
  | _ ->

      Flx_bexe.iter
        ~f_bid:(add)
        ~f_btype:(uses_btype add bsym_table count_inits)
        ~f_bexpr
        exe

and uses_bexe add bsym_table count_inits exe =
  try
    uses_bexe' add bsym_table count_inits exe;
  with
    NotFoundDefn i ->
      failwith ("[Flx_use.uses_bexe] Cannot find bound defn for <" ^ string_of_bid i ^ "> in\n" ^
      string_of_bexe bsym_table 0 exe
   )


and uses_bexpr add bsym_table count_inits ((e,t) as x) =
  Flx_bexpr.iter
    ~f_bid:(add)
    ~f_btype:(uses_btype add bsym_table count_inits)
    x

and uses_symbol add bsym_table count_inits i =
    let xbbdcl =
      try Some (let bsym = Flx_bsym_table.find bsym_table i in bsym,Flx_bsym.bbdcl bsym)
      with Not_found -> None
    in
    match xbbdcl with
    | Some (bsym,bbdcl) ->
(*
print_endline ("  VVVV START Flx_use.uses processing index " ^ si i ^ " symbol " ^ Flx_bsym.id bsym);
*)
        Flx_bbdcl.iter
          ~f_bid:(add)
          ~f_btype:(uses_btype add bsym_table count_inits)
          ~f_bexpr:(uses_bexpr add bsym_table count_inits)
          ~f_bexe:(uses_bexe add bsym_table count_inits)
          bbdcl;
(*
print_endline ("  ^^^^ END   Flx_use.uses processing index " ^ si i ^ " symbol " ^ Flx_bsym.id bsym);
*)
      
    | None ->
        raise (NotFoundDefn i)

let find_roots syms bsym_table (root: int option) bifaces =
  (* make a list of the root and all exported functions,
  add exported types and components thereof into the used
  set now too
  *)
  let roots = ref (BidSet.empty) in
  let add i = roots := BidSet.add i (!roots) in
  begin match root with
  | None -> ()
  | Some p -> add p
  end;

  List.iter begin function
  | BIFACE_export_python_fun (_,x,_)
  | BIFACE_export_fun (_,x,_) 
  | BIFACE_export_cfun (_,x,_) -> add x;
  | BIFACE_export_type (_,t,_) -> uses_btype add bsym_table true t
  | BIFACE_export_struct (_,idx) -> add idx
  | BIFACE_export_union (_,idx,_) -> add idx
  | BIFACE_export_requirement (_,breqs) ->
     List.iter (fun (idx, ts) -> 
       add idx; 
       List.iter (fun t->uses_btype add bsym_table true t) ts
     )
     breqs 
  end bifaces;

  (* keep primitives in concordance *)
  for i= 100 to Flx_bid.start_counter - 1 do
    if Flx_bsym_table.mem bsym_table i then begin
(*
print_endline ("Concordance index " ^ string_of_int i ^ " made root");
*)
      add i
    end
  done;
  syms.roots := !roots

let cal_use_closure syms bsym_table (count_inits:bool) =
(*
print_endline ("---------------------------------");
print_endline ("Cal use closure...");
*)
  let usecount = Hashtbl.create 97 in
  let addcount i = 
     Hashtbl.replace usecount i 
      begin try Hashtbl.find usecount i + 1 
       with Not_found -> 1
      end
  in
  let traced = ref BidSet.empty in (* final set of used symbols *)

  let roots = !(syms.roots) in
  (* add coercion types and functions to roots *)
  let roots = Flx_bsym_table.fold_coercions bsym_table
    (fun acc ((a,b),c) -> 
      let x = BidSet.add a acc in
      let y = BidSet.add b x in
      let z = BidSet.add c y in
      z
    )
    roots
  in

  let v : BidSet.t = roots in (* used but not traced yet *)
  let untraced = ref v in
(*
  print_endline "Roots";
  BidSet.iter (fun i ->
    print_endline ("Root " ^ string_of_int i)
  )
  (!untraced)
  ;
*)
  let add' bid =
(*
if Flx_bsym_table.find_id bsym_table bid = "yyyy" then
print_endline ("Add count for " ^ Flx_bsym_table.find_id bsym_table bid  ^ "<"^string_of_int bid^">" ^
", isparam= " ^ string_of_bool (is_param bsym_table bid)
);
*)
    addcount bid;
    if not (BidSet.mem bid !traced) && not (BidSet.mem bid !untraced) then begin
(*
      print_endline ("Keeping " ^ string_of_int bid );
*)
      begin try
        let bsym = Flx_bsym_table.find bsym_table bid in
        ()
      with Not_found -> print_endline ("Flx_use: Woops, can't find a symbol we're keeping? " ^ string_of_int bid)
      end;
      untraced := BidSet.add bid !untraced;
    end
  in
  let ut t = uses_btype add' bsym_table count_inits t in
  let add bid = 
    if bid <> 0 then begin
(*
print_endline ("Flx_use:cal_use_closure: Adding bid " ^ si bid);
*)
      add' bid;
(*
      try 
        let entries = Hashtbl.find syms.virtual_to_instances bid in
assert (List.length entries = 0); (* THIS IS OLD CODE ... ? *)
        List.iter begin fun (vs,con,ts,j) ->
          add' j;
          ut con;
          List.iter ut ts
        end entries
      with Not_found -> ()
*)
    end
  in

  (* Register use of the typeclass instances. *)
(*
  print_endline "Instance of typeclass";
*)
(*
  if (Hashtbl.length syms.instances_of_typeclass) <> 0 then
    failwith "Typeclasses not eliminated"
  ;
  Hashtbl.iter begin fun i entries ->
    add i;
    List.iter begin fun (j, (vs, con, ts)) ->
      add j;
      ut con;
      List.iter ut ts
    end entries
  end syms.instances_of_typeclass;
*) 
(*

THIS CRAP IS HERE FOR THIS REASON: A symbol X may be unused,
but a reduction Y -> X performed later then makes X used.
So if we're going to apply reductions, the symbols on the RHS
of the reduction have to be retained if the symbols on the LHS
are, in case the LHS matches.

I'm going to skip this for the moment!



  (* process reductions. assume temporarily that useless ones
    have been removed. Check later this is right. This is a 
    nasty routine here, adds stuff that cannot match because it
    is based on the input table (which contains garbage).
    However it's not trivial because a reduction whose LHS has
    symbols not at this time in the output table could still match
    the RHS of a reduction.
  *)

  (* Reduction parameters don't exist, if a reduction is applied
     the parameter is substituted with the argument.
  *)
  let maybe_add ignores j = 
    if not (List.mem j ignores) then add j
  in
(*
  if List.length (!(syms.reductions)) <> 0 then
     failwith ("Reductions exist!!")
  ;
*)
  List.iter
  (fun (id,bvs,bps,lhs, rhs) ->
    let ignorelist = List.map (fun p -> p.Flx_bparameter.pindex) bps in
    uses_bexpr (maybe_add ignorelist) bsym_table count_inits rhs;
  )
  !(syms.reductions)
  ;
*)

  while not (BidSet.is_empty !untraced) do
    let bid = BidSet.choose !untraced in
(*
    print_endline ("Tracing " ^ string_of_int bid);
*)
    untraced := BidSet.remove bid !untraced;
    traced := BidSet.add bid !traced;
    uses_symbol add bsym_table count_inits bid
  done;
(*
print_endline ("DONE cal_use_closure <<<<<<<<<<<<<<<<<<<<<<<<<<<<<");
*)
  !traced

let full_use_closure syms bsym_table =
  cal_use_closure syms bsym_table true

exception Bad

let keep bsym_table bidset exe =
  match exe with
  | BEXE_assign (sr,((_,t) as lhs),rhs) ->
    if istriv t then false
    else
    let add i = if BidSet.mem i bidset then () else raise Bad in
    begin try uses_bexe add bsym_table true exe; true
    with Bad -> false
    end

  | exe ->
    let add i = if BidSet.mem i bidset then () else raise Bad in
    begin try uses_bexe add bsym_table true exe; true
    with Bad -> false
    end

let strip_inits bsym_table bidset exes =
  let rec aux exes_in exes_out =
    match exes_in with
    | [] -> List.rev exes_out
    | exe::tail ->
      aux tail (if keep bsym_table bidset exe then (exe::exes_out) else exes_out)
  in
  aux exes [] 

let copy_used1' syms bsym_table =
(*
print_endline ("copy used ... ");
*)
  (* Calculate the used symbols. *)
  let bidset = cal_use_closure syms bsym_table false in

  (* Return a new bsym_table that has only the used symbols. *)
  let new_bsym_table = Flx_bsym_table.create_from bsym_table in

  (* Iterate through the used symbols and copy them to the new table. *)
  let rec aux bid =
    (* Exit early if we've already added the bid. *)
    if Flx_bsym_table.mem new_bsym_table bid then () else begin

      (* Try to add the parent if it's in the use list. *)
      let parent =
        match Flx_bsym_table.find_parent bsym_table bid with
        | None -> None
        | Some parent ->
            (* Only add the parent if we're in the use list. Otherwiser, just
             * turn the symbol into a top level symbol. *)
            if BidSet.mem parent bidset then begin
              aux parent;
              Some parent

            end else begin
(*
              if parent != 0 then 
              begin 
                print_endline ("Used symbols parent not marked as used: symbol: " ^ string_of_int bid ^
                 ", parent=" ^ string_of_int parent);
                let sym = Flx_bsym_table.find bsym_table bid in
                print_endline ("Symbol: " ^ Flx_bsym.id sym);

                try 
                   let psym = Flx_bsym_table.find bsym_table parent in
                   print_endline ("Parent: "^ Flx_bsym.id psym);
                   begin match Flx_bsym.bbdcl psym with
                   | BBDCL_module -> print_endline "MODULE"
                   | BBDCL_typeclass _ -> print_endline "TYPECLASS"
                   | _ -> ()
                   end
                   
                   with Not_found -> print_endline "Parent not in bsym_table"
              end
              ;
*)
              None
            end
      in

      let bsym = Flx_bsym_table.find bsym_table bid in
      let bsym =
        match bsym.Flx_bsym.bbdcl with 
        | BBDCL_fun  (prop, bvs, ps, res, effects, exes) ->  
(*
print_endline ("Flx_use: BEGIN Handling function " ^ Flx_bsym.id bsym);
*)
          let exes = strip_inits bsym_table bidset exes in
          let bbdcl = Flx_bbdcl.bbdcl_fun  (prop, bvs, ps, res, effects, exes) in
          let nubsym = Flx_bsym.create 
            ~sr:(bsym.Flx_bsym.sr) 
            bsym.Flx_bsym.id  
            bbdcl
          in
(*
print_endline ("Flx_use: END   Handling function " ^ Flx_bsym.id bsym);
*)
           nubsym
        | _ -> bsym
      in 

      (* Finally, add the symbol to the root. *)
      Flx_bsym_table.add new_bsym_table bid parent bsym
    end
  in

  (* Add all the symbols to the new symbol bsym_table. *)
  BidSet.iter aux bidset;

  (* Return the new symbol bsym_table. *)
  new_bsym_table

let copy_used1 syms bsym_table =
  try
    copy_used1' syms bsym_table
  with
    NotFoundDefn i ->
      failwith ("[Flx_use.uses] Cannot find bound defn for <" ^ string_of_bid i ^ ">")

let copy_used syms bsym_table =
  if syms.compiler_options.Flx_options.print_flag then begin
    print_endline "COPY USED";
    Flx_print.print_bsym_table bsym_table
   end;
  let rec aux bsym_table old =
    if syms.compiler_options.Flx_options.print_flag then
      print_endline ("Copy used1: ninput symbols = " ^ si old);
    let bsym_table = copy_used1 syms bsym_table in
    let nu = Flx_bsym_table.length bsym_table in
    assert (nu <= old);
    if nu = old then bsym_table else
    aux bsym_table nu
  in
  let result =  aux bsym_table (Flx_bsym_table.length bsym_table) in
  if syms.compiler_options.Flx_options.print_flag then
    print_endline "*** COPY DONE";
  Flx_bsym_table.validate "Flx_use: copy_used" result;
  result



