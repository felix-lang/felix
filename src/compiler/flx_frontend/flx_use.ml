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
open Flx_maps
open Flx_exceptions

let rec is_proj (e,t) =
  match e with
  | BEXPR_name _-> true
  | BEXPR_get_n (_,e) -> is_proj e
  | _ -> false

let rec get_var (e,t) =
  match e with
  | BEXPR_name (i,ts) -> i
  | BEXPR_get_n (j,e) -> get_var e 
  | _ -> assert false

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

let rec uses_btype add bsym_table count_inits t =
  let f_btype t = uses_btype add bsym_table count_inits t in
 
  (* We only care about inits. *)
  match t with
  | BTYP_inst (i,ts) ->
      add i;
      List.iter f_btype ts

  | _ -> Flx_btype.flat_iter ~f_btype t

and uses_bexe add bsym_table count_inits exe =
  let f_bexpr e = uses_bexpr add bsym_table count_inits e in

  match exe,count_inits with
  | BEXE_init (_,i,e),false -> f_bexpr e
  | BEXE_assign (_,lhs,rhs),_ ->
      (* check is a term is a tuple projection of a variable *)
      if count_inits or not (is_proj lhs)
      then f_bexpr lhs;
      f_bexpr rhs
  | _ ->

      Flx_bexe.iter
        ~f_bid:(add)
        ~f_btype:(uses_btype add bsym_table count_inits)
        ~f_bexpr
        exe

and uses_bexpr add bsym_table count_inits ((e,t) as x) =
  Flx_bexpr.iter
    ~f_bid:(add)
    ~f_btype:(uses_btype add bsym_table count_inits)
    x

and uses add bsym_table count_inits i =
    let bbdcl =
      try Some (Flx_bsym_table.find_bbdcl bsym_table i)
      with Not_found -> None
    in
    match bbdcl with
    | Some bbdcl ->
        Flx_bbdcl.iter
          ~f_bid:(add)
          ~f_btype:(uses_btype add bsym_table count_inits)
          ~f_bexpr:(uses_bexpr add bsym_table count_inits)
          ~f_bexe:(uses_bexe add bsym_table count_inits)
          bbdcl
      
    | None ->
        failwith ("[Flx_use.uses] Cannot find bound defn for <" ^
          string_of_bid i ^ ">")

let find_roots syms bsym_table root bifaces =
  (* make a list of the root and all exported functions,
  add exported types and components thereof into the used
  set now too
  *)
  let roots = ref (BidSet.singleton root) in
  let add i = roots := BidSet.add i (!roots) in

  List.iter begin function
  | BIFACE_export_python_fun (_,x,_)
  | BIFACE_export_fun (_,x,_) 
  | BIFACE_export_cfun (_,x,_) -> add x;
  | BIFACE_export_type (_,t,_) -> uses_btype add bsym_table true t
  end bifaces;

  syms.roots := !roots

let cal_use_closure syms bsym_table (count_inits:bool) =
  let traced = ref BidSet.empty in (* final set of used symbols *)
  let v : BidSet.t = !(syms.roots) in (* used but not traced yet *)
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
    if not (BidSet.mem bid !traced) && not (BidSet.mem bid !untraced) then begin
(*
      print_endline ("Keeping " ^ string_of_int bid);
*)
      untraced := BidSet.add bid !untraced;
    end
  in
  let ut t = uses_btype add' bsym_table count_inits t in
  let add bid =
    add' bid;
    try 
      let entries = Hashtbl.find syms.virtual_to_instances bid in
      List.iter begin fun (vs,con,ts,j) ->
        add' j;
        ut con;
        List.iter ut ts
      end entries
    with Not_found -> ()
  in

  (* Register use of the typeclass instances. *)
(*
  print_endline "Instance of typeclass";
*)
  Hashtbl.iter begin fun i entries ->
    add i;
    List.iter begin fun (j, (vs, con, ts)) ->
      add j;
      ut con;
      List.iter ut ts
    end entries
  end syms.instances_of_typeclass;
(*
  (* Register use for the typeclass instance functions. *)
  print_endline "typeclass to instance";
  Hashtbl.iter begin fun i entries ->
    add i;
    List.iter begin fun (vs,con,ts,j) ->
      add j;
      ut con;
      List.iter ut ts
    end entries
  end syms.virtual_to_instances;
*)
(*
  print_endline "Tracing untraced";
*)
  while not (BidSet.is_empty !untraced) do
    let bid = BidSet.choose !untraced in
(*
    print_endline ("Tracing " ^ string_of_int bid);
*)
    untraced := BidSet.remove bid !untraced;
    traced := BidSet.add bid !traced;
    uses add bsym_table count_inits bid
  done;

  !traced

let full_use_closure syms bsym_table =
  cal_use_closure syms bsym_table true


let strip_inits bidset exes =
  let rec aux exes_in exes_out =
    match exes_in with
    | [] -> List.rev exes_out
    | exe::tail ->
      match exe with
      | BEXE_init (sr,i,e) when not (Flx_types.BidSet.mem i bidset) ->
        (*
        print_endline ("Stripping init of variable " ^ string_of_int i);
        *)
        aux tail exes_out 
      | BEXE_assign (sr,lhs,rhs) when is_proj lhs && not (Flx_types.BidSet.mem (get_var lhs) bidset) ->
        (*
        let i = get_var lhs in
        print_endline ("Stripping assign of variable " ^ string_of_int i);
        *)
        aux tail exes_out 
      | _ -> aux tail (exe::exes_out) 
  in
  aux exes [] 

let copy_used1 syms bsym_table =
  if syms.compiler_options.Flx_options.print_flag then
    print_endline "COPY USED";

  (* Calculate the used symbols. *)
  let bidset = cal_use_closure syms bsym_table false in

  (* Return a new bsym_table that has only the used symbols. *)
  let new_bsym_table = Flx_bsym_table.create () in

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
            if Flx_types.BidSet.mem parent bidset then begin
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
        | BBDCL_fun  (prop, bvs, ps, res, exes) ->  
          let exes = strip_inits bidset exes in
          let bbdcl = Flx_bbdcl.bbdcl_fun  (prop, bvs, ps, res, exes) in
          Flx_bsym.create 
            ~sr:(bsym.Flx_bsym.sr) 
            ~vs:(bsym.Flx_bsym.vs) 
            bsym.Flx_bsym.id  
            bbdcl
        | _ -> bsym
      in 

      (* Finally, add the symbol to the root. *)
      Flx_bsym_table.add new_bsym_table bid parent bsym
    end
  in

  (* Add all the symbols to the new symbol bsym_table. *)
  Flx_types.BidSet.iter aux bidset;

  (* Return the new symbol bsym_table. *)
  new_bsym_table

let copy_used syms bsym_table =
  let rec aux bsym_table old =
    let bsym_table = copy_used1 syms bsym_table in
    let nu = Flx_bsym_table.length bsym_table in
    assert (nu <= old);
    if nu = old then bsym_table else
    aux bsym_table nu
  in
  aux bsym_table (Flx_bsym_table.length bsym_table)

