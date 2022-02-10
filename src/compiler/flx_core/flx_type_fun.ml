open Flx_util
open Flx_types
open Flx_btype
open Flx_mtypes2

open Flx_print
open Flx_typing
open Flx_unify
open Flx_maps
open Flx_btype_subst
open Flx_kind

(* BETA REDUCTION OF APPLICATION.

  The function must be a type function expression.
  No lookup is done. If it's an instance, the reduction
  cannot proceed.

  The type must also contain fixpoints already.

  If function contains a fixpoint term BTYP_fix,
  the counter must be adjusted by +2 due to stripping off the
  BTYP_type_apply term AND the BTYP_type_function term.
  Note this is a Felix weirdo. The fixpoint counts levels down
  to the binder, but the implicit binder is above so the counter is
  always non-positive. The gap is reduced by beta-reduction so the term
  has to be added to.

  NOTE: ERRORS. The argument can be anything (except contain a fixpoint that is not
  in the trail).

  If the function term is a BTYP_function, the reduction proceeds by substitution
  and fixpoint adjustment.

  Otherwise we just return the original type function application term unreduced.

  Three possible cases exist where a subsequent reduction will succeed:

  1. A type alias is replace by a type function term
  2. A type function instance is replace by a type function term
  3. A type variable is replaced by a type function term

  Other cases are universally an error. However we should only
  call this function is we actually have a type function term already.

*)



let adjust bsym_table t =
(*
print_endline ("Fixpoint adjust " ^ sbt bsym_table t);
*)
  Flx_btype_rec.adjust_fixpoint t

let cal_isrecfun calltag bsym_table termlist t1 t = 
      match t1 with 
      | BTYP_fix (j,mt) ->
(*
print_endline ("Called from " ^calltag^ ":");
print_endline ("Attempting to beta-reduce type function application with fn as fixpoint! ");
print_endline ("Application is " ^ sbt bsym_table t);

    print_endline ("Function = " ^ sbt bsym_table t1);
    print_endline ("Argument = " ^ sbt bsym_table t2);
*)
        let whole =
          let index = -2-j in
          if List.length termlist <= index then `Unred t1 
          else `Whole (List.nth termlist index )
        in
        begin match whole with
        | `Unred t -> 
           print_endline ("Fixpoint " ^ string_of_int j ^ 
             " not in trail, index = " ^string_of_int (-2-j) ^ "  called from " ^ calltag); 
           print_endline "Trail is: ";
           List.iter (fun t -> print_endline (sbt bsym_table t)) termlist;
           assert false; 
           false
        | `Whole ((BTYP_type_function _) as t) -> 
(*
print_endline ("Found fixpoint function in trail: " ^ sbt bsym_table t);
*)
          true
        | `Whole _ -> 
print_endline ("Found fixpoint NON function in trail???: " ^ sbt bsym_table t);
print_endline "Trail is:";
          List.iter (fun t -> print_endline (sbt bsym_table t)) termlist;
print_endline "We picked term:";
print_endline (sbt bsym_table (List.nth termlist (-2-j)));

          assert false;
          false
        end
      | _ -> false


let type_apply br beta_reduce' calltag counter bsym_table sr termlist t t1 t2 = 
(*
 print_endline ("Flx_beta: BTYP_type_apply\n  " ^ Flx_btype.st t1 ^ "\nto\n  " ^ 
  Flx_btype.st t2);
*)

(* NOT clear if this is OK or not *)
(* ITS NOT! WE REQUIRE LAZY EVALUATION! Substitutiuon first, THEN reduction! *)
(* NO NO NO. WE MUST BETA-REDUCE THE FUNCTION TERM BECAUSE IT MIGHT BE AN APPLICATION
ITSELF
*)


    let t1 = br t1 in
(*
    let t2 = br t2 in
*)
(* NOTE: Since beta now calls expand first .. these should never occur *)
(*
    let t1 = match t1 with
    | BTYP_finst (index, ks, dom, cod) ->
      begin try 
       let bsym = Flx_bsym_table.find bsym_table index in
       match bsym.bbdcl with
       | BBDCL_type_function (bks, t) -> t (* FIXME: assumes bks empty *)

       | _ -> raise Not_found
      with Not_found -> t1
      end      
    | BTYP_inst (`Alias, index, vs, k) ->
      begin try 
       let bsym = Flx_bsym_table.find bsym_table index in
       match bsym.bbdcl with
       | BBDCL_structural_type_alias (bvs, t) 
       | BBDCL_nominal_type_alias (bvs, t) ->
         let params = List.map2 (fun (s,index,_) t -> index, t) bvs vs in
         let t1 = list_subst counter params t1 in
         t1
       | _ -> raise Not_found
      with Not_found -> t1
      end    
    | t1 -> t1
    in
*)
    begin
    let m1 = Flx_btype_kind.metatype sr t1 in
    let m2 = Flx_btype_kind.metatype sr t2 in

    begin match m1 with
    | KIND_function (d,c) ->
      if Flx_kind.kind_ge [d, m2] then () else
      let _ = print_endline ("Function metatype domain doesn't agree with argument metatype") in
      Flx_exceptions.clierr sr
      ("Flx_beta: In application: " ^ 
       "\ntype apply requires domain of type function to agree with argument\n" ^
       "Domain kind=" ^ Flx_kind.sk d ^ 
       "\nArgument kind=" ^ Flx_kind.sk m2 ^
       "\n\nFunction = " ^ Flx_btype.st t1 ^
       "\n\nArgument =" ^ Flx_btype.st t2 ^ ", kind=" ^ Flx_kind.sk m2)

    | _ -> print_endline ("Function metatype isn't function"); 
      Flx_exceptions.clierr sr 
      ("Flx_beta: type apply requires first argument to be KIND_function, got\n" ^ 
       "Type="^ Flx_btype.st t1 ^", kind=" ^ Flx_kind.sk m1)
    end;

(*
print_endline ("Flx_beta " ^calltag ^ " Attempting to beta-reduce type function application " ^ sbt bsym_table t);
*)
    let isrecfun = cal_isrecfun calltag bsym_table termlist t1 t in 
    let getrecfun () =
      match t1 with 
      | BTYP_fix (j,mt) -> List.nth termlist (-2-j)
      | _ -> assert false
    in
    let isrec = 
      if isrecfun then
        let fn = getrecfun () in
(*
print_endline ("got recfun = " ^ sbt bsym_table fn);
*)
        let arg = match fn with 
          | BTYP_type_function ([i,mt],ret,body) -> btyp_type_var (i,mt)
          | BTYP_type_function (ls,ret,body) -> 
             btyp_type_tuple (List.map (fun (i,mt) -> btyp_type_var (i,mt)) ls)
          | _ -> assert false
        in
        type_eq bsym_table counter arg t2
      else false
    in
(*
print_endline ("Calculated isrec= " ^ if isrec then "true" else "false");
*)
    let getmt () = 
       match getrecfun () with 
       | BTYP_type_function (_,ret,_) -> ret 
       | _ -> assert false 
    in
    if isrec then 
       match t1 with 
       | BTYP_fix (j,_) -> 
         print_endline "Calulcating recursive type";
         btyp_fix (j+1) (getmt())
       | _ -> assert false
    else 
    let t1 = if isrecfun then  getrecfun () else unfold "flx_beta" t1 in
    begin match t1 with
    | BTYP_type_function (ps,r,body) ->
(*
print_endline ("Attempting to apply type function " ^ Flx_print.sbt bsym_table t1 ^ "\n");
print_endline ("To argument " ^ Flx_print.sbt bsym_table t2^ "\n");
*)
      let params' =
        match ps with
        | [] -> []
        | [i,_] -> [i,t2]
        | _ ->
          let ts = match br t2 with
          | BTYP_type_tuple ts -> ts
          | _ -> 
print_endline ("Expected Argument  to type function to be type tuple, got " ^ Flx_print.sbt bsym_table t2);
assert false
          in
            if List.length ps <> List.length ts
            then failwith "Wrong number of arguments to typefun"
            else List.map2 (fun (i,_) t -> i, t) ps ts
      in
(*
      print_endline ("Body before subs    = " ^ sbt bsym_table body);
      print_endline ("Parameters= " ^ catmap ","
        (fun (i,t) -> "T"^si i ^ "=>" ^ sbt bsym_table t) params');
*)
      let t' = list_subst counter params' body in
(*
      print_endline ("Body after subs     = " ^ sbt bsym_table t');
*)
      let t' = beta_reduce' calltag counter bsym_table sr (t::termlist) t' in
(*
      print_endline ("Body after reduction = " ^ sbt bsym_table t');
*)
(* FIXME: IS THIS RIGHT? SHOULDN'T THE ADJUST HAPPEN FIRST?  I mean, the top level
beta reduction routine should always adjust. Hmmm.
*)


      let t' = adjust bsym_table t' in
(*
print_endline ("Flx_beta: result of application is: " ^ Flx_btype.st t' ^ "\n**********************\n\n");
*)
      t'

    | _ ->
      let t = btyp_type_apply (t1,t2) in
(*
      print_endline ("Flx_beta: type apply nonfunction .. can't beta reduce, keep as " ^ Flx_btype.st t);
*)
      t
    end
    end


