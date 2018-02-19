
open Flx_version
open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_typing2
open Flx_pat
open Flx_exceptions
let generated = Flx_srcref.make_dummy "[flx_desugar_expr] generated"


(** A helper function to take two opt[expressions] in, boolean conjunct them,
    and wrap back into an opt. *)
let inv_join l r sr = 
  match l, r with
  | None, None -> None
  | None, Some i2 -> r
  | Some i1, None -> l
  | Some i1, Some i2 -> 
      Some( EXPR_apply (sr, 
        (EXPR_name (sr, "land", []), 
         EXPR_tuple (sr, [i1; i2]))))

(** Scoop up invariants from the body of a function and return them as an expression *)
let invariants_of_stmts body sr =
  let invariants = ref [] in

  List.iter 
    (fun st ->
      match st with
      | STMT_invariant (_, e)  as invariant -> 
          invariants := invariant :: !invariants
      | _ -> ()
    )
    body;
  
  (* take the statements, pull out expression and then boolean and them (e and (e2 and ...)) *)
  let conjunction =
    match !invariants with
    | [] -> None
    | _ -> 
      Some(
        List.fold_left 
          (fun x y -> 
            match y with
            | STMT_invariant (sr, e) ->
               EXPR_apply (sr, (EXPR_name (sr, "land", []), EXPR_tuple (sr, [x; e]))) 
            | _ -> failwith "Unexpected statement type found processing invariants"
          ) 
          (EXPR_typed_case (sr, 1, TYP_unitsum 2)) 
          !invariants
      )
    in
    conjunction


(* Removes everything except invariants, becuase those aren't allowed to be run. *)
let propagate_invariants body invariants sr =

  let addpost p i = 
      begin match p,i with
      | None,None -> None
      | None,Some _ -> i
      | Some _, None -> p
      | Some post, Some inv -> Some( EXPR_apply (sr, (EXPR_name (sr, "land", []), EXPR_tuple (sr, [post; inv]))))
      end
  in

  let newstatements = ref [] in

  List.iter (fun st ->
    match st with
    (* Erase invariants from the original expression. *)
    | STMT_invariant (_, _) -> ()

    (* propagate invariants to deeper levels (i.e. object -> method) *)
    | STMT_curry (sr, name, vs, pss, (res,traint) , effects, kind, adjectives, ss)
            when kind = `Method || kind = `GeneratorMethod -> 
        let inv2 = addpost traint invariants in 
        (*
        print_endline ("Propagating invariants to child STMT_curry: " ^ name ^ " constraints=" ^ 
          match inv2 with
          | Some (t) -> string_of_expr t
          | None -> "");
        *)
        newstatements := 
          STMT_curry (sr,name, vs, pss, (res,inv2) , effects, kind, adjectives, ss)
          :: !newstatements

    (* Just accumulate everything else. *)
    | _ -> 
        newstatements := st :: !newstatements
  )
  body;

  (* Becuase the statements get added backwards, reverse to correct it. *)
  let body = List.rev !newstatements in 
  body



(** Iterate over parameters and create type variables when needed. *)
let fix_param sr seq p =
  let pparam p : (string * kindcode_t) list * parameter_t =
    (* ps : (sr * param_kind_t * Flx_id.t * typecode_t * expr_t option) *)
    match p with

    (* The case where the param type is none. 
       This is where things like 'could not match type "_v4029" with "int" in "x + 3"' originate *)
    | (sr,kind,id,TYP_none,expr) ->

      let v = "_v" ^ string_of_bid (seq()) in  (* Create a fresh identifier *)
      let vt = TYP_name (generated,v,[]) in    (* Create a new type variable w/ dummy source ref. *)
      [v,KND_generic],(sr,kind,id,vt,expr)

    | p -> [],p
  in

  let rec pspec (ps:paramspec_t)  : (string * kindcode_t) list * paramspec_t = 
    match ps with
    | Satom p -> 
      let vs, p = pparam p in 
      vs,Satom p

    | Slist ps ->
      let vps = List.map pspec ps in
      let vs = List.concat (List.map fst vps) in
      let pss = List.map snd vps in
      vs,Slist pss
   in

  let p, traint = p in   (* Split ps into type variables and type constraints *)
  let vs,p = pspec p in    (* Process params *)
  vs,(p,traint)           (* Reassemble *)


let cal_props kind props = match kind with
  | `CFunction -> `Cfun::props
  | `InlineFunction -> if not (List.mem `Inline props) then `Inline::props else props
  | `GeneratedInlineProcedure-> `GeneratedInline::props
  | `GeneratedInlineFunction -> `GeneratedInline::props
  | `NoInlineFunction -> if not (List.mem `NoInline props) then `NoInline::props else props
  | `Ctor -> `Ctor::props
  | `Generator -> (* `NoInline:: *) `Generator::props
  | `GeneratorMethod -> (* `NoInline:: *) `Generator::props
  | `Virtual -> if not (List.mem `Virtual props) then `Virtual::props else props

  | `Function
  | `Object
  | `Method -> props

(** Currying, A.K.A. Shonfinkeling *)
let mkcurry seq sr name vs args return_type effects kind body props =

  (* preflight checks *)
  if List.mem `Pure props && match return_type with  | TYP_void _,_ -> true | _ -> false then
    clierrx "[flx_desugar/flx_curry.ml:159: E319] " sr "Felix procedure cannot be pure";

  (* Manipulate the type variables ----- *)

  (* vs = type variables and tcon = type constraints *)
  let vs, tcon = vs in

  (* Iterate over parameters and determine if new type variables need to be created *)
  (* New ones are stored in vss' *)
  let vss',args = 
    List.split 
      (List.map (fix_param sr seq) args) in 

  (* Reassemble vs back together *)
  let vs = List.concat (vs :: vss') in
  let vs : vs_list_t = vs,tcon in

  let return_type, postcondition = return_type in
  let mkret arg (eff,ret) = 
    Flx_typing.flx_unit,
    TYP_effector 
    (
      typeof_paramspec_t (fst arg),
      eff,
      ret
    )
  in

  let arity = List.length args in

  let rettype args eff =
    match return_type with
    | TYP_none -> TYP_none
    | _ -> 
        snd 
          (List.fold_right 
            mkret 
            args 
            (eff,return_type))
  in

  let isobject = kind = `Object in
  let rec aux (args:params_t list) (vs:vs_list_t) props (detached_args:params_t list) = 

    (* Gather invariants from: param constraints and from invariant statements *)
    let _,traints = List.split args in
    let pre_inv = match traints with | Some(h) :: t -> Some(h) | _ -> None in
    let post_inv = invariants_of_stmts body sr in 
    let invariants = inv_join pre_inv post_inv sr in

    (* Don't forget to propagate the invariants from params that are dropped when currying. *)
    if List.length detached_args > 1 then 
        clierrx "[flx_desugar/flx_curry.ml:218: E320_2] " sr "Detached args expected to be of length 0 or 1";
    let _,traints = List.split detached_args in
    let pre_inv = match traints with | Some(h) :: t -> Some(h) | _ -> None in
    let invariants = inv_join pre_inv invariants sr in

    (* propagate invariants into child functions *)
    let body = propagate_invariants body invariants sr in

    (* merge invariants with the current closure's post conditions *)
    let postcondition = inv_join postcondition invariants sr in

    let n = List.length args in
    let synthname n =
      if n = arity
      then name
      else name^"'" ^ si (arity-n+1)
    in
    match args with
    | [] ->
        begin match return_type with
        | TYP_void _ ->
          let body = 
            let reved = List.rev body in
            List.rev (STMT_label (sr,"_endof_" ^ synthname n) ::
              match reved with
              | STMT_proc_return _ :: _ ->  reved
              | _ -> STMT_proc_return sr :: reved
            )
          in
          STMT_function (sr, synthname n, vs, (Slist [],None), (return_type,postcondition), effects, props, body)

        | _ ->
          (* allow functions with no arguments now .. *)
          begin match body with
          | [STMT_fun_return (_,e)] ->
            let rt = match return_type with
            | TYP_none -> None
            | x -> Some x
            in
            STMT_lazy_decl (sr, synthname n, vs, rt, Some e)
          | _ ->
            clierrx "[flx_desugar/flx_curry.ml:260: E320] " sr "Function with no arguments"
          end
        end

    | h :: [] -> (* bottom level *)
      if isobject then begin
        (*
        print_endline "Found an object, scanning for methods and bogus returns";
        *)
        let methods = ref [] in

        List.iter 
          (fun st ->
            (*
            print_endline ("Statement " ^ Flx_print.string_of_statement 2 st);
            *)
            match st with
            | STMT_fun_return _ -> clierrx "[flx_desugar/flx_curry.ml:277: E321] " sr "FOUND function RETURN in Object";
            | STMT_proc_return _ -> clierrx "[flx_desugar/flx_curry.ml:278: E322] " sr "FOUND procedure RETURN in Object";
            | STMT_curry (_,name, vs, pss, (res,traint) , effects, kind, adjectives, ss)
                when kind = `Method || kind = `GeneratorMethod -> 
                methods := name :: !methods
            | _ -> ()
          )
          body
        ;

        (* Calculate methods to attach to return type *)
        let revbody = List.rev body in 
        let mkfield s = s,EXPR_name (sr,s,[]) in
        let record = EXPR_record (sr, List.map mkfield (!methods)) in
(*
        print_endline ("Object method record: " ^ string_of_expr record);
*)
        let retstatement = STMT_fun_return (sr, record) in
        let revbody = retstatement :: revbody in
        let body = List.rev revbody in
(*
        print_endline ("Object " ^name^ " return type " ^ string_of_typecode return_type); 
*)
        STMT_function (sr, synthname n, vs, h, (return_type, postcondition), effects,props, body)

      end else 
        let body = 
          match return_type with
          | TYP_void _  ->
(*
            print_endline ("(args) Name = " ^ name ^ "synthname n = " ^ synthname n);
*)
            let reved = List.rev body in
            List.rev (STMT_label (sr,"_endof_" ^ synthname n) ::
              match reved with
              | STMT_proc_return _ :: _ ->  reved
              | _ -> STMT_proc_return sr :: reved
            )
          | _ -> body
        in
        STMT_function (sr, synthname n, vs, h, (return_type,postcondition), effects,props, body)

    | h :: t ->
      let argt =
        let hdt = List.hd t in
        let xargs,traint = hdt in
        typeof_paramspec_t xargs
      in
      let m = List.length args in
      let body = [ 
        aux t dfltvs [] [h];
        STMT_fun_return ( sr, EXPR_suffix ( sr, ( `AST_name (sr,synthname (m-1),[]),argt))) ] 
      in
      let noeffects = Flx_typing.flx_unit in
      STMT_function (sr, synthname m, vs, h, (rettype t effects,postcondition), noeffects,`Generated "curry"::props, body)

   in aux args vs (cal_props kind props) []



