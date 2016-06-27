
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

let fix_params sr seq (ps:params_t):plain_vs_list_t * params_t =
  let rec aux (ps:parameter_t list) :plain_vs_list_t * parameter_t list =
    match ps with
    | (k,x,TYP_none,d) :: t ->
      let v = "_v" ^ string_of_bid (seq()) in
      let vt: typecode_t = TYP_name (generated,v,[]) in
      let vs,ps = aux t in
      (*
      ((v,TPAT_any)::vs),((k,x,vt,d)::ps) (* a bit HACKY *)
      *)
      ((v,TYP_patany sr)::vs),((k,x,vt,d)::ps) (* a bit HACKY *)

    | h :: t ->
      let vs,ps = aux t in
      vs, (h::ps)
    | [] -> [],[]
  in
  let ps, traint = ps in
  let vs,ps = aux ps in
  vs,(ps,traint)

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
  | _ -> []

let mkcurry seq sr (name:string) (vs:vs_list_t) (args:params_t list) return_type effects (kind:funkind_t) body props =
  let noeffects = Flx_typing.flx_unit in
  if List.mem `Lvalue props then
    clierr sr "Felix function cannot return lvalue"
  ;
  if List.mem `Pure props && match return_type with  | TYP_void _,_ -> true | _ -> false then
    clierr sr "Felix procedure cannot be pure"
  ;

  let vs, tcon = vs in
  let return_type, postcondition = return_type in
  let vss',(args:params_t list)= List.split (List.map (fix_params sr seq) args) in
  let vs = List.concat (vs :: vss') in
  let vs : vs_list_t = vs,tcon in
  let mkfuntyp d e c = TYP_effector (d,e,c)
  and typeoflist lst = match lst with
    | [x] -> x
    | _ -> TYP_tuple lst
  in
  let mkret arg (eff,ret) = Flx_typing.flx_unit,mkfuntyp (typeoflist (List.map (fun(x,y,z,d)->z) (fst arg))) eff ret in
  let arity = List.length args in
  let rettype args eff =
    match return_type with
    | TYP_none -> TYP_none
    | _ -> snd (List.fold_right mkret args (eff,return_type))
  in

  let isobject = kind = `Object in
  let rec aux (args:params_t list) (vs:vs_list_t) props =
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
          STMT_function (sr, synthname n, vs, ([],None), (return_type,postcondition), effects, props, body)
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
          clierr sr "Function with no arguments"
          end
        end

    | h :: [] -> (* bottom level *)
      if isobject then begin
        (*
        print_endline "Found an object, scanning for methods and bogus returns";
        *)

        let methods = ref [] in
        let invariants = ref [] in

        let revbody = 
          let newstatements = ref [] in
          List.iter (fun st ->
            match st with
            | STMT_invariant (_, _) -> ()
            | _ -> newstatements := st :: !newstatements
          )
            body
          ;

          List.iter (fun st ->
            (*
            print_endline ("Statement " ^ Flx_print.string_of_statement 2 st);
            *)
            match st with
            | STMT_fun_return _ -> clierr sr "FOUND function RETURN in Object";
            | STMT_proc_return _ -> clierr sr "FOUND procedure RETURN in Object";
            | STMT_curry (_,name, vs, pss, (res,traint) , effects, kind, adjectives, ss)
                when kind = `Method || kind = `GeneratorMethod -> 
                methods := name :: !methods
            | STMT_invariant (_, _)  as invariant -> invariants := invariant :: !invariants
            | _ -> ()
          )
          body
          ;
          !newstatements
        in

        let mkfield s = s,EXPR_name (sr,s,[]) in
        let record = EXPR_record (sr, List.map mkfield (!methods)) in
        let retstatement = STMT_fun_return (sr, record) in
        let revbody = retstatement :: revbody in

        let conjunction =
          List.fold_left 
            (fun x y -> 
              match y with
              | STMT_invariant (sr, e) ->
                EXPR_apply (sr, (EXPR_name (sr, "land", []), EXPR_tuple (sr, [x; e])))
              | _ -> failwith "Unexpected statement type found processing invariants"
            ) 
            (EXPR_typed_case (sr, 1, TYP_unitsum 2)) 
            !invariants
        in
        let invariant_func = 
          STMT_function (sr, "invariant", dfltvs, ([], None), (TYP_unitsum 2, None), effects,[], [STMT_fun_return (sr, conjunction)]) 
        in

        let body = List.rev (invariant_func :: revbody) in
(* print_endline ("Object " ^name^ " return type " ^ string_of_typecode return_type); *)
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
        typeoflist (List.map (fun (x,y,z,d) -> z) xargs)
      in
      let m = List.length args in
      let body =
        [
          aux t dfltvs [];
          STMT_fun_return
          (
            sr,
            EXPR_suffix
            (
              sr,
              (
                `AST_name (sr,synthname (m-1),[]),argt
              )
            )
          )
        ]
      in
        STMT_function (sr, synthname m, vs, h, (rettype t effects,None), noeffects,`Generated "curry"::props, body)
   in aux args vs (cal_props kind props)


