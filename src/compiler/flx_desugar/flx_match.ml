open Flx_exceptions
open Flx_print
open Flx_ast
open Flx_types
open Flx_pat
open Flx_util

 
let generated = Flx_srcref.make_dummy "[flx_match] generated"
let etup = EXPR_tuple (generated,[])

let make_match_check sr rex pat match_var_name match_var_index =
  let params = [], None in
  let match_expr = Flx_desugar_pat.gen_match_check pat (EXPR_index (sr,match_var_name, match_var_index)) in
  let stmts, e = rex match_expr in
  let asms = stmts @ [Exe (sr,EXE_fun_return e)] in
  DCL_function (params, Flx_typing.flx_bool,Flx_typing.flx_unit,[`Generated "Flx_match.make_match_check"],asms)

let make_match_handler sr rex pat match_var_name match_var_index body =
 assert false (* TO BE DONE *)

let gen_match rex seq name sr e pss =
(* print_endline ("Generating expr match " ^ name ^ ", expr=" ^ string_of_expr e); *)

    if List.length pss = 0 then clierrx "[flx_desugar/flx_match.ml:25: E339] " sr "Empty Pattern";

    (* step 1: evaluate e *)
    let d,x = rex e in
    let match_function_index = seq() in
    let match_var_index = seq() in
    (*
    print_endline ("Match function index = " ^ string_of_bid match_function_index );
    print_endline ("Match variable index = " ^ string_of_bid match_var_index );
    *)

    let match_var_name = name ^ "_mv_" ^ string_of_bid match_function_index in
    let match_function_id =
      name ^ "_mf_" ^ string_of_bid match_function_index
    in
    let match_function =
      EXPR_index (sr,match_function_id,match_function_index)
    in
    let match_seq = ref (seq()) in

    let expr_src = src_of_expr e in

    (* WOE. The expr may contain a lambda, which stuffs up
       bind_expression which is called by bind_type ..
    *)
    let evl =
      [
        Dcl (
          expr_src,
          match_var_name,
          Some match_var_index,
          `Private,
          dfltvs,
          DCL_value (TYP_typeof x, `Val));
        Exe (expr_src,EXE_iinit ((match_var_name,match_var_index),x))
      ]
    in
    let pats,_ = List.split pss in
    Flx_pat.validate_patterns pats
    ;
    let ematch_seq = seq() in
    (*
    let end_match_label = "_em" ^ string_of_bid ematch_seq in
    *)
    let matches = ref [Exe (generated,EXE_comment "begin match")] in
    let match_caseno = ref 1 in
    let iswild = ref false in
    List.iter
    (fun (pat,e) ->
      let n1 = !match_seq in
      let n2 = seq() in
      let mh_idx = seq () in
      let mc_idx = seq () in
      if !iswild then ()
        (*
        print_endline "WARNING, matches after wildcard ignored"
        *)
      else begin
        iswild := is_irrefutable pat;
        let patsrc = src_of_pat pat in
        let expr_src = src_of_expr e in
        let match_checker_id = name ^ "_mc" ^ string_of_bid n1 in
        let match_handler_id = name ^ "_mh" ^ string_of_bid n1 in
        let match_checker = EXPR_index (patsrc,match_checker_id,mc_idx) in
        let match_handler = EXPR_index (expr_src,match_handler_id,mh_idx) in
        (*
        print_endline ("Match checker index = " ^ string_of_bid mc_idx);
        print_endline ("Match handler index = " ^ string_of_bid mh_idx);
        *)
        let sts,result_expr = rex e in
        let body =
          sts @
          [Exe (expr_src,EXE_fun_return (result_expr))]
        in
        matches := !matches @
        [
          Dcl (patsrc,match_checker_id,Some mc_idx,`Private,dfltvs,
          make_match_check sr rex pat match_var_name match_var_index);
          Dcl
          (
            expr_src,
            match_handler_id,Some mh_idx,
            `Private,
            dfltvs,
            DCL_match_handler
            (
              pat,
              (match_var_name,match_var_index),
              body
            )
          )
        ]
        @
        [
        Exe (patsrc,EXE_comment ("match case " ^ si !match_caseno^":" ^ string_of_pattern pat))
        ]
        @
        (
        (* we dont need a label for the first case *)
        if !match_caseno <> 1 then
        let label = "_ml"^string_of_bid n1 in
        [
        Exe (patsrc,EXE_label label)
        ]
        else []
        )
        @

        (* This code checks the match condition, it can be
           elided if the match is wildcard
        *)
        (if !iswild then [] else
        [
          Exe
          (
            patsrc,
            EXE_ifgoto
            (
              EXPR_not
              (
                patsrc,
                EXPR_apply
                (
                  patsrc,
                  (
                    match_checker,
                    EXPR_tuple (patsrc,[])
                  )
                )
              ),
              "_ml" ^ string_of_bid n2
            )
          )
        ]
        )
        @
        [
        Exe
        (
          patsrc,
          EXE_fun_return
          (
            EXPR_apply
            (
              patsrc,
              (
                match_handler,
                EXPR_tuple (patsrc,[])
              )
            )
          )
        )
        (*
        ;
        Exe (patsrc,EXE_goto end_match_label)
        *)
        ]
        ;
        incr match_caseno;
        match_seq := n2
      end
    )
    pss
    ;
    let failure_label = "_ml" ^ string_of_bid !match_seq in

    let match_function_body =
    d
    @
    evl
    @
    !matches
    @
    (if !iswild then [] else
      let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
      let s = Flx_print.string_of_string f ^"," ^
        si sl ^ "," ^ si sc ^ "," ^
        si el ^ "," ^ si ec
      in
      [
        Exe (sr, EXE_comment "match failure");
        Exe (sr, EXE_label failure_label);
        Exe (sr, EXE_noreturn_code (
          CS.Str ("      FLX_MATCH_FAILURE(" ^ s ^ "); //[flx_match] \n"),etup));
      ]
    )
    in
    [
      Dcl
      (
        sr,
        match_function_id,Some match_function_index,
        `Private,
        dfltvs,
        DCL_function
        (
          ([],None),
          TYP_none,
          Flx_typing.flx_unit,
          [`GeneratedInline;`Generated "desugar:match fun"],
          match_function_body
        )
      )
    ]
    ,
    EXPR_apply
    (
      sr,
      (
        match_function,
        EXPR_tuple (sr,[])
      )
    )

let gen_stmt_match seq rex rsts name parent_vs access sr e pss =
(*
print_endline ("Generating stmt match " ^ name ^ ", expr=" ^ string_of_expr e);
*)
    if List.length pss = 0 then clierrx "[flx_desugar/flx_match.ml:243: E340] " sr "Empty Pattern";

    (* step 1: evaluate e *)
    let d,x = rex e in
    let match_var_index : bid_t = seq () in

    let match_var_name = name^ "_mv_" ^ string_of_bid match_var_index in
    let match_id = name^ "_mf_" ^ string_of_bid match_var_index in
    let end_match_label = "_em" ^ string_of_bid match_var_index in

    let expr_src = src_of_expr e in

    (* WOE. The expr may contain a lambda, which stuffs up
       bind_expression which is called by bind_type ..
    *)
    let evl =
      [
        Dcl (
          expr_src,
          match_var_name,
          Some match_var_index,
          `Private,
          dfltvs,
          DCL_value (TYP_typeof x, `Val));
        Exe (expr_src,EXE_iinit ((match_var_name,match_var_index),x))
      ]
    in
    let pats,_ = List.split pss in
    Flx_pat.validate_patterns pats
    ;
    let matches = ref [Exe (generated,EXE_comment "begin match")] in
    let match_caseno = ref 1 in
    let iswild = ref false in
    let n2 = ref (seq()) in (* the next case *)
    let need_final_label = ref false in
    List.iter
    (fun (pat,sts) ->
(*
print_endline "Pattern statements are:";
List.iter (fun s -> print_endline (string_of_statement 2 s)) sts;
*)
      let n1 = !n2 in (* this case *)
      n2 := seq(); (* the next case *)
      iswild := is_irrefutable pat;
      let patsrc = src_of_pat pat in
      let match_checker_id = name ^ "_mc" ^ string_of_bid n1 in
      let match_checker = EXPR_index (patsrc,match_checker_id,n1) in
      let vars = ref [] in
      Flx_desugar_pat.get_pattern_vars vars pat [];
(*
          print_endline ("PATTERN IS " ^ string_of_pattern pat ^ ", VARIABLE=" ^ match_var_name);
          print_endline "VARIABLES ARE";
          List.iter (fun vname (sr,extractor) ->
            let component =
              Flx_desugar_pat.gen_extractor extractor (EXPR_index (sr,match_var_name,match_var_index))
            in
            print_endline ("  " ^ vname ^ " := " ^ string_of_expr component);
          ) (List.rev (!vars);
*)
      let new_sts = ref sts in
      List.iter
          (fun (vname, (sr,extractor)) ->
            let component =
              Flx_desugar_pat.gen_extractor extractor
              (EXPR_index (sr,match_var_name,match_var_index))
            in
            let dcl = STMT_val_decl (sr,vname,dfltvs,None,Some component) in
            new_sts := dcl :: !new_sts;
          )
      (List.rev (!vars));
      let body = 
(*
        rsts name parent_vs access [block sr !new_sts]
*)
        rsts name parent_vs access !new_sts
      in
      (* hacky attempt to elide useless jumps at the end of each case
       * doesn't account for non-returning calls, trailing comments or non
       * executable statements, or complicated statements (such as nested matches)
       *)
      let returns = 
        let rec aux body =
          match List.rev (List.filter (fun x -> match x with Exe x -> true | _ -> false) body) with 
          | Exe (_,h) ::_ -> 
            begin match h with
            | EXE_noreturn_code _ 
            | EXE_goto _ 
            | EXE_jump _ 
            | EXE_loop _ 
            | EXE_fun_return _ 
            | EXE_proc_return
            | EXE_halt _ 
              -> true
            | _ -> false
            end
          | _ -> false
        in aux body
      in
      if not returns then need_final_label := true;
      matches := !matches @
        [
          Dcl (patsrc,match_checker_id,Some n1,`Private,dfltvs,
          make_match_check sr rex pat match_var_name match_var_index)
        ]
        @
        [
        Exe (patsrc,EXE_comment ("match case " ^ si !match_caseno^":" ^ string_of_pattern pat))
        ]
        @
        (if !iswild then [] else
        [
          Exe
          (
            patsrc,
            EXE_ifgoto
            (
              EXPR_not
              (
                patsrc,
                EXPR_apply
                (
                  patsrc,
                  (
                    match_checker,
                    EXPR_tuple (patsrc,[])
                  )
                )
              ),
              "_ml" ^ string_of_bid (!n2)
            )
          )
        ]
        )
        @
        body
        @
        (if not returns then [Exe (patsrc,EXE_goto end_match_label) ] else [])
        @
        (let label = "_ml"^string_of_bid (!n2) in
        [
        Exe (patsrc,EXE_label label)
        ])
      ;
      incr match_caseno
    )
    pss
    ;

    let match_function_body =
    d
    @
    evl
    @
    !matches
    @
    (if !iswild then [] else
      let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
      let s = Flx_print.string_of_string f ^"," ^
        si sl ^ "," ^ si sc ^ "," ^
        si el ^ "," ^ si ec
      in
      [
        Exe (sr,EXE_comment "match failure");
        Exe (sr,EXE_noreturn_code (CS.Str
          ("      FLX_MATCH_FAILURE(" ^ s ^ ");\n"), etup));
      ]
    )
    @
    (if !need_final_label then 
      [ 
        Exe (sr,EXE_label end_match_label) 
      ] 
      else []
    )
    in
    match_function_body



