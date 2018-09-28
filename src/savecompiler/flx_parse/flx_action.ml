let dflt_action prod =
  let rn = ref 1 in
  let action =
      "`(" ^
      List.fold_left (fun acc _ -> let n = !rn in incr rn;
        (if acc = "" then "" else acc ^ " ") ^ ",_" ^ string_of_int n
      ) "" prod
      ^ ")"

  in
  action

let cal_action prod action =
  match action with
  | Flx_token.Action_None -> dflt_action prod
  | Flx_token.Action_Expr scm -> 
    let x =  
      "(SUBST (quote " ^ Ocs_print.string_of_ocs scm ^ ") " ^ 
      " (vector 0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20))" 
    in
    x

  | Flx_token.Action_Statements scm -> 
    let x =  
      "(SUBST `(ast_seq ,_sr " ^ Ocs_print.string_of_ocs scm ^ ") " ^ 
      " (vector 0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20))" 
    in
    x

  | Flx_token.Action_Scheme scm -> scm




