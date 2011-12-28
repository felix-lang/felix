open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat

type lookup_state_t = {
  counter : bid_t ref;
  print_flag: bool;
  ticache : (bid_t, Flx_btype.t) Hashtbl.t;
  varmap: Flx_mtypes2.typevarmap_t; 
    (* used by unification to fix the return types of functions
     * MUST be a reference to the global one because that's used
     * in the front and back ends extensively..
     *)
  sym_table: Flx_sym_table.t;
  env_cache: (Flx_types.bid_t, Flx_mtypes2.env_t) Hashtbl.t;
}


let hfind msg h k =
  try Flx_sym_table.find h k
  with Not_found ->
    print_endline ("flx_lookup Flx_sym_table.find failed " ^ msg);
    raise Not_found

let get_data table index =
  try Flx_sym_table.find table index
  with Not_found ->
    failwith ("[Flx_lookup.get_data] No definition of <" ^
      string_of_bid index ^ ">")

let rsground= {
  constraint_overload_trail = [];
  idx_fixlist = [];
  type_alias_fixlist = [];
  as_fixlist = [];
  expr_fixlist = [];
  depth = 0;
  open_excludes = []
}

let cal_method_apply bsym_table state env rs be bt koenig_lookup cal_apply bind_type' sr sra fn e2 meth_ts =
    (*
    print_endline ("METHOD APPLY: " ^ string_of_expr e);
    *)
    (* .. PRAPS .. *)
    let meth_ts = List.map (bt sra) meth_ts in
    let (be2,t2) as x2 = be e2 in
    begin match t2 with
    | BTYP_record ("",es) ->
      let rcmp (s1,_) (s2,_) = compare s1 s2 in
      let es = List.sort rcmp es in
      let field_name = String.sub fn 4 (String.length fn -4) in
      begin match list_index (List.map fst es) field_name with
      | Some n -> bexpr_get_n (List.assoc field_name es) (n,x2)
      | None -> clierr2 sr sra
         (
           "Field " ^ field_name ^
           " is not a member of anonymous structure " ^
           sbt bsym_table t2
          )
      end
    | _ ->
    let tbe1 =
      match t2 with
      | BTYP_inst (index,ts) ->
        let parent = Flx_sym_table.find_parent state.sym_table index in
        begin match get_data state.sym_table index with
        { Flx_sym.id=id; sr=sr; symdef=entry } ->
        match parent with
        | None -> clierr2 sr sra "Koenig lookup: No parent for method apply (can't handle global yet)"
        | Some index' ->
            let sym = get_data state.sym_table index' in
            match sym.Flx_sym.symdef with
            | SYMDEF_root _
            | SYMDEF_module _
            | SYMDEF_function _ ->
                koenig_lookup
                  state
                  bsym_table
                  env
                  rs
                  sra
                  sym.Flx_sym.id
                  sym.Flx_sym.pubmap
                  fn
                  t2
                  (ts @ meth_ts)
            | _ -> clierr2 sr sra ("Koenig lookup: parent for method apply not module")
        end

      | _ -> clierr2 sr sra ("apply method "^fn^" to nongenerative type")
    in
      cal_apply state bsym_table sra rs tbe1 (be2, t2)
    end


let handle_dot state bsym_table build_env env rs be bt koenig_lookup cal_apply bind_type' sr e e2 =
  let mkenv i = build_env state bsym_table (Some i) in
  let rt t = beta_reduce state.counter bsym_table sr t in
    (* Analyse LHS.
      If it is a pointer, dereference it transparently.
      The component lookup is an lvalue if the argument
      is an lvalue or a pointer, unless an apply method
      is used, in which case the user function result
      determines the lvalueness.
    *)
    let ttt,e,te =
      let (_,tt') as te = be e in (* polymorphic! *)
      let rec aux n t = match t with
        | BTYP_pointer t -> aux (n+1) t
        | _ -> n,t
      in
      let np,ttt = aux 0 (rt tt') in
      let rec dref n x = match n with
          | 0 -> x
          | _ -> dref (n-1) (EXPR_deref (sr,x))
      in
      let e = dref np e in
      let e',t' = be e in
      let te = e',t' in
      ttt,e,te
    in

    begin match e2 with

    (* RHS IS A SIMPLE NAME *)
    | EXPR_name (_,name,ts) ->
      begin match ttt with

      (* LHS HAS A NOMINAL TYPE *)
      | BTYP_inst (i,ts') ->
        begin match hfind "lookup" state.sym_table i with

        (* STRUCT *)
        | { Flx_sym.id=id; vs=vs; sr=sra; symdef=SYMDEF_struct ls } ->
          begin try
          let cidx,ct =
            let rec scan i = function
            | [] -> raise Not_found
            | (vn,vat)::_ when vn = name -> i,vat
            | _:: t -> scan (i+1) t
            in scan 0 ls
          in
          let ct =
            let bvs = List.map
              (fun (n,i,_) -> n, btyp_type_var (i, btyp_type 0))
              (fst vs)
            in
            let env' = build_env state bsym_table (Some i) in
            bind_type' state bsym_table env' rsground sr ct bvs mkenv
          in
          let vs' = List.map (fun (s,i,tp) -> s,i) (fst vs) in
          let ct = tsubst vs' ts' ct in
          bexpr_get_n ct (cidx,te)
          with Not_found ->
            let get_name = "get_" ^ name in
            begin try cal_method_apply bsym_table state env rs be bt koenig_lookup cal_apply bind_type' sr sra get_name e ts
            with exn1 -> try be (EXPR_apply (sr,(e2,e)))
            with exn2 ->
            clierr sr (
              "AST_dot: cstruct type: koenig apply "^get_name ^
              ", AND apply " ^ name ^
              " failed with " ^ Printexc.to_string exn2
              )
            end
          end
        (* LHS CSTRUCT *)
        | { Flx_sym.id=id; vs=vs; sr=sra; symdef=SYMDEF_cstruct (ls,_) } ->
          (* NOTE: we try $1.name binding using get_n first,
          but if we can't find a component we treat the
          entity as abstract.

          Hmm not sure that cstructs can be polymorphic.
          *)
          begin try
            let cidx,ct =
              let rec scan i = function
              | [] -> raise Not_found
              | (vn,vat)::_ when vn = name -> i,vat
              | _:: t -> scan (i+1) t
              in scan 0 ls
            in
            let ct =
              let bvs = List.map
                (fun (n,i,_) -> n, btyp_type_var (i, btyp_type 0))
                (fst vs)
              in
              let env' = build_env state bsym_table (Some i) in
              bind_type' state bsym_table env' rsground sr ct bvs mkenv
            in
            let vs' = List.map (fun (s,i,tp) -> s,i) (fst vs) in
            let ct = tsubst vs' ts' ct in
            (* propagate lvalueness to struct component *)
            bexpr_get_n ct (cidx,te)
          with
          | Not_found ->
            (*
            print_endline ("Synth get method .. (1) " ^ name);
            *)
            let get_name = "get_" ^ name in
            begin try cal_method_apply bsym_table state env rs be bt koenig_lookup cal_apply bind_type' sr sra get_name e ts
            with _ -> try be (EXPR_apply (sr,(e2,e)))
            with exn ->
            clierr sr (
              "AST_dot: cstruct type: koenig apply "^get_name ^
              ", AND apply " ^ name ^
              " failed with " ^ Printexc.to_string exn
              )
            end

           end

        (* LHS HAS A PRIMITIVE TYPE *)
        | { Flx_sym.id=id; sr=sra; symdef=SYMDEF_abs _ } ->
            (*
            print_endline ("Synth get method .. (4) " ^ name);
            *)
          let get_name = "get_" ^ name in
          begin try cal_method_apply bsym_table state env rs be bt koenig_lookup cal_apply bind_type' sr sra get_name e ts
          with exn1 -> try be (EXPR_apply (sr,(e2,e)))
          with exn2 -> 
          clierr2 sr sra
          (
            "AST_dot: Abstract type "^id^"="^sbt bsym_table ttt ^
            "\napply " ^ name ^
            " failed with " ^ Printexc.to_string exn2
          )
          end

        | _ ->
          (* Try reverse application *)
          let retry = EXPR_apply (sr,(e2,e)) in
          be retry
          (*
          failwith ("[lookup] operator . Expected LHS nominal type to be"^
          " (c)struct or abstract primitive, got " ^
          sbt bsym_table ttt)
          *)

        end

      (* LHS HAS A RECORD TYPE *)
      | BTYP_record ("",es) ->
        let rcmp (s1,_) (s2,_) = compare s1 s2 in
        let es = List.sort rcmp es in
        let field_name = name in
        begin match list_index (List.map fst es) field_name with
        | Some n -> bexpr_get_n (List.assoc field_name es) (n,te)
        | None ->
          try be (EXPR_apply (sr,(e2,e)))
          with exn ->
          clierr sr
          (
            "[bind_expression] operator dot: Field " ^ field_name ^
            " is not a member of anonymous structure type " ^
             sbt bsym_table ttt ^
             "\n and trying " ^ field_name ^
             " as a function also failed"
          )
        end

      (* LHS FUNCTION TYPE *)
      | BTYP_function (d,c) ->
        (* print_endline ("AST_dot: LHS function, RHS name= " ^ name ); *)
        let bound = begin try be (EXPR_apply (sr,(e2,e)))
        with exn ->
        (* print_endline "Reverse application failed, checking for composition"; *)
        try 
          let r,rt = be e2 in
          match rt with
          | BTYP_function (a,b) ->
            (* print_endline "RHS is a function"; *)
            if c = a then (
              (* print_endline "Composed!";  *)
              bexpr_compose (btyp_function (d,b)) (te,(r,rt))
            )
            else
             clierr sr (
               "AST_dot " ^ string_of_expr e ^ " . " ^ string_of_expr e2^
               "codomain of LHS function should match domain of RHS function"
             )
 
          | _ -> 
            clierr sr (
            "AST_dot, LHS function, RHS arg "^ string_of_expr e2^
            " is simple name, should have been a function but got " ^
            Printexc.to_string exn
            )
        with exn2 ->
          clierr sr (
          "AST_dot, LHS function, RHS arg "^ string_of_expr e2^
          " is simple name, and attempt to bind it failed with " ^
          Printexc.to_string exn2
          )
        end
        in
        (* print_endline "AST_dot, RHS simple name, is done"; *)
        bound

      (* LHS TUPLE TYPE *)
      | BTYP_tuple _ ->
        begin try be (EXPR_apply (sr,(e2,e)))
        with exn ->
        clierr sr (
        "AST_dot, arg "^ string_of_expr e2^
        " is simple name, and attempt to apply it failed with " ^
        Printexc.to_string exn
        )
        end

      (* LHS OTHER ALGEBRAIC TYPE *)
      | _ ->
        begin try be (EXPR_apply (sr,(e2,e)))
        with exn ->
        clierr sr (
        "AST_dot, arg "^ string_of_expr e2^
        " is simple name, and attempt to apply it failed with " ^
        Printexc.to_string exn
        )
        end
      end

    (* RHS NOT A SIMPLE NAME: reverse application *)
    | _ ->
      begin try be (EXPR_apply (sr,(e2,e)))
      with exn ->
      (* print_endline "Reverse application failed, checking for composition"; *)
      match ttt with
      | BTYP_function (d,c) -> (* LHS *)
        (* print_endline ("AST_dot: LHS function, RHS function expr= " ^ * string_of_expr e2); *)
        begin try 
          let r,rt = be e2 in
          match rt with
          | BTYP_function (a,b) -> (* RHS *)
            (* print_endline "RHS is a function"; *)
            if c = a then (
              (* print_endline "Composed!";  *)
              bexpr_compose (btyp_function (d,b)) (te,(r,rt))
            )
            else
             clierr sr (
               "AST_dot " ^ string_of_expr e ^ " . " ^ string_of_expr e2^
               "codomain of LHS function should match domain of RHS function"
             )
 
          | _ -> 
            clierr sr (
            "AST_dot, LHS function, RHS arg "^ string_of_expr e2^
            " is expr, could have been a function but got " ^
            sbt bsym_table rt ^
            " and reverse application also failed."
            )
        with exn2 ->
          clierr sr (
          "AST_dot, LHS function, RHS arg "^ string_of_expr e2^
          " is expression, and attempt to bind it failed with " ^
          Printexc.to_string exn2
          )
        end

      | _ -> (* LHS not function *)
      clierr sr (
        "AST_dot, arg "^ string_of_expr e2^
        " is not simple name, and attempt to apply it failed with " ^
        Printexc.to_string exn
        )
      end
  end


