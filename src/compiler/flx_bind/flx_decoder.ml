open Flx_btype
open Flx_bexe
open Flx_bexpr

let gen_decoder (state: Flx_lookup_state.lookup_state_t) bsym_table bt lookup_name_with_sig env rs sr ts  =
  let counter = state.Flx_lookup_state.counter in
    begin (*4*)match ts with
    | [ut] ->
(*
print_endline("Binding type " ^ Flx_print.string_of_typecode ut);
*)
      let but = bt sr ut in
(*
print_endline("Bound type " ^ Flx_print.sbt bsym_table but);
*)
      begin (*6*)match but with
      | BTYP_inst (index, ts,_) ->
        begin
        try List.assoc (index,ts) state.Flx_lookup_state.decoder_cache
        with Not_found ->
        let bsym = 
           try Flx_bsym_table.find bsym_table index 
           with Not_found -> Flx_exceptions.syserr sr ("Can't find index " ^ string_of_int index ^ " in bound symbol table")
        in
(*
        print_endline ("Found index " ^ si index ^ " -> " ^ Flx_bsym.id bsym);
*)
        let bbdcl = Flx_bsym.bbdcl bsym in
        begin (* 8*)match bbdcl with
        | Flx_bbdcl.BBDCL_union (ubvs, flds)  -> 
          let uname = Flx_bsym.id bsym in
(*
print_endline ("Generating union decoder chip _decoder_" ^ uname); 
print_endline ("ubvs = " ^ catmap "," (fun (s,i) -> s ^ "<" ^ string_of_int i ^">") ubvs);
*)
          let ischannel_t = Flx_btype_subst.tsubst sr ubvs ts (bt sr (Flx_ast.TYP_name (sr,"ischannel", [ut]))) in
          let oschannel_t ubt = bt sr (Flx_ast.TYP_name (sr,"oschannel",[ubt])) in
(*
print_endline ("ischannel type = " ^ Flx_print.sbt bsym_table ischannel_t);
*)
          let outer_fun_name = "_decoder_" ^ uname in
          let inner_proc_name = "_inner_"^outer_fun_name in
          let ubt t = Flx_typecode_of_btype.typecode_of_btype bsym_table counter sr t in
          let flds = List.map (fun (name,v,vs',d,c,gadt) -> name,d) flds in
          (* fudge constant constructors to use unit argument *)
          let flds = List.map (fun (s,t) -> s,(match t with BTYP_void -> btyp_tuple [] | _ -> t)) flds in
          let flds = List.map (fun (s,t) -> s,Flx_btype_subst.tsubst sr ubvs ts t) flds in
(*
print_endline ("Field types " ^ catmap "," (fun (s,t) -> s ^ ":" ^ Flx_print.sbt bsym_table t) flds);
*)
          let ochans = List.map (fun (s,d) -> s,oschannel_t (ubt d)) flds in
(*
print_endline ("Output channel fields " ^ catmap "," (fun (s,t) -> s ^ ":" ^ Flx_print.sbt bsym_table t) ochans);
*)
          let outer_fun_index = Flx_bid.fresh_bid counter in
          let inner_proc_index = Flx_bid.fresh_bid counter in

          (* construct parameter *)
          let param_index = Flx_bid.fresh_bid counter in
          let param_type = btyp_record ((uname,ischannel_t) :: ochans) in
          let param_name = "io" in
          let param = {  Flx_bparameter.pid = param_name; pindex = param_index; pkind = `PVal; ptyp= param_type }  in

          let props = [] in
          let bvs = ubvs in
          let effects = btyp_unit () in
          let inner_proc_ret = btyp_function ( btyp_unit (), btyp_void () ) in
          let label_index = Flx_bid.fresh_bid counter in
          let param_var = bexpr_varname param_type (param_index,ts) in
          (* read input channel *)
          let read_var_index = Flx_bid.fresh_bid counter in
          let read_var = bexpr_varname but (read_var_index, ts) in 
          let read_pin = bexpr_apply ischannel_t (bexpr_rprj uname param_type ischannel_t, param_var) in
          let read_fun = lookup_name_with_sig state bsym_table sr sr env env rs "read" [] [ischannel_t] in

          let read_expr = bexpr_apply but (read_fun, read_pin) in
(*
print_endline ("Read expr = " ^ Flx_print.sbe bsym_table read_expr);
*)
          let read_op = bexe_assign (sr,read_var, read_expr) in
          let case k = 
(*
print_endline ("Case " ^ string_of_int k);
*)
            let next_label_index = Flx_bid.fresh_bid state.Flx_lookup_state.counter in
            let field_name, field_t = List.nth flds k in
            let ochan_t = oschannel_t (Flx_typecode_of_btype.typecode_of_btype bsym_table state.Flx_lookup_state.counter sr field_t) in
            let write_arg_type = btyp_tuple [ochan_t; field_t] in
            let write_proc = lookup_name_with_sig state bsym_table sr sr env env rs "write" [] [write_arg_type] in
            let write_pin = bexpr_apply ochan_t (bexpr_rprj field_name param_type ochan_t, param_var ) in
            let write_arg = bexpr_tuple write_arg_type [write_pin; bexpr_case_arg field_t (k,read_var)] in

            (* add label to symbol table *)
            let next_label_name = "_not_" ^ field_name in
            let next_label_bbdcl = Flx_bbdcl.bbdcl_label next_label_name in 
            let next_label_bsym  = Flx_bsym.create ~sr next_label_name next_label_bbdcl in
            Flx_bsym_table.add bsym_table next_label_index (Some inner_proc_index) next_label_bsym;            
            [
              bexe_ifgoto (sr,bexpr_not (bexpr_match_case (k,read_var)), next_label_index);
              bexe_call (sr,write_proc, write_arg);
              bexe_label (sr,next_label_index)
            ]
          in
            
          let match_body =  List.concat (List.map case (Flx_list.nlist (List.length flds))) in
          let exes = bexe_label (sr,label_index) :: read_op :: match_body @ [bexe_goto (sr,label_index)] in
(*
print_endline ("Exes =\n" ^ String.concat "\n" (List.map (Flx_print.string_of_bexe bsym_table 1) exes));
*)
          (* add read var to symbol table *)
          let read_var_bbdcl = Flx_bbdcl.bbdcl_val (bvs, but, `Val) in 
          let read_var_bsym = Flx_bsym.create ~sr "_r" read_var_bbdcl in
          Flx_bsym_table.add bsym_table read_var_index (Some inner_proc_index) read_var_bsym;

          (* add label to symbol table *)
          let label_bbdcl = Flx_bbdcl.bbdcl_label ("_start") in 
          let label_bsym  = Flx_bsym.create ~sr "_start" label_bbdcl in
          Flx_bsym_table.add bsym_table label_index (Some inner_proc_index) label_bsym;

          (* add inner procedure to symbol table, type 1->0 *)
          let inner_proc_bbdcl = Flx_bbdcl.bbdcl_fun (props, bvs, (Slist [], None), btyp_void (), effects, exes) in
          let inner_proc_bsym = Flx_bsym.create ~sr inner_proc_name inner_proc_bbdcl in
          Flx_bsym_table.add bsym_table inner_proc_index (Some outer_fun_index) inner_proc_bsym;

          (* add parameter to symbol table *)
          let param_bbdcl = Flx_bbdcl.bbdcl_val (bvs, param_type, `Val) in 
          let param_bsym = Flx_bsym.create ~sr param_name param_bbdcl in
          Flx_bsym_table.add bsym_table param_index (Some outer_fun_index) param_bsym;

          (* add outer function to symbol table, type params -> (1->0) *)
          let unit_proc_type = btyp_function (btyp_unit (), btyp_void ()) in
          let outer_fun_exes = [bexe_fun_return (sr, bexpr_closure unit_proc_type (inner_proc_index,ts))] in
          let outer_fun_ret = btyp_function (param_type, unit_proc_type) in
          let outer_fun_bbdcl = Flx_bbdcl.bbdcl_fun (props, bvs, (Satom param, None), unit_proc_type, effects, outer_fun_exes) in
          let outer_fun_bsym = Flx_bsym.create ~sr outer_fun_name outer_fun_bbdcl in
          Flx_bsym_table.add bsym_table outer_fun_index None outer_fun_bsym;

          let outer_fun_type = btyp_function (param_type, unit_proc_type) in
          let result = bexpr_closure outer_fun_type (outer_fun_index,ts) in
          state.Flx_lookup_state.decoder_cache <- ((index, ts), result) :: state.Flx_lookup_state.decoder_cache;
(*
print_endline "Decoder generated";
*)
          result

        | _ -> Flx_exceptions.clierr2 sr (Flx_bsym.sr bsym) ("_decoder requires union argument, got " ^ Flx_bsym.id bsym);
        end (*8 BTYP_inst*)
        end
      | _ -> Flx_exceptions.clierr sr ("_decoder type argument must be union instance")
      end (*6 match but *)
    | _ -> Flx_exceptions.clierr sr ("_decoder requires single type argument")
    end (*4 match ts *)


