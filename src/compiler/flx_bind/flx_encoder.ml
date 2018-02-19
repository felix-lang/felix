
open Flx_btype
open Flx_bexe
open Flx_bexpr

let unit_proc_type = btyp_function (btyp_unit (), btyp_void ())

let gen_encoder (state: Flx_lookup_state.lookup_state_t) bsym_table bt lookup_name_with_sig env rs sr ts  =
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
        try List.assoc (index,ts) state.Flx_lookup_state.encoder_cache
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
print_endline ("Generating union encoder chip _encoder_" ^ uname); 
print_endline ("ubvs = " ^ catmap "," (fun (s,i) -> s ^ "<" ^ string_of_int i ^">") ubvs);
*)
          let oschannel_t = Flx_btype_subst.tsubst sr ubvs ts (bt sr (Flx_ast.TYP_name (sr,"oschannel", [ut]))) in
          let ischannel_t ubt = bt sr (Flx_ast.TYP_name (sr,"ischannel",[ubt])) in
(*
print_endline ("oschannel type = " ^ Flx_print.sbt bsym_table oschannel_t);
*)
          let super_fun_name = "_encoder_" ^ uname in
          let super_proc_name = "_inner_"^super_fun_name in
          let super_fun_index = Flx_bid.fresh_bid counter in
          let super_proc_index = Flx_bid.fresh_bid counter in
(*
print_endline ("Enclosing function " ^ super_fun_name ^ "<" ^ string_of_int super_fun_index ^ ">");
print_endline ("Enclosing procedure " ^ super_proc_name ^ "<" ^ string_of_int super_proc_index ^ ">");
*)
          let ubt t = Flx_typecode_of_btype.typecode_of_btype bsym_table counter sr t in
          let original_flds = List.map (fun (name,v,vs',d,c,gadt) -> name,d) flds in
          (* fudge constant constructors to use unit argument *)
          let flds = List.map (fun (s,t) -> s,(match t with BTYP_void -> btyp_tuple [] | _ -> t)) original_flds in
          let flds = List.map (fun (s,t) -> s,Flx_btype_subst.tsubst sr ubvs ts t) flds in
(*
print_endline ("Field types " ^ catmap "," (fun (s,t) -> s ^ ":" ^ Flx_print.sbt bsym_table t) flds);
*)
          let ichans = List.map (fun (s,d) -> s,ischannel_t (ubt d)) flds in
(*
print_endline ("Input channel fields " ^ catmap "," (fun (s,t) -> s ^ ":" ^ Flx_print.sbt bsym_table t) ichans);
*)
          (* construct parameter *)
          let super_param_index = Flx_bid.fresh_bid counter in
          let super_param_type = btyp_record ((uname,oschannel_t) :: ichans) in
          let super_param_name = "io" in
          let super_param = {  Flx_bparameter.pid = super_param_name; pindex = super_param_index; pkind = `PVal; ptyp= super_param_type }  in
          let super_param_var = bexpr_varname super_param_type (super_param_index, ts) in
(* ========================= *)
          let case k = 
(*
print_endline ("Case " ^ string_of_int k);
*)
            let _, original_field_t = List.nth original_flds k in
            let field_name, field_t = List.nth flds k in
            let outer_fun_index = Flx_bid.fresh_bid counter in
            let inner_proc_index = Flx_bid.fresh_bid counter in
            let outer_fun_name = field_name ^ "_to_" ^ uname ^ "_" ^ string_of_int outer_fun_index in
            let inner_proc_name = "inner_" ^ outer_fun_name in
(*
print_endline ("  ** outer function " ^ outer_fun_name ^ "<" ^ string_of_int outer_fun_index ^ ">");
print_endline ("  ** inner procedure " ^ inner_proc_name ^ "<" ^ string_of_int inner_proc_index ^ ">");
print_endline ("  ** Input type " ^ Flx_print.sbt bsym_table field_t);
*)
            let ichan_t = ischannel_t (Flx_typecode_of_btype.typecode_of_btype bsym_table state.Flx_lookup_state.counter sr field_t) in
(*
print_endline ("  ** Input channel type " ^ Flx_print.sbt bsym_table field_t);
*)
            (* construct parameter *)
            let param_index = Flx_bid.fresh_bid counter in
            let param_type = btyp_record [("out",oschannel_t); ("inp",ichan_t)] in
            let param_name = "io" in
            let param = {  Flx_bparameter.pid = param_name; pindex = param_index; pkind = `PVal; ptyp= param_type }  in
            let param_var = bexpr_varname param_type (param_index,ts) in
(*
print_endline ("  ** Constructed parameter "^ string_of_int param_index);
*)
            (* construct read variable *)
            let read_var_index = Flx_bid.fresh_bid counter in
            let read_var = bexpr_varname field_t (read_var_index, ts) in
(*
print_endline ("  ** Constructed variable to read into " ^ string_of_int read_var_index);
*)
            (* construct argument of read procedure *)
            let read_proc_arg_type = btyp_tuple [ichan_t; btyp_pointer field_t] in
            let read_proc = lookup_name_with_sig state bsym_table sr sr env env rs "read" [] [read_proc_arg_type] in
            let read_pin = bexpr_apply ichan_t (bexpr_rprj "inp" param_type ichan_t, param_var ) in
            let read_arg = bexpr_tuple read_proc_arg_type [read_pin; bexpr_ref (btyp_pointer field_t) (read_var_index,ts)] in
(*
print_endline ("  ** Constructed argument of read procedure");
*)
            (* construct argument of write procedure *)
            let write_proc_arg_type = btyp_tuple [oschannel_t; but] in
            let write_proc = lookup_name_with_sig state bsym_table sr sr env env rs "write" [] [write_proc_arg_type] in
            let write_pin = bexpr_apply oschannel_t (bexpr_rprj "out" param_type oschannel_t, param_var ) in

            let write_val = 
              match original_field_t with
              | BTYP_void -> bexpr_const_case (k,but)
              | _ -> 
                let inj = bexpr_inj k field_t but in
                bexpr_apply but (inj,read_var) 
            in 
            let write_arg = bexpr_tuple (btyp_tuple [oschannel_t;field_t]) [write_pin;write_val] in
(*
print_endline ("  ** Constructed argument of write procedure");
*)
            (* add label to symbol table *)
            let label_index = Flx_bid.fresh_bid state.Flx_lookup_state.counter in
            let label_name = "_repeat_" ^ field_name in
            let label_bbdcl = Flx_bbdcl.bbdcl_label label_name in 
            let label_bsym  = Flx_bsym.create ~sr label_name label_bbdcl in
            Flx_bsym_table.add bsym_table label_index (Some inner_proc_index) label_bsym;            

            let exes = 
            [
              bexe_label (sr,label_index);
              bexe_call (sr,read_proc, read_arg);
              bexe_call (sr,write_proc, write_arg);
              bexe_goto (sr,label_index)
            ]
            in
(*
print_endline ("  ** generated instructions for transducer");
*)
            let props = [] in
            let bvs = ubvs in
            let effects = btyp_unit () in
            let inner_proc_ret = btyp_function ( btyp_unit (), btyp_void () ) in

            (* add read var to symbol table *)
            let read_var_bbdcl = Flx_bbdcl.bbdcl_val (bvs, field_t, `Val) in 
            let read_var_bsym = Flx_bsym.create ~sr "_r" read_var_bbdcl in
            Flx_bsym_table.add bsym_table read_var_index (Some inner_proc_index) read_var_bsym;
(*
print_endline ("  ** added read variable to symbol table " ^ string_of_int read_var_index);
*)
            (* add inner procedure to symbol table, type 1->0 *)
            let inner_proc_bbdcl = Flx_bbdcl.bbdcl_fun (props, bvs, Flx_bparams.unit_bparams, btyp_void (), effects, exes) in
            let inner_proc_bsym = Flx_bsym.create ~sr inner_proc_name inner_proc_bbdcl in
            Flx_bsym_table.add bsym_table inner_proc_index (Some outer_fun_index) inner_proc_bsym;
(*
print_endline ("  ** added inner procedure to symbol table " ^ string_of_int inner_proc_index);
*)
            (* add parameter to symbol table *)
            let param_bbdcl = Flx_bbdcl.bbdcl_val (bvs, param_type, `Val) in 
            let param_bsym = Flx_bsym.create ~sr param_name param_bbdcl in
            Flx_bsym_table.add bsym_table param_index (Some outer_fun_index) param_bsym;
(*
print_endline ("  ** added parameter to symbol table " ^ string_of_int param_index);
*)
            (* add outer function to symbol table, type params -> (1->0) *)
            let outer_fun_exes = [bexe_fun_return (sr, bexpr_closure unit_proc_type (inner_proc_index,ts))] in
            let outer_fun_bbdcl = Flx_bbdcl.bbdcl_fun (props, bvs, (Satom param, None), unit_proc_type, effects, outer_fun_exes) in
            let outer_fun_bsym = Flx_bsym.create ~sr outer_fun_name outer_fun_bbdcl in
            Flx_bsym_table.add bsym_table outer_fun_index None outer_fun_bsym;
(*
print_endline ("  ** added outer function to symbol table " ^ string_of_int outer_fun_index);
*)
            (* bind nested function parameter to super function parameter *)
            let outer_fun_type = btyp_function (param_type, unit_proc_type) in
            let super_param_write_pin = bexpr_apply oschannel_t (bexpr_rprj uname super_param_type oschannel_t, super_param_var ) in
            let super_param_read_pin = bexpr_apply ichan_t (bexpr_rprj field_name super_param_type ichan_t, super_param_var ) in
            let bind_arg = bexpr_record [("inp", super_param_read_pin); ("out",super_param_write_pin)] in
            let bound_proc = bexpr_apply unit_proc_type (bexpr_closure outer_fun_type (outer_fun_index,ts), bind_arg) in

            let spawn_proc = lookup_name_with_sig state bsym_table sr sr env env rs "spawn_fthread" [] [unit_proc_type] in
            let spawn_call = bexe_call (sr, spawn_proc,bound_proc) in
(*
print_endline (" ** Generated spawn instruction");
*)
            spawn_call
          in
          let super_proc_exes = List.map case (Flx_list.nlist (List.length flds)) in
(*
print_endline ("Generate master procedure spawns");
*)
          let props = [] in
          let bvs = ubvs in
          let effects = btyp_unit () in
(*
print_endline ("Exes =\n" ^ String.concat "\n" (List.map (Flx_print.string_of_bexe bsym_table 1) super_proc_exes));
*)
          (* add super procedure to symbol table, type 1->0 *)
          let super_proc_bbdcl = Flx_bbdcl.bbdcl_fun (props, bvs, Flx_bparams.unit_bparams, btyp_void (), effects, super_proc_exes) in
          let super_proc_bsym = Flx_bsym.create ~sr super_proc_name super_proc_bbdcl in
          Flx_bsym_table.add bsym_table super_proc_index (Some super_fun_index) super_proc_bsym;

          (* add super_parameter to symbol table *)
          let super_param_bbdcl = Flx_bbdcl.bbdcl_val (bvs, super_param_type, `Val) in 
          let super_param_bsym = Flx_bsym.create ~sr super_param_name super_param_bbdcl in
          Flx_bsym_table.add bsym_table super_param_index (Some super_fun_index) super_param_bsym;

          (* add super function to symbol table, type params -> (1->0) *)
          let super_fun_exes = [bexe_fun_return (sr, bexpr_closure unit_proc_type (super_proc_index,ts))] in
          let super_fun_ret = btyp_function (super_param_type, unit_proc_type) in
          let super_fun_bbdcl = Flx_bbdcl.bbdcl_fun (props, bvs, (Satom super_param, None), unit_proc_type, effects, super_fun_exes) in
          let super_fun_bsym = Flx_bsym.create ~sr super_fun_name super_fun_bbdcl in
          Flx_bsym_table.add bsym_table super_fun_index None super_fun_bsym;

          let super_fun_type = btyp_function (super_param_type, unit_proc_type) in
          let result = bexpr_closure super_fun_type (super_fun_index,ts) in
          state.Flx_lookup_state.decoder_cache <- ((index, ts), result) :: state.Flx_lookup_state.decoder_cache;
(*
print_endline "Encoder generated";
*)
          result

        | _ -> Flx_exceptions.clierr2 sr (Flx_bsym.sr bsym) ("_decoder requires union argument, got " ^ Flx_bsym.id bsym);
        end (*8 BTYP_inst*)
        end
      | _ -> Flx_exceptions.clierr sr ("_decoder type argument must be union instance")
      end (*6 match but *)
    | _ -> Flx_exceptions.clierr sr ("_decoder requires single type argument")
    end (*4 match ts *)


