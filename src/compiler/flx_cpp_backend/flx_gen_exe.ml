open List

open Flx_bbdcl
open Flx_beta
open Flx_bexe
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_cexpr
open Flx_ctorgen
open Flx_ctypes
open Flx_display
open Flx_egen
open Flx_exceptions
open Flx_label
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_name
open Flx_ogen
open Flx_options
open Flx_pgen
open Flx_print
open Flx_types
open Flx_typing
open Flx_unify
open Flx_util
open Flx_gen_helper
open Flx_btype_subst
open Flx_bid

module CS = Flx_code_spec

(* NOTE: it isn't possible to pass an explicit tuple as a single
argument to a primitive, nor a single value of tuple/array type.
In the latter case a cast/abstraction can defeat this, for the
former you'll need to make a dummy variable.
*)

let fix_endlines s = 
  let n = String.length s in
  let b = Buffer.create (n+20) in
  for i=0 to n - 1 do
     let ch = s.[i] in
     if ch < ' ' then
       Buffer.add_string b ("\\x" ^ Flx_string.hex2 (Char.code ch))
     else
       Buffer.add_char b ch
     ; 
  done
  ;
  Buffer.contents b


type kind_t = Function | Procedure

let gen_exe filename cxx_name
  syms
  bsym_table
  (shapes : Flx_set.StringSet.t ref) shape_map
  label_info 
  counter
  this
  vs
  ts
  instance_no
  needs_switch
  stackable
  exe
=
(*
print_endline "---------------------------------------------";
print_endline ("generating exe in " ^ cxx_name);
print_endline ("gen_exe: " ^ string_of_bexe bsym_table 0 exe);
*)
  let sr = Flx_bexe.get_srcref exe in
  if length ts <> length vs then
  failwith
  (
    "[gen_exe} wrong number of args, expected vs = " ^
    si (length vs) ^
    ", got ts=" ^
    si (length ts)
  );
  let src_str = string_of_bexe bsym_table 0 exe in
  let src_str = fix_endlines src_str in
  let with_comments = syms.compiler_options.with_comments in
  
(*
  print_endline ("generating exe " ^ string_of_bexe bsym_table 0 exe);
  print_endline ("vs = " ^ catmap "," (fun (s,i) -> s ^ "->" ^ si i) vs);
  print_endline ("ts = " ^ catmap ","  (sbt bsym_table) ts);
*)
  let tsub t = beta_reduce "gen_exe" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  let ge = gen_expr syms bsym_table shapes shape_map label_info this vs ts in
  let ge' = gen_expr' syms bsym_table shapes shape_map label_info this vs ts in
  let tn t = cpp_typename syms bsym_table (tsub t) in
  let bsym =
    try Flx_bsym_table.find bsym_table this with _ ->
      failwith ("[gen_exe] Can't find this " ^ string_of_bid this)
  in
  let our_display = get_display_list bsym_table this in
  let caller_name = Flx_bsym.id bsym in
  let kind = match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (_,_,_,BTYP_fix (0,_),_,_) -> Procedure
    | BBDCL_fun (_,_,_,BTYP_void,_,_) -> Procedure
    | BBDCL_fun (_,_,_,_,_,_) -> Function
    | _ -> failwith "Expected executable code to be in function or procedure"
  in let our_level = length our_display in

  let rec handle_closure sr is_jump index ts subs' a stack_call =
    let subs =
      catmap ""
      (fun ((_,typ) as e,s) ->
        match typ with
        | BTYP_tuple [] -> ""
        | _ ->
        let t = cpp_ltypename syms bsym_table typ in
        let e = ge sr e in
        "      " ^ t ^ " " ^ s ^ " = " ^ e ^ ";\n"
      )
      subs'
    in
    let sub_start =
      if String.length subs = 0 then ""
      else "      {\n" ^ subs
    and sub_end =
      if String.length subs = 0 then ""
      else "      }\n"
    in
    let bsym =
      try Flx_bsym_table.find bsym_table index with _ ->
        failwith ("[gen_exe(call)] Can't find index " ^ string_of_bid index)
    in
    let called_name = Flx_bsym.id bsym in
    let handle_call props vs ps ret bexes =
      let is_ehandler = match ret with BTYP_fix (0,_) -> true | _ -> false in
      if bexes = []
      then
      "      //call to empty procedure " ^ Flx_bsym.id bsym ^ " elided\n"
      else begin
        let n = fresh_bid counter in
        let the_display =
          let d' =
            List.map begin fun (i,vslen) ->
              "ptr" ^ cpp_instance_name syms bsym_table i (list_prefix ts vslen)
            end (get_display_list bsym_table index)
          in
            if length d' > our_level
            then "this" :: tl d'
            else d'
        in
        (* if we're calling from inside a function,
           we pass a 0 continuation as the caller 'return address'
           otherwise pass 'this' as the caller 'return address'
           EXCEPT that stack calls don't pass a return address at all
        *)
        let this = match kind with
          | Function ->
            if is_jump && not is_ehandler
            then
              clierrx "[flx_cpp_backend/flx_gen_exe.ml:167: E300] " 
                sr ("[gen_exe] can't jump inside function " ^ caller_name ^" to " ^ called_name ^ 
               ", return type " ^ sbt bsym_table ret)
            else if stack_call then ""
            else "0"

          | Procedure ->
            if stack_call then "" else
            if is_jump then "tmp"
            else "this"
        in

        let args = match a with
          | _,BTYP_tuple [] -> this
          | _ ->
            (
              let a = ge sr a in
              if this = "" then a else this ^ ", " ^ a
            )
        in
        let name = cpp_instance_name syms bsym_table index ts in
        if mem `Cfun props then begin
          (if with_comments
          then "      //call cproc " ^ src_str ^ "\n"
          else "") ^
          "      " ^ name ^"(" ^ args ^ ");\n"
        end
        else if stack_call then begin
          (*
          print_endline ("[handle_closure] GENERATING STACK CALL for " ^ id);
          *)
          (if with_comments
          then "      //run procedure " ^ src_str ^ "\n"
          else "") ^
          "      {\n" ^
          subs ^
          "      " ^ name ^ Flx_gen_display.strd the_display props^ "\n" ^
          "      .stack_call(" ^ args ^ ");\n" ^
          "      }\n"
        end
        else
        let ptrmap = name ^ "_ptr_map" in
        begin
          match kind with
          | Function ->
(*
            (if with_comments
            then 
*)
            "      //run procedure " ^ src_str ^ "\n"
(*
            else "") 
*)
            ^
            "      {\n" ^
            subs ^
            "        ::flx::rtl::con_t *_p =\n" ^
            "          (FLX_NEWP(" ^ name ^ ")" ^ Flx_gen_display.strd the_display props^ ")\n" ^
            "          ->call(" ^ args ^ ");\n" ^
            "        while(_p) {\n" ^
            "          if(_p->p_svc) {\n" ^
            "            int svc = _p->p_svc->variant;\n" ^
            "            fprintf(stderr,\"Function calls procedure which does service call %d: %s\\n\",\n"^
            "                svc,::flx::rtl::describe_service_call(svc));\n"^
            "            fprintf(stderr,\"Caller "^caller_name^"\\n\");\n" ^
            "            fprintf(stderr,\"Calls  "^called_name^"\\n\");\n" ^
            "            abort();\n" ^
            "          }\n"^
            "          _p=_p->resume();\n" ^
            "        }\n"^
            "      }\n"

          | Procedure ->
            let call_string =
              "      return (FLX_NEWP(" ^ name ^ ")" ^ Flx_gen_display.strd the_display props ^ ")" ^
              "\n      ->call(" ^ args ^ ");\n"
            in
            if is_jump
            then
              (if with_comments then
              "      //jump to procedure " ^ src_str ^ "\n"
              else "") ^
              "      {\n" ^
              subs ^
              "      ::flx::rtl::con_t *tmp = _caller;\n" ^
              "      _caller = 0;\n" ^
              call_string ^
              "      }\n"
            else
            (
              needs_switch := true;
              (if with_comments then
              "      //call procedure " ^ src_str ^ "\n"
              else ""
              )
              ^

              sub_start ^
              "      FLX_SET_PC(" ^ cid_of_bid n ^ ")\n" ^
              call_string ^
              sub_end ^
              "    FLX_CASE_LABEL(" ^ cid_of_bid n ^ ")\n"
            )
        end
      end
    in
    begin
    match Flx_bsym.bbdcl bsym with
    | BBDCL_external_fun (_,vs,_,BTYP_fix (0,_),_,_,`Code code) ->

      (* there is no jump_prim so an assertion this should be "is_jump"
        will fail
      *)
      if length vs <> length ts then
      clierrx "[flx_cpp_backend/flx_gen_exe.ml:280: E301] " sr "[gen_prim_call] Wrong number of type arguments"
      ;
      let wc s =
        (if with_comments then "      // " ^ src_str ^ "\n" else "") ^
        sub_start ^
        "      " ^ s ^ ";\n" ^ (* NOTE added semi-colon .. hack .. *)
        sub_end
      in
      let ws s =
        let s = sc "expr" s in wc s
      in
      begin match code with
      | CS.Identity -> syserr sr "Identity proc is nonsense"
      | CS.Virtual ->
          clierr2 (Flx_bexe.get_srcref exe) (Flx_bsym.sr bsym) ("Instantiate virtual procedure(1) " ^ Flx_bsym.id bsym) ;
      | CS.Str s -> wc s
      | CS.Str_template s ->
        let ss = gen_prim_call 
           syms bsym_table 
           shapes shape_map 
           tsub ge' 
           s 
           ts 
           a 
           (Flx_btype.btyp_none()) sr (Flx_bsym.sr bsym) 
           "atom"  (Flx_bsym.id bsym) 
        in
        ws ss
      end

    | BBDCL_external_fun (_,vs,_,BTYP_void,_,_,`Code code) ->
      assert (not is_jump);

      if length vs <> length ts then
      clierrx "[flx_cpp_backend/flx_gen_exe.ml:314: E302] " sr "[gen_prim_call] Wrong number of type arguments"
      ;
      let wc s = 
        (if with_comments then "      // " ^ src_str ^ "\n" else "") ^
        sub_start ^
        "      " ^ s ^ "\n" ^
        sub_end
      in
      let ws s =
        let s = sc "expr" s in wc s
      in
      begin match code with
      | CS.Identity -> syserr sr "Identity proc is nonsense"
      | CS.Virtual ->
          clierr2 (Flx_bexe.get_srcref exe) (Flx_bsym.sr bsym) ("Instantiate virtual procedure(1) " ^ Flx_bsym.id bsym) ;
      | CS.Str s -> wc s
      | CS.Str_template s ->
        let ss = gen_prim_call syms bsym_table shapes shape_map tsub ge' s ts a (Flx_btype.btyp_none()) sr (Flx_bsym.sr bsym) "atom" (Flx_bsym.id bsym) in
        ws ss
      end

    | BBDCL_external_fun (_,vs,ps_cf,ret,_,_,`Callback _) ->
      assert (ret = btyp_void ());

      if length vs <> length ts then
      clierrx "[flx_cpp_backend/flx_gen_exe.ml:339: E303] " sr "[gen_prim_call] Wrong number of type arguments"
      ;
      let s = Flx_bsym.id bsym ^ "($a);" in
      let s =
        gen_prim_call syms bsym_table shapes shape_map tsub ge' s ts a (Flx_btype.btyp_none()) sr (Flx_bsym.sr bsym) "atom" (Flx_bsym.id bsym)
      in
      let s = sc "expr" s in
      (if with_comments then "      // " ^ src_str ^ "\n" else "") ^
      sub_start ^
      "      " ^ s ^ "\n" ^
      sub_end
      ^
      (* NOTE: the "stackable" here should refer to the context not the primcall *)
      begin 
        if is_jump then 
          if stackable then
            "      return; //  Callback\n"
          else
            (if !needs_switch then 
            "      FLX_KILLPC\n" 
            else "") ^
            "      FLX_RETURN // Callback: procedure return\n"
        else ""
      end

    | BBDCL_fun (props,vs,ps,ret,effects,bexes) ->
      begin match ret with
      | BTYP_void 
      | BTYP_fix (0,_) -> handle_call props vs ps ret bexes
      | _ ->
        failwith
        (
          "[gen_exe] Expected '" ^ Flx_bsym.id bsym ^ "' to be procedure constant, got function " ^
          string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) index
        )
      end
    | _ ->
      failwith
      (
        "[gen_exe] Expected '" ^ Flx_bsym.id bsym ^ "' to be procedure constant, got " ^
        string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) index
      )
    end
  in
  let gen_nonlocal_goto pc frame index =
    (* WHAT THIS CODE DOES: we pop the call stack until
       we find the first ancestor containing the target label,
       set the pc there, and return its continuation to the
       driver; we know the address of this frame because
       it must be in this function's display.
    *)
    let target_instance =
      try Hashtbl.find syms.instances (frame, ts)
      with Not_found -> failwith ("[Flx_gen_exe:gen_nonlocal_goto] Can't find instance for "^string_of_int frame)
    in
    let label = cid_of_flxid (get_label_name bsym_table index) in 
    let frame_ptr = "ptr" ^ cpp_instance_name syms bsym_table frame ts in
    "      // non local goto " ^ label ^ "\n" ^
    "      {\n" ^
    "        ::flx::rtl::con_t *tmp1 = this;\n" ^
    "        while(tmp1 && " ^ frame_ptr ^ "!= tmp1)\n" ^
    "        {\n" ^
    "          ::flx::rtl::con_t *tmp2 = tmp1->_caller;\n" ^
    "          tmp1 -> _caller = 0;\n" ^
    "          tmp1 = tmp2;\n" ^
    "        }\n" ^
    "      }\n" ^
    "      " ^ frame_ptr ^ "->pc = FLX_FARTARGET(" ^ cid_of_bid pc ^ "," ^ cid_of_bid target_instance ^ "," ^ label ^ ");\n" ^
    "      return " ^ frame_ptr ^ ";\n"
  in
  let forget_template sr s = match s with
  | CS.Identity -> syserr sr "Identity proc is nonsense(2)!"
  | CS.Virtual -> clierrx "[flx_cpp_backend/flx_gen_exe.ml:408: E304] " sr "Instantiate virtual procedure(2)!"
  | CS.Str s -> s
  | CS.Str_template s -> s
  in
  let rec gexe exe =
    (*
    print_endline (string_of_bexe bsym_table 0 exe);
    *)
    (if with_comments then "    // # " ^ Flx_srcref.short_string_of_src sr ^ "\n"
    else "") ^
    match exe with
    | BEXE_try _ -> "  try {\n";
    | BEXE_endtry _ -> "\n  }//end try/catch\n";
    | BEXE_catch (sr, s, t) -> "\n}\n  catch (" ^tn t^ " &" ^s^") {\n";

    | BEXE_axiom_check _ -> assert false

    | BEXE_nonreturn_code (sr,code,a)
    | BEXE_code (sr,code, a) ->
      let a = match a with (a,t) -> a, tsub t in
      let subs,x = Flx_unravel.unravel syms bsym_table a in
      let subs = List.map (fun ((e,t),s) -> (e,tsub t), cid_of_flxid s) subs in
      let subs =
        catmap ""
        (fun ((_,typ) as e,s) ->
          match typ with
          | BTYP_tuple [] -> ""
          | _ ->
          let t = cpp_ltypename syms bsym_table typ in
          let e = ge sr e in
          "      " ^ t ^ " " ^ s ^ " = " ^ e ^ ";\n"
        )
        subs
      in
      let sub_start =
        if String.length subs = 0 then ""
        else "      {\n" ^ subs
      and sub_end =
        if String.length subs = 0 then ""
        else "      }\n"
      in
      let wc s = 
        (if with_comments then "      // [BEXE_code] " ^ src_str ^ "\n" else "") ^
        sub_start ^
        "      " ^ s ^ ";\n" ^ (* NOTE added semi-colon .. hack .. *)
        sub_end
      in
      let ws s =
        let s = sc "expr" s in wc s
      in
      begin match code with
      | CS.Identity -> syserr sr "Identity proc is nonsense"
      | CS.Virtual ->
          clierr2 (Flx_bexe.get_srcref exe) (Flx_bsym.sr bsym) ("Instantiate virtual procedure(1) " ^ Flx_bsym.id bsym) ;
      | CS.Str s -> wc s
      | CS.Str_template s ->
        let ss = gen_prim_call 
          syms bsym_table shapes shape_map tsub ge' 
          s [] a 
          (Flx_btype.btyp_none()) sr (Flx_bsym.sr bsym) 
          "atom" ("inline_code") 
        in
        ws ss
      end


    | BEXE_comment (_,s) -> "/*" ^ s ^ "*/\n"
    | BEXE_label (_,label_index) ->
      let label_kind = get_label_kind_from_index label_info.label_usage label_index in
      let label = cid_of_flxid (get_label_name bsym_table label_index) in
      (match kind with
        | Procedure ->
          begin match label_kind with
          | `Far ->
            needs_switch := true;
            "    FLX_LABEL(" ^ cid_of_bid label_index ^ "," ^
              cid_of_bid instance_no ^ "," ^ label ^ ")\n"
          | `Near ->
            "    " ^label ^ ":;\n"
          | `Unused -> ""
          end

        | Function ->
          begin match label_kind with
          | `Far -> failwith ("[gen_exe] In function " ^ Flx_bsym.id bsym ^ 
              ": Non-local going to label " ^label)
          | `Near ->
            "    " ^ label ^ ":;\n"
          | `Unused -> ""
          end
      )

    (* FIX THIS TO PUT SOURCE REFERENCE IN *)
    | BEXE_halt (sr,msg) ->
      let msg = Flx_print.string_of_string ("HALT: " ^ msg) in
      let f, sl, sc, el, ec = Flx_srcref.to_tuple sr in
      let s = Flx_print.string_of_string f ^"," ^
        si sl ^ "," ^ si sc ^ "," ^
        si el ^ "," ^ si ec
      in
       "      FLX_HALT(" ^ s ^ "," ^ msg ^ ");\n"

    | BEXE_trace (sr,v,msg) ->
      let msg = Flx_print.string_of_string ("TRACE: " ^ msg) in
      let f, sl, sc, el, ec = Flx_srcref.to_tuple sr in
      let s = Flx_print.string_of_string f ^"," ^
        si sl ^ "," ^ si sc ^ "," ^
        si el ^ "," ^ si ec
      in
       "      FLX_TRACE(" ^ v ^"," ^ s ^ "," ^ msg ^ ");\n"


    | BEXE_goto (sr,idx) ->
      let distance = Flx_label.find_label_distance bsym_table label_info.labels_by_proc this idx in
      begin match distance with
      | `Local ->
        let cname = cid_of_flxid (get_label_name bsym_table idx) in
        "      goto " ^ cname ^ ";\n"
      | `Nonlocal -> 
        let pc = get_label_pc idx in
        let frame = get_label_frame bsym_table idx in
        gen_nonlocal_goto pc frame idx
      | `Unreachable ->
        clierrx "[flx_cpp_backend/flx_gen_exe.ml:531: E305] " sr ("Unconditional Jump to Unreachable label " ^ string_of_int idx)
      end

    | BEXE_ifgoto (sr,e,idx) ->
      let distance = Flx_label.find_label_distance bsym_table label_info.labels_by_proc this idx in
      begin match distance with
      | `Local ->
        let cname = cid_of_flxid (get_label_name bsym_table idx) in
        "      if(" ^ ge sr e ^ ") goto " ^ cname ^ ";\n"
      | `Nonlocal ->
        let skip = "_skip_" ^ cid_of_bid (fresh_bid syms.counter) in
        let not_e = ce_prefix "!" (ge' sr e) in
        let not_e = string_of_cexpr not_e in
        let pc = get_label_pc idx in
        let frame = get_label_frame bsym_table idx in
        "      if("^not_e^") goto " ^ cid_of_flxid skip ^ ";\n"  ^
        gen_nonlocal_goto pc frame idx ^
        "    " ^ cid_of_flxid skip ^ ":;\n"

      | `Unreachable ->
        syserr sr ("Conditional Jump to unreachable label " ^ string_of_int idx)
      end


    | BEXE_cgoto (sr, e) ->
      (* Computed goto. Expression e must resolve to a label expression of C++ type jump_address_t *)
      needs_switch := true;
      let e = ge sr e in
      (* temporarily ignore stack unwinding issues .. and other issues too *) 
      "      FLX_DIRECT_LONG_JUMP(" ^ e ^ ")\n"

    | BEXE_ifcgoto (sr, e1,e2) ->
      (* Computed goto. Expression e must resolve to a label expression of C++ type jump_address_t *)
      needs_switch := true;
      let skip = "_skip_" ^ cid_of_bid (fresh_bid syms.counter) in
      let not_e = ce_prefix "!" (ge' sr e1) in
      let not_e = string_of_cexpr not_e in

      (* temporarily ignore stack unwinding issues .. and other issues too *) 
      let e2 = ge sr e2 in
      "      if("^not_e^") goto " ^ cid_of_flxid skip ^ ";\n"  ^
      "      FLX_DIRECT_LONG_JUMP(" ^ e2 ^ ")\n" ^
      "    " ^ cid_of_flxid skip ^ ":;\n"
       

    (* Hmmm .. stack calls ?? *)
    | BEXE_call_stack (sr,index,ts,a)  ->
      let bsym =
        try Flx_bsym_table.find bsym_table index with _ ->
          failwith ("[gen_expr(apply instance)] Can't find index " ^
            string_of_bid index)
      in
      let ge_arg ((x,t) as a) =
        let t = tsub t in
        match t with
        | BTYP_tuple [] -> ""
        | _ -> ge sr a
      in
      let nth_type ts i = match ts with
        | BTYP_tuple ts -> nth ts i
        | BTYP_array (t,BTYP_unitsum n) -> assert (i<n); t
        | _ -> assert false
      in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,ps,BTYP_fix (0,_),effects,_)
      | BBDCL_fun (props,vs,ps,BTYP_void,effects,_) ->
        assert (mem `Stack_closure props);
        let a = match a with (a,t) -> a, tsub t in
        let ts = List.map tsub ts in
        (* C FUNCTION CALL *)
        if mem `Cfun props || mem `Pure props && not (mem `Heap_closure props) then
          let display = get_display_list bsym_table index in
          let name = cpp_instance_name syms bsym_table index ts in
          assert (length display = 0);
          let prjs = Flx_bparams.get_prjs ps in
          let args = List.map (fun (_,prj) -> match prj with
            | None -> a
            | Some ((_,BTYP_function (_,c)) as prj) -> bexpr_apply c (prj,a) 
            | _ -> assert false
            ) prjs
          in
          let s = String.concat "," (List.map (fun x -> ge sr x) args) in
          let s =
            if mem `Requires_ptf props then
              if String.length s > 0 then "FLX_FPAR_PASS " ^ s
              else "FLX_FPAR_PASS_ONLY"
            else s
          in
          "  " ^ name ^ "(" ^ s ^ ");\n"
        else
          let subs,x = Flx_unravel.unravel syms bsym_table a in
          let subs = List.map
            (fun ((e,t),s) -> (e,tsub t), cid_of_flxid s)
            subs
          in
          handle_closure sr false index ts subs x true
      | _ -> failwith "procedure expected"
      end


    | BEXE_call_prim (sr,index,ts,a)
    | BEXE_call_direct (sr,index,ts,a)
    | BEXE_call (sr,(BEXPR_closure (index,ts),_),a) ->
      let a = match a with (a,t) -> a, tsub t in
      let subs,x = Flx_unravel.unravel syms bsym_table a in
      let subs = List.map (fun ((e,t),s) -> (e,tsub t), cid_of_flxid s) subs in
      let ts = List.map tsub ts in
      handle_closure sr false index ts subs x false

    (* i1: variable
       i2, class_ts: class closure
       i3: constructor
       a: ctor argument
    *)
    | BEXE_jump (sr,((BEXPR_closure (index,ts),_)),a)
    | BEXE_jump_direct (sr,index,ts,a) ->
      let a = match a with (a,t) -> a, tsub t in
      let subs,x = Flx_unravel.unravel syms bsym_table a in
      let subs = List.map (fun ((e,t),s) -> (e,tsub t), cid_of_flxid s) subs in
      let ts = List.map tsub ts in
      handle_closure sr true index ts subs x false

    (* Note: same semantics in both functions and procedures.
       so call with trap CANNOT handle service calls
    *)
    | BEXE_call_with_trap (sr,p,a) ->
      let args =
        let this = "0" in
        match a with
          | _,BTYP_tuple [] -> this
          | _ -> this ^ ", " ^ ge sr a
      in
      (if with_comments then
        "      //run procedure " ^ src_str ^ "\n"
      else "") ^
        "      {\n" ^
        "        ::flx::rtl::con_t *_p = ("^ge sr p ^ ")->clone()\n      ->call("^args^");\n" ^
        "        retry: while(_p) {\n" ^
        "          if(_p->p_svc) {\n" ^
        "            int svc = _p->p_svc->variant;\n" ^
        "            fprintf(stderr,\"call_with_trap procedure which does service call %d: %s\\n\",\n"^
        "                svc,::flx::rtl::describe_service_call(svc));\n"^
        "            abort();\n" ^
        "          }\n"^
        "          try { _p=_p->resume(); } \n" ^
        "          catch (::flx::rtl::con_t *_q) { _p = _q; goto retry; }\n" ^
        "        }\n"^
        "      }\n"


    | BEXE_call (sr,p,a) ->
      let args =
        let this = match kind with
          | Procedure -> "this"
          | Function -> "0"
        in
        match a with
        | _,BTYP_tuple [] -> this
        | _ -> this ^ ", " ^ ge sr a
      in
      begin let _,t = p in match t with
      | BTYP_cfunction (d,_) ->
        begin match d with
        | BTYP_tuple ts ->
          begin match a with
          | BEXPR_tuple xs,_ ->
            let s = String.concat ", " (List.map (fun x -> ge sr x) xs) in
            (ge sr p) ^"(" ^ s ^ ");\n"
          | _ ->
           failwith "[flx_gen_exe][tuple] can't split up arg to C function yet"
          end
        | BTYP_array (t,BTYP_unitsum n) ->
          let ts = 
           let rec aux ts n = if n = 0 then ts else aux (t::ts) (n-1) in
           aux [] n
          in
          begin match a with
          | BEXPR_tuple xs,_ ->
            let s = String.concat ", " (List.map (fun x -> ge sr x) xs) in
            (ge sr p) ^"(" ^ s ^ ");\n"
          | _ ->
            failwith "[flx_gen_exe][array] can't split up arg to C function yet"
          end

        | _ ->
          (ge sr p) ^"(" ^ ge sr a ^ ");\n"
        end
      | _ ->
      match kind with
      | Function ->
        (if with_comments then
        "      //run procedure " ^ src_str ^ "\n"
        else "") ^
        "      {\n" ^
        "        ::flx::rtl::con_t *_p = ("^ge sr p ^ ")->clone()\n      ->call("^args^");\n" ^
        "        while(_p) {\n" ^
        "          if(_p->p_svc) {\n" ^
        "            int svc = _p->p_svc->variant;\n" ^
        "            fprintf(stderr,\"Function calls procedure which does service call %d: %s\\n\",\n"^
        "                svc,::flx::rtl::describe_service_call(svc));\n"^
        "            abort();\n" ^
        "          }\n"^
        "          _p=_p->resume();\n" ^
        "        }\n"^
        "      }\n"



      | Procedure ->
        needs_switch := true;
        let n = fresh_bid counter in
        (if with_comments then
        "      //"^ src_str ^ "\n"
        else "") ^
        "      FLX_SET_PC(" ^ cid_of_bid n ^ ")\n" ^
        "      return (" ^ ge sr p ^ ")->clone()\n      ->call(" ^ args ^");\n" ^
        "    FLX_CASE_LABEL(" ^ cid_of_bid n ^ ")\n"
      end

    | BEXE_jump (sr,p,a) ->
      let args = match a with
        | _,BTYP_tuple [] -> "tmp"
        | _ -> "tmp, " ^ ge sr a
      in
      begin let _,t = p in match t with
      | BTYP_cfunction _ ->
        "    "^ge sr p ^ "("^ge sr a^");\n"
      | _ ->
      (if with_comments then
      "      //"^ src_str ^ "\n"
      else "") ^
      "      {\n" ^
      "        ::flx::rtl::con_t *tmp = _caller;\n" ^
      "        _caller=0;\n" ^
      "        return (" ^ ge sr p ^ ")->clone()\n      ->call(" ^ args ^");//tail call (BEXE_jump)\n" ^
      "      }\n"
      end

    | BEXE_proc_return _ ->
      begin match kind with
      | Procedure ->
        if stackable then
        "      return; // proc return from stackable \n"
        else
        (if !needs_switch then 
        "      FLX_KILLPC\n" 
        else "") ^
        "      FLX_RETURN // procedure return\n"
      | Function ->
        clierrx "[flx_cpp_backend/flx_gen_exe.ml:800: E306] " sr "Function contains procedure return";
      end

    | BEXE_svc (sr,index) ->
      begin match stackable, kind with
        | false,Procedure ->
        let bsym =
          try Flx_bsym_table.find bsym_table index with _ ->
            failwith ("[gen_expr(name)] Can't find index " ^ string_of_bid index)
        in
        let t =
          match Flx_bsym.bbdcl bsym with
          | BBDCL_val (_,t,(`Val | `Var | `Once)) -> t
          | _ -> syserr (Flx_bsym.sr bsym) "Expected read argument to be variable"
        in
        let n = fresh_bid counter in
        needs_switch := true;
        "      //read variable\n" ^
        "      p_svc = &" ^ get_var_ref syms bsym_table this index ts^";\n" ^
        "      FLX_SET_PC(" ^ cid_of_bid n ^ ")\n" ^
        "      return this;\n" ^
        "    FLX_CASE_LABEL(" ^ cid_of_bid n ^ ")\n"
      | true,Procedure ->
        clierrx "[flx_cpp_backend/flx_gen_exe.ml:823: E307] " sr ("Stackable procedure contains service call")
      | _,Function ->
        clierrx "[flx_cpp_backend/flx_gen_exe.ml:825: E308] " sr ("Function contains service call")
      end

    | BEXE_yield (sr,e) ->
      let labno = fresh_bid counter in
      let code =
        "      FLX_SET_PC(" ^ cid_of_bid labno ^ ")\n" ^
        (
          let _,t = e in
          (if with_comments then
          "      //" ^ src_str ^ ": type "^tn t^"\n"
          else "") ^
          "      return "^ge sr e^";//yield\n"
        )
        ^
        "    FLX_CASE_LABEL(" ^ cid_of_bid labno ^ ")\n"
      in
      needs_switch := true;
      code

    | BEXE_fun_return (sr,e) ->
      let _,t = e in
      (if with_comments then
      "      //" ^ src_str ^ ": type "^tn t^"\n"
      else "") ^
      (* HACK WARNING! *)
      begin match t with
      | BTYP_fix (0,_) -> "      "^ge sr e^"; // non-returning\n"
      | _ ->          "      return "^ge sr e^";\n"
      end

    | BEXE_nop (_,s) -> "      //Nop: " ^ s ^ "\n"

    | BEXE_storeat (sr,l,r) ->
       "      *"^ge sr l^"="^ge sr r ^"; // storeat\n"

    | BEXE_assign (sr,(_,lhst as e1),(_,rhst as e2)) ->
      if lhst = btyp_unit () then "" else
      let comment = (if with_comments then "      //"^src_str^"\n" else "") in
      (* j: component to assign, ts: types of components *)
      let rec aux1 ls i out = 
         match ls with 
         | [] -> assert false 
         | h :: t ->
           if i = 0 then out,h
           else aux1 t (i-1) (sizeof_linear_type bsym_table h * out)
      in 
      let assign_to_packed_tuple lhs rhs j ts =
        let lo,elt = aux1 (List.rev ts) (List.length ts - j - 1) 1 in
        let elt = sizeof_linear_type bsym_table elt in
    (*
    print_endline ("Type of variable is " ^ sbt bsym_table t');
    print_endline ("proj = " ^ si j^ ", Size of component = " ^ si elt ^ ", size of lower bit = " ^ si lo);
    *)
        let celt = ce_int elt in
        let clo = ce_int lo in
        let clomelt = ce_int (lo * elt) in
        let ad x y = ce_add x y in
        let di x y = ce_div x y in
        let mu x y = ce_mul x y in
        let mo x y = ce_rmd x y in
        let nuval =  ad (mu (ad (mu (di lhs clomelt) celt) rhs) clo) (mo lhs clo) in
        nuval
      in
      let assign_to_packed_array lhs rhs ix vt array_len =
        let seq = syms.Flx_mtypes2.counter in
        let power_table = syms.Flx_mtypes2.power_table in
        let ipow' base exp = 
          match exp with
          | `Ce_int i -> 
            let rec ipow = begin function 0 -> 1 | n -> base * (ipow (n - 1)) end in
            ce_int (ipow i)
          | _ ->
            let ipow = Flx_ixgen.get_power_table bsym_table power_table base array_len in
            ce_array (ce_atom ipow) exp
        in
        let elt = sizeof_linear_type bsym_table vt in
        let celt = ce_int elt in
        let rix = ce_sub (ce_sub (ce_int array_len) ix) (ce_int 1) in
        let clo = ipow' elt ix in 
        let clomelt = ce_mul clo celt in
        let ad x y = ce_add x y in
        let di x y = ce_div x y in
        let mu x y = ce_mul x y in
        let mo x y = ce_rmd x y in
        let nuval =  ad (mu (ad (mu (di lhs clomelt) celt) rhs) clo) (mo lhs clo) in
        nuval
      in

      let rec split e trail = 
        match e with
        | BEXPR_varname _,t
        | BEXPR_deref _,t ->
          let t' = snd e in ge' sr e, t', trail

        | BEXPR_apply ((BEXPR_prj (_,d,_),_ as p), arg ),t
        | BEXPR_apply ((BEXPR_aprj (_,d,_),_ as p), arg ),t-> 
          if islinear_type bsym_table d then split arg ((p,t)::trail)
          else let t' = snd e in ge' sr e,t',trail
        | x -> 
          print_endline src_str;
          clierrx "[flx_cpp_backend/flx_gen_exe.ml:926: E309] " sr ("variable required on lhs of assignment got lhs = " ^ sbe bsym_table x)
      in
      let lv,lvt,prjs = split e1 [] in
      begin match prjs with
      | [] -> 
          comment ^ 
          "      "^ 
          string_of_cexpr lv ^ " = " ^ ge sr e2 ^ "; //assign simple\n"
      | _ -> 
(*
        print_endline src_str;
        print_endline ("Assign to projection of compact linear type " ^ sbt bsym_table lvt); 
*)
        (* This is a functional update of the lhs with the rhs. The lhs is divided
           into three parts: hi, mid, lo. We return hi, rhs, lo.

           If there is a projection in the list, however, then the rhs we have
           is too small. We have split mid into hi2 mid2 lo2 and return
           hi2 rhs lo2. So mid is the new lhs.
        *)
        let rec evp lhs rhs prjs =
          match prjs with
          | [] -> rhs
          | ((BEXPR_prj (j,BTYP_tuple ts,c),_ as p),_) :: tl ->
            (* should work for 1 component *)
            let lo,elt = aux1 (List.rev ts) (List.length ts - j - 1) 1 in
            let elt = sizeof_linear_type bsym_table elt in
            let celt = ce_int elt in
            let clo = ce_int lo in
            let mid = ce_rmd (ce_div lhs clo) celt in
            let rhs = evp mid rhs tl in
            assign_to_packed_tuple lhs rhs j ts 

          | ((BEXPR_prj (j,BTYP_array (vt,BTYP_unitsum n),c),_),_) :: tl ->
            (* should work for 1 component *)
            let ts = let rec aux ts n = if n = 0 then ts else aux (vt::ts) (n-1) in aux [] n in
            let lo,elt = aux1 (List.rev ts) (List.length ts - j - 1) 1 in
            let elt = sizeof_linear_type bsym_table elt in
            let celt = ce_int elt in
            let clo = ce_int lo in
            let mid = ce_rmd (ce_div lhs clo) celt in
            let rhs = evp mid rhs tl in
            assign_to_packed_tuple lhs rhs j ts 

          | ((BEXPR_prj (ix,t,c),_),_) :: tl -> assert false

          | ((BEXPR_aprj (ix,BTYP_array (vt,aixt),c),_),_) :: tl ->
            let array_len = Flx_btype.sizeof_linear_type bsym_table aixt in
            let seq = syms.Flx_mtypes2.counter in
            let power_table = syms.Flx_mtypes2.power_table in
            let ipow' base exp = 
              match exp with
              | `Ce_int i -> 
                let rec ipow = begin function 0 -> 1 | n -> base * (ipow (n - 1)) end in
                ce_int (ipow i)
              | _ ->
                let ipow = Flx_ixgen.get_power_table bsym_table power_table base array_len in
                ce_array (ce_atom ipow) exp
            in
            let array_value_size = sizeof_linear_type bsym_table vt in
            let ix = ge' sr ix in
            let sdiv = ipow' array_value_size (ce_sub (ce_int (array_len - 1)) ix) in
            let mid = (ce_rmd (ce_div lhs sdiv) (ce_int array_value_size)) in
            let rhs = evp mid rhs tl in
            assign_to_packed_array lhs rhs ix vt array_len 

          | _ -> assert false
        in
        let newv = evp (ce_atom "lv") (ce_atom "rv") prjs in
        comment ^
        "     { // assign compact\n" ^
        "        " ^ tn lvt ^ " & lv = " ^ string_of_cexpr lv ^ ";\n" ^
        "        " ^ (let t' = snd e2 in tn t') ^ " const & rv = " ^ ge sr e2 ^ ";\n" ^
        "        lv = " ^ string_of_cexpr newv ^ ";" ^
        "     }\n"
      end
(*
(*
print_endline "Assignment";
*)
      let lhsprojoflinear = Flx_ixgen.projoflinear bsym_table  e1  in
      let t = tsub rhst in
      let comment = (if with_comments then "      //"^src_str^"\n" else "") in
      begin match t with
      | BTYP_tuple [] -> ""
      | _ when lhsprojoflinear ->
(*
print_endline "PROJ OF LINEAR";
*)
        begin match e1 with
        | BEXPR_apply ((BEXPR_prj (j,_,_),_),(_,(BTYP_tuple ts as t') as var)),_ ->
          let n = List.length ts in
          comment ^ 
          "      "^ 
          Flx_ixgen.assign_to_packed_tuple bsym_table ge' ge sr e2 n j ts t' var

        | BEXPR_apply ((BEXPR_prj (j,_,_),_),(_,(BTYP_array (vt,BTYP_unitsum n) as t') as var)),_ ->

print_endline ("Assign to array component " ^ si j ^ ", array = " ^ sbe bsym_table var);
          let ts =  (* list of n vt's *)
            let rec aux n out = 
              match n with 
              | 1 -> out 
              | _ -> aux (n-1) (vt::out) 
            in 
            aux n [vt] 
          in 
          assert (List.length ts = n);
          comment ^ 
          "      "^ 
          Flx_ixgen.assign_to_packed_tuple bsym_table ge' ge sr e2 n j ts t' var

        | _ -> assert false
        end
      | _ ->
(*
print_endline ("Assign type = " ^ sbt bsym_table lhst ^ " lhs term = " ^ sbe bsym_table e1);
*)
        if e1 = e2 then "" else (* eliminate a self-assign .. shouldn't happen but seems to! *)
        comment ^ 
        "      "^ ge sr e1 ^ " = " ^ ge sr e2 ^
        "; //assign\n"
      end
*)

    | BEXE_init (sr,v,((_,t) as e)) ->
      let t = tsub t in
      begin match t with
      | BTYP_tuple [] -> ""
      | _ ->
        let bsym =
          try Flx_bsym_table.find bsym_table v with Not_found ->
            failwith ("[gen_exe] can't find index " ^ string_of_bid v)
        in
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_val (vs,vt,kind) ->
(*
print_endline ("gen_exe: " ^ string_of_bexe bsym_table 0 exe);
print_endline ("init " ^ Flx_bsym.id bsym ^"< instno="^si instance_no^",this="^ si this^ ">:\nLHS type = "^
      sbt bsym_table vt^ "\nRHS type = " ^ sbt bsym_table t ^ "\nLHS ts = " ^ catmap "," (sbt bsym_table) ts);
*)
            (if with_comments then "      //"^src_str^"\n" else "") ^
            "      " ^
            begin match kind with
            | `Tmp -> get_variable_typename syms bsym_table v [] ^ " "
            | _ -> ""
            end ^
            get_ref_ref syms bsym_table this v ts ^
            " " ^
            " = " ^
            ge sr e ^
            "; //init\n"
          | _ -> assert false
        end
      end

    | BEXE_begin -> "      {\n"
    | BEXE_end -> "      }\n"

    | BEXE_assert (sr,e) ->
       let f, sl, sc, el, ec = Flx_srcref.to_tuple sr in
       let s = string_of_string f ^ "," ^
         si sl ^ "," ^ si sc ^ "," ^
         si el ^ "," ^ si ec
       in
       "      {if(FLX_UNLIKELY(!(" ^ ge sr e ^ ")))\n" ^
       "        FLX_ASSERT_FAILURE("^s^");}\n"

    | BEXE_assert2 (sr,sr2,e1,e2) ->
       print_endline "ASSERT2";
       let f, sl, sc, el, ec = Flx_srcref.to_tuple sr in
       let s = string_of_string f ^ "," ^
         si sl ^ "," ^ si sc ^ "," ^
         si el ^ "," ^ si ec
       in
       let f2, sl2, sc2, el2, ec2 = Flx_srcref.to_tuple sr2 in
       let s2 = string_of_string f2 ^ "," ^
         si sl2 ^ "," ^ si sc2 ^ "," ^
         si el2 ^ "," ^ si ec2
       in
       (match e1 with
       | None ->
       "      {if(FLX_UNLIKELY(!(" ^ ge sr e2 ^ ")))\n"
       | Some e ->
       "      {if(FLX_UNLIKELY("^ge sr e^" && !(" ^ ge sr e2 ^ ")))\n"
       )
       ^
       "        FLX_ASSERT2_FAILURE("^s^"," ^ s2 ^");}\n"

    | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
       (*
       print_endline "AXIOM CHECK";
       *)
       let f, sl, sc, el, ec = Flx_srcref.to_tuple sr in
       let s = string_of_string f ^ "," ^
         si sl ^ "," ^ si sc ^ "," ^
         si el ^ "," ^ si ec
       in
       let f2, sl2, sc2, el2, ec2 = Flx_srcref.to_tuple sr2 in
       let s2 = string_of_string f2 ^ "," ^
         si sl2 ^ "," ^ si sc2 ^ "," ^
         si el2 ^ "," ^ si ec2
       in
       try
       (match e1 with
       | None ->
       "      {if(FLX_UNLIKELY(!(" ^ ge sr e2 ^ ")))\n"
       | Some e ->
       "      {if(FLX_UNLIKELY("^ge sr e^" && !(" ^ ge sr e2 ^ ")))\n"
       )
       ^
       "        FLX_AXIOM_CHECK_FAILURE("^s^"," ^ s2 ^");}\n"
       with _ ->
         print_endline "ELIDING FAULTY AXIOM CHECK -- typeclass virtual instantiation failure?";
         ""


  in gexe exe

let gen_exes
  filename cxx_name
  syms
  bsym_table
  shapes shape_map
  display
  label_info
  counter
  index
  exes
  vs
  ts
  instance_no
  stackable
=
  let needs_switch = ref false in
  let b = Buffer.create (200 * List.length exes) in 
  try
    List.iter 
     (fun exe -> Buffer.add_string b 
      (gen_exe filename cxx_name syms bsym_table shapes shape_map
      label_info counter index vs
      ts instance_no needs_switch stackable exe))
    exes
    ;
    Buffer.contents b,!needs_switch
  with exn ->
    print_endline ("Error generating code for "^cxx_name^"  exes=\n");
    List.iter (fun exe -> print_endline (Flx_print.string_of_bexe bsym_table 2 exe)) exes;
  raise exn


