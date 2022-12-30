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

let debug = false

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
  print_endline ("vs = " ^ catmap "," (fun (s,i,k) -> s ^ "->" ^ si i) vs);
  print_endline ("ts = " ^ catmap ","  (sbt bsym_table) ts);
*)
  let tsub t = beta_reduce "gen_exe" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  let ge sr e = gen_expr syms bsym_table shapes shape_map label_info this vs ts sr e in
  let ge' = gen_expr' syms bsym_table shapes shape_map label_info this vs ts in
  let tn t = cpp_typename syms bsym_table (tsub t) in
  let bsym =
    try Flx_bsym_table.find bsym_table this with _ ->
      failwith ("[gen_exe] Can't find this " ^ string_of_bid this)
  in
  let our_display = get_display_list bsym_table this in
  let caller_name = Flx_bsym.id bsym in
  let kind = match Flx_bsym.bbdcl bsym with
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
        let this = if is_ehandler then "" else match kind with
          | Function ->
            if is_jump 
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
          (* this code should work for an ESCAPE function too *)
          (if with_comments
          then "      //call cproc " ^ src_str ^ "\n"
          else "") ^
          "      " ^ name ^"(" ^ args ^ ");\n"
        end
        else if stack_call then begin
          (*
          print_endline ("[handle_closure] GENERATING STACK CALL for " ^ id);
          *)
         if is_ehandler then begin
            let _,argt = a in
              "      // stack call ESCAPE FUNCTION\n" ^
              "      {\n" ^
              subs ^
              "      " ^ name ^ Flx_gen_display.strd the_display props ^ 
              "\n      .apply (" ^ args ^ ");\n" ^
              "      }\n"
          end else

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
          if is_ehandler then begin
            let _,argt = a in
              "      // heap call ESCAPE FUNCTION\n" ^
              "      {\n" ^
              subs ^
              "      (FLX_NEWP(" ^ name ^ ")" ^ Flx_gen_display.strd the_display props ^ ")" ^
              "\n      ->apply (" ^ args ^ ");\n" ^
              "      }\n"
          end else
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
            "            int svc = _p->p_svc->svc_req;\n" ^
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
    if debug then
    print_endline ("GEN EXE: " ^ string_of_bexe bsym_table 0 exe);

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
              if String.length s > 0 then "ptf, " ^ s
              else "ptf"
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
        "            int svc = _p->p_svc->svc_req;\n" ^
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
        "            int svc = _p->p_svc->svc_req;\n" ^
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
      | BTYP_function (_, BTYP_fix (0,_)) ->
        "    " ^ ge sr p ^ "->apply(" ^ ge sr a ^ ");//tail call ESCAPE FUN (BEXE_jump)\n"
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
        "      p_svc = (::flx::rtl::svc_req_t*)&" ^ get_var_ref syms bsym_table this index ts^";\n" ^
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

      begin match e with
      | BEXPR_varname _,BTYP_fix (0,_) ->
        "  // elide return of variable of type any\n"

      | BEXPR_coerce (  (BEXPR_varname _,BTYP_fix (0,_)),_),_ ->
        "  // elide return of coerced variable of type any\n"

      | BEXPR_coerce (  (_,BTYP_fix (0,_)) as x,_),_ ->
        "      "^ge sr x^"; // return ESCAPE non-returning\n"

      | _ ->

      let _,t = e in
      (if with_comments then
      "      //" ^ src_str ^ ": type "^tn t^"\n"
      else "") ^
      (* HACK WARNING! *)
      begin match t with
      | BTYP_fix (0,_) -> "      "^ge sr e^"; // non-returning\n"
      | _ ->          "      return "^ge sr e^"; // "^tn t^ "\n"
      end
      end

    | BEXE_nop (_,s) -> "      //Nop: " ^ s ^ "\n"

    | BEXE_storeat (sr,(_,lt as l),r) ->
      begin match lt with
      (* use C++ procedure for compact linear pointers *)
      | BTYP_ptr (_,_,[_]) ->
(*
print_endline ("Storeat, clt case: " ^ sbe bsym_table l ^ " <- " ^ sbe bsym_table r);
*)
        "     ::flx::rtl::storeat("^ge sr l ^","^ge sr r^"); // cltpointer\n";
      (* use standard syntax for ordinary pointers *)
      | BTYP_ptr (_,_,[]) ->
       "      *"^ge sr l^"="^ge sr r ^"; // storeat\n"
      (* dunno what to do with abstract types yet! *)
      | _ -> assert false
      end


    | BEXE_assign (sr,v,((_,t) as e))
    | BEXE_init (sr,v,((_,t) as e)) ->
(*
print_endline ("BEXE_INIT, RHS type = " ^ Flx_btype.st t);
*)
      let t = tsub t in
(*
print_endline ("BEXE_INIT, RHS type after tsub = " ^ Flx_btype.st t);
*)

      (* if the RHS is a variable, the side effect has already happend I hope, sp
         it is safe to elide the assignment, since the variable carries no
         information anyhow
      *)
      begin match e with
      | BEXPR_varname _,BTYP_fix (0,_) ->
        "  // elide assignment of variable of type any to LHS\n"

      | BEXPR_coerce (  (BEXPR_varname _,BTYP_fix (0,_)),_),_ ->
        "  // elide assignment of coerced variable of type any to LHS\n"

      | BEXPR_coerce ( (_,BTYP_fix(0,_)) as x,_),_ ->
        "    " ^ ge sr x ^ "; //init or assign expr coerced from type 'any' replaced by evaluation\n"

      | _ ->
      begin match t with
      | BTYP_tuple [] -> ""

      | BTYP_void -> assert false
      | BTYP_fix (0,_) -> 
        (* NOTE: this MAY NOT WORK. 
           It WILL work if the RHS is a C binding.
           If it's a 'function' or 'procedure' which is reduced to a C style
           Felix function it should also work.

           If it's a Felix function .. the apply() should work.
           If it's a procedure it will NOT work, a procedure
           has to be 'called' by a micro scheduler ...

           So we need to treat the assignment as if it were a call to the RHS ..
           because it actually is.
        *)
        "    " ^ ge sr e ^ "; //init or assign type 'any' replaced by evaluation\n"

      | _ ->
        let bsym =
          try Flx_bsym_table.find bsym_table v with Not_found ->
            failwith ("[gen_exe] can't find index " ^ string_of_bid v)
        in
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_val (vs,vt,kind) ->

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
            "; //init or assign\n"
          | _ -> assert false
        end
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


