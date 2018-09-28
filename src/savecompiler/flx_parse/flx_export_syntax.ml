open Lexing
open Flx_drules
open Ocs_types
open Sex_types
open Dyp
open Flx_string
open Flx_token
open Flx_drules
open Flx_define_syntax

let munge s = 
  let s2 = ref "" in
  for i=0 to String.length s - 1 do
    let ch = s.[i] in
    if 
      ch >= '0' && ch <= '9' ||
      ch >= 'A' && ch <= 'Z' ||
      ch >= 'a' && ch <= 'z' ||
      ch = '_' 
    then
      s2 := !s2 ^ String.make 1 ch
    else if ch = '.' then
      s2 := !s2 ^ "_"
  done
  ;
  !s2

let catmap sep fn ls = String.concat sep (List.map fn ls)

let uniq_add elt lst =
  if List.mem elt lst then lst else (elt::lst)


let re_expand global_names local_names re = 
  let map = List.map in
  let rec aux re =
    match re with
    | RE_Name s -> 
      begin try
        let re = List.assoc s local_names in
(*
print_endline ("Found " ^ s ^ " as local name");
*)
        aux re
      with Not_found ->
      try
        let re = List.assoc s global_names in
(*
print_endline ("Found " ^ s ^ " as global name");
*)
        aux re
      with Not_found -> 
        print_endline ("Can't find regexp name " ^ s);
        failwith ("Can't find regexp name " ^ s)
      end
    | RE_Alt ls -> RE_Alt (map aux ls)
    | RE_Seq ls -> RE_Seq (map aux ls)
    | RE_Option s -> RE_Option (aux s)
    | RE_Plus s -> RE_Plus (aux s)
    | RE_Star s -> RE_Star (aux s)
    | x -> x
  in aux re


let silly_strtoken k = Flx_prelex.string_of_token k


(* Changed JS 23 Aug 2017. Backwards list for src ref to Ocs,
  f-file, fl-first line, fc-first column, ll-last line, lc-last column
  should read lc,ll,fc,fl,f
  but did read lc,ll,fc,**ll**,f
*)
let xsr sr =
  match Flx_srcref.to_tuple sr with f,fl,fc,ll,lc ->
  Ocs_misc.make_slist Snull ((Sint lc) :: (Sint ll) :: (Sint fc) :: (Sint fl) :: (Sstring (Bytes.of_string f)) :: [])

let buffer_add_ocs b r = Ocs_print.print_to_buffer b false r

let cal_priority_relation (p:priority_relation_t) : string Dyp.nt_prio =
  match p with
  | Priority_Var -> print_endline "ERROR: uninstantiated variable"; assert false
  | Priority_None -> No_priority
  | Priority_Eq p -> Eq_priority p
  | Priority_Less p -> Less_priority p
  | Priority_Lesseq p -> Lesseq_priority p
  | Priority_Greater p -> Greater_priority p
  | Priority_Greatereq p -> Greatereq_priority p

let make_rule expand_regexp privacy_map secret_name priority (rhs:token list) : Dyp.rule =
(* Translate Felix production to Dypgen production *)
  let f o : Dyp.symb = match o with
    | STRING s ->  (* Handle strings like "fun" in productions *)
      Dyp.Regexp (Dyp.RE_String s)

    | REGEX re -> 
(*
      print_endline "Translating some kind of regexp";
*)
      let re = expand_regexp re in
      Dyp.Regexp re

    | NONTERMINAL (s,Priority_Var) ->  assert false (* variables should have been replaced *)

    | NONTERMINAL (s,p) -> (* handle identifiers like sexpr in productions *)
      let nt = privacy_map s in
      let ntpri = cal_priority_relation p in
      Dyp.Non_ter (nt,ntpri)

    | NAME s -> assert false (* should have been converted to NONTERMINAL *)

    | s -> (* anything else is an actual keyword or symbol so make a Dypgen terminal *)
      let name = Flx_prelex.name_of_token s in
      Dyp.Ter name
  in
   secret_name,(List.map f rhs),priority,[] 

(* Add a production to a dssl *)
let define_scheme sr dyp global_regexps dssl_record 
  dssl (privacy:privacy_t) (name:string) prio (rhs:token list) (scm:string) 
=
(*
print_endline ("define_scheme " ^ name);
*)
  let privacy_map name = try Drules.find name dssl_record.privacy with Not_found -> name in
  let pr_age = !(dyp.global_data.pcounter) in
  incr (dyp.global_data.pcounter);
  let secret_name = privacy_map name in

  let cde =
    try
      let l = Flx_ocs_run.scheme_lex sr scm in
      let c = Flx_ocs_run.scheme_compile dyp.global_data.env l in
      c
    with
    | Ocs_error.Error err | Ocs_error.ErrorL (_,err) -> failwith ("Error " ^ err ^ " compiling " ^ scm)
  in

  let priority = match prio with
    | Priority_Default -> "default_priority"
    | Priority_Name p -> p
  in
  let expand_regexp re = re_expand global_regexps dssl_record.regexps re in
  let rule = make_rule expand_regexp privacy_map secret_name priority rhs in
  let strtok tok = 
    let tok = match tok with 
      | NONTERMINAL (s,p) -> NONTERMINAL (privacy_map s,p) 
      | _ -> tok 
    in 
    silly_strtoken tok
  in
  if !(dyp.global_data.pdebug) then
    print_endline (
      "Rule " ^ string_of_int pr_age ^ " " ^ name ^ "("^secret_name^")["^priority^"] := " ^
      catmap " " strtok rhs ^
      " =># " ^ scm
    )
   ;
  (* this is the core routine which defines the action on reducing the production *)
  let action = fun dyp2 avl ->
    match avl,scm with
    (* optimise special case *)
    | [`Obj_sexpr s],"_1" -> `Obj_sexpr s,[]
    | _ ->
    let age = ref pr_age in
    let b = Buffer.create 200 in

    if !(dyp.global_data.pdebug) then begin
      Buffer.add_string b (
        "[buffered] Reducing Rule " ^ string_of_int pr_age ^ " for " ^ secret_name ^ "[" ^
        priority ^ "], scm=" ^ scm ^ "\n"
      );

      print_endline (
        "Reducing Rule " ^ string_of_int pr_age ^ " for " ^ secret_name ^ "[" ^
        priority ^ "] := " ^ catmap " " strtok rhs ^ " =>#'" ^ scm ^
        "'\n"
      );
    end;

    (* let env = Ocs_env.env_copy dyp.local_data.env in *)
    (* let env = dyp.local_data.env in *)
    let env = dyp.global_data.env in

    (* this is the core routine which compares the parsed attributes
     * with the translated production at parse time
     * objs: the symbols of the production
     * syms: the attributes parsed for those symbols 
     *)
    let rec aux objs syms n = match objs, syms with
      | [],[] -> ()
      | [],_ | _,[] -> assert false
      | h1::t1,h2::t2 ->
        let s =
          match h1,h2 with
          | _,`Obj_sexpr s ->
            s

          | k,`Obj_keyword -> assert false; Snull (* Sstring (Flx_prelex.string_of_token k) *)

          | STRING s1, `Obj_NAME s2 ->
            if s1 <> s2 then raise Giveup;
            Sstring (Bytes.of_string s1)

          | STRING _, `Lexeme_matched s -> (* print_endline ("Matched regexp to " ^ s); *) Sstring (Bytes.of_string s)

          | REGEX _, `Lexeme_matched s -> (* print_endline ("Matched regexp to " ^ s); *) Sstring (Bytes.of_string s)

          | k , _ ->
          print_endline ("Woops, unhandled token=" ^ Flx_prelex.string_of_token k);
          Sstring (Bytes.of_string (Flx_prelex.string_of_token k))
        in
        if !(dyp.global_data.pdebug) then begin
          Buffer.add_string b ("Arg " ^ string_of_int n ^ " = ");
          buffer_add_ocs b s; Buffer.add_string b "\n";
        end;
        let v1:Ocs_types.sval = Ocs_sym.get_symbol ("_" ^ string_of_int n) in
        Ocs_env.set_glob env v1 s;
        aux t1 t2 (n+1)
    in
    aux rhs avl 1;
    if !(dyp.global_data.pdebug) then
    Buffer.add_string b "End of arguments\n";
    let sr = Flx_parse_srcref.getsr dyp2 in
(*
print_endline ("sr of reduction is " ^ Flx_srcref.short_string_of_src sr);
*)
    begin
      let v1:Ocs_types.sval = Ocs_sym.get_symbol ("_sr") in
      let v2:Ocs_types.sval = Ocs_sym.get_symbol ("_filebase") in
      (*
      let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = ssr } in
      *)
      Ocs_env.set_glob env v1 (xsr sr);
      let filebase = 
(* this should NOT be necessary because "munge" below should remove the dot (.) 
   but it doesn't .. no idea why not
*)
        let b = Filename.basename (Flx_srcref.file sr) in
        let b = try Filename.chop_extension b with _ -> b in
        b
      in
      let filenase = munge filebase in 
      Ocs_env.set_glob env v2 (Sstring (Bytes.of_string filebase))
    end
    ;
    let r =
      try Flx_ocs_run.scheme_eval cde
      with Ocs_error.Error err | Ocs_error.ErrorL (_,err) ->
        print_string (Buffer.contents b);
        print_string ("Error "^err^" evaluating " ^ scm);
        failwith "Error evaluating Scheme"
    in
    `Obj_sexpr r,[]
  in
  rule,action,Bind_to_cons [(secret_name, "Obj_sexpr")]

(* top routine to add a new production to a dssl *)
let extend_grammar dyp (dssl,rule) = 
  match rule with privacy,name,prio,prod,action,anote,sr ->
  let m = dyp.local_data in
  let dssl_record = Drules.find dssl m.drules in
  let global_regexps = m.global_regexps in
  let r = define_scheme sr dyp global_regexps dssl_record dssl privacy name prio prod action in
  r

(* ------------------------------------------------------ *)
let add_rule global_data local_data dssl_name rule =
  let m = local_data in
  let d = try Drules.find dssl_name m.drules with Not_found -> Flx_parse_data.fresh_dssl in
  match rule with
  | Rule_Nop -> global_data,m

  | Rule_Unprocessed_Scheme_rule _ -> assert false

  | Rule_Processed_Scheme_rule (privacy,name,prio,tokens,action,note,sr as rule_data: rule_t)->
     let rules = uniq_add rule_data d.rules in
     let privacy =
       match privacy with
       | Privacy_Private ->
          let n = !(global_data.pcounter) in incr (global_data.pcounter);
          let secret = "_private_"^name^"_"^string_of_int n in
          Drules.add name secret d.privacy
       | Privacy_Public -> d.privacy
     in
     let d = { d with rules = rules; privacy = privacy  } in
     let m = { m with drules = Drules.add dssl_name d m.drules  } in
     global_data,m

  | Rule_Requires ls ->
     let d = { d with deps = ls @ d.deps } in
     let m = { m with drules = Drules.add dssl_name d m.drules } in
     global_data,m

  | Rule_Priorities p ->
    let d = { d with prios = p::d.prios } in
    let m = { m with drules = Drules.add dssl_name d m.drules } in
    global_data, m

  | Rule_Regex (name, re) -> (* do nothing at the moment *)
    let d = { d with regexps = (name,re)::d.regexps } in
    let m = { m with 
      drules = Drules.add dssl_name d m.drules;
      global_regexps = (name,re)::m.global_regexps
    } 
    in
    global_data, m

let add_rules dssl_name global_data local_data dyprods = 
  List.fold_left 
    (fun (g,l) x -> add_rule g l dssl_name x) 
    (global_data, local_data) 
    dyprods 




