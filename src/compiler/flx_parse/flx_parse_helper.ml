open Lexing
open Flx_drules
open Ocs_types
open Sex_types
open Dyp
open Flx_string
open Flx_token
open Flx_drules

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

type action_t = Action_Scheme of string | Action_None | Action_Expr of sval | Action_Statements of sval

type symbol_t = 
  | Grammar_Nonterminal of string * priority_relation_t
  | Grammar_String of string
  | Grammar_Regex of Dyp.regexp 
  | Grammar_Group of dyalt_t list
  | Grammar_Macro_Call of string * dyalt_t list list
  | Grammar_Star of symbol_t
  | Grammar_Plus of symbol_t
  | Grammar_Quest of symbol_t

and dyalt_t = symbol_t list * Flx_srcref.t * action_t * anote_t

type privacy_t = Privacy_Public | Privacy_Private
type grammar_rule_t = 
  | Rule_Scheme_rule of privacy_t * string * priority_level_t * string list * dyalt_t list
  | Rule_Requires of string list 
  | Rule_Priorities of string list
  | Rule_Regex of string * Dyp.regexp 
  | Rule_Nop
 

let lexeme x = Dyp.lexeme x
let silly_strtoken k = Flx_prelex.string_of_token k

let fresh_dssl = {
  Flx_token.regexps = [];
  Flx_token.prios = [];
  Flx_token.rules = [];
  Flx_token.deps = [];
  Flx_token.privacy = Drules.empty;
}
let debug = try ignore(Sys.getenv "FLX_DEBUG_PARSER"); true with Not_found -> false

let global_data = {
  Flx_token.pcounter = ref 1;
  Flx_token.env = Flx_ocs_init.init_env ();
  Flx_token.pdebug = ref debug;
  Flx_token.parsing_device = ref None;
}

let local_data = {
  Flx_token.global_regexps = [];
  Flx_token.drules = Drules.empty;
  Flx_token.installed_dssls = [];
  Flx_token.scm = [];
  Flx_token.instances = [];
(*
  Flx_token.rev_stmts = [];
*)
  Flx_token.rev_stmts_as_scheme = [];
}


let xsr sr =
  match Flx_srcref.to_tuple sr with f,fl,fc,ll,lc ->
  Ocs_misc.make_slist Snull ((Sint lc) :: (Sint ll) :: (Sint fc) :: (Sint ll) :: (Sstring f) :: [])

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

(* Add a production to a dssl *)
let define_scheme sr dyp global_regexps dssl_record dssl name prio rhs (scm:string) =
(*
print_endline ("define_scheme " ^ name);
*)
  let mapnt name = try Drules.find name dssl_record.privacy with Not_found -> name in
  let pr_age = !(dyp.global_data.pcounter) in
  incr (dyp.global_data.pcounter);
  let name = mapnt name in

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

  (* Translate Felix production to Dypgen production *)
  let rule = 
    let f o = match o with
      | STRING s ->  (* Handle strings like "fun" in productions *)
        Dyp.Regexp (Dyp.RE_String s)

      | REGEX re -> 
(*
        print_endline "Translating some kind of regexp";
*)
        let re = re_expand global_regexps dssl_record.regexps re in
        Dyp.Regexp re

      | NONTERMINAL (s,Priority_Var) ->  assert false (* variables should have been replaced *)

      | NONTERMINAL (s,p) -> (* handle identifiers like sexpr in productions *)
        let nt = mapnt s in
        let ntpri = cal_priority_relation p in
        Dyp.Non_ter (nt,ntpri)

      | NAME s -> assert false (* should have been converted to NONTERMINAL *)

      | s -> (* anything else is an actual keyword or symbol so make a Dypgen terminal *)
        let name = Flx_prelex.name_of_token s in
        Dyp.Ter name
    in
     name,(List.map f rhs),priority,[] 
  in
  if !(dyp.global_data.pdebug) then
    print_endline (
      "Rule " ^ string_of_int pr_age ^ " " ^ name ^ "["^priority^"] := " ^
      catmap " " silly_strtoken rhs ^
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
        "[buffered] Reducing Rule " ^ string_of_int pr_age ^ " for " ^ name ^ "[" ^
        priority ^ "], scm=" ^ scm ^ "\n"
      );

      print_endline (
        "Reducing Rule " ^ string_of_int pr_age ^ " for " ^ name ^ "[" ^
        priority ^ "] := " ^ catmap " " silly_strtoken rhs ^ " =>#'" ^ scm ^
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
            Sstring s1

          | STRING _, `Lexeme_matched s -> (* print_endline ("Matched regexp to " ^ s); *) Sstring s

          | REGEX _, `Lexeme_matched s -> (* print_endline ("Matched regexp to " ^ s); *) Sstring s

          | k , _ ->
          print_endline ("Woops, unhandled token=" ^ Flx_prelex.string_of_token k);
          Sstring (Flx_prelex.string_of_token k)
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
      let filebase = Filename.basename (Flx_srcref.file sr) in
      let filenase = munge filebase in 
      Ocs_env.set_glob env v2 (Sstring filebase)
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
  rule,action,Bind_to_cons [(name, "Obj_sexpr")]

(* top routine to add a new production to a dssl *)
let extend_grammar dyp (dssl,(name,prio,params,prod,action,anote,sr)) =
  let m = dyp.local_data in
  let dssl_record = Drules.find dssl m.drules in
  let global_regexps = m.global_regexps in
  if params = [] then
    let r = define_scheme sr dyp global_regexps dssl_record dssl name prio prod action in
    Some r
  else
    None

(* ------------------------------------------------------ *)
let dflt_action prod =
  let rn = ref 1 in
  let action =
      "`(" ^
      List.fold_left (fun acc _ -> let n = !rn in incr rn;
        (if acc = "" then "" else acc ^ " ") ^ ",_" ^ string_of_int n
      ) "" prod
      ^ ")"
  in
  (*
  print_endline ("DEFAULT ACTION for " ^
    catmap " " silly_strtoken prod ^
    " =># " ^ action)
  ;
  *)
  action

let cal_action prod action =
  match action with
  | Action_None -> dflt_action prod
  | Action_Expr scm -> 
    let x =  
      "(SUBST (quote " ^ Ocs_print.string_of_ocs scm ^ ") " ^ 
      " (vector 0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20))" 
    in
    x

  | Action_Statements scm -> 
    let x =  
      "(SUBST `(ast_seq ,_sr " ^ Ocs_print.string_of_ocs scm ^ ") " ^ 
      " (vector 0 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20))" 
    in
    x

  | Action_Scheme scm -> scm

type instance_t = (string * priority_relation_t) * string
type instances_t = instance_t list

let rec mksingle sr pcounter params sym =
  let mks sym = mksingle sr pcounter params sym in
  match sym with
  | Grammar_Nonterminal (s,p)  -> NONTERMINAL (s,p),[]
  | Grammar_String s  -> STRING s,[]
  | Grammar_Regex r  -> REGEX r,[]
  | Grammar_Group dyalts ->
    let x = string_of_int (!pcounter) in incr pcounter;
    let sl = "__grp_" ^ x in
    let rules = fixup_alternatives pcounter sl Priority_Default params dyalts in
    NONTERMINAL (sl, Priority_None),rules

  | Grammar_Star h -> 
    let s,extras = mks h in
    let x = string_of_int (!pcounter) in incr pcounter;
    let slr = "__rlist_"^x in
    let sl = "__list_"^x in
    let rule0 = slr,Priority_Default,params,[NONTERMINAL(sl,Priority_None)],"(reverse _1)","",sr in
    let rule1 = sl,Priority_Default,params,[NONTERMINAL(sl,Priority_None);s],"(cons _2 _1)","",sr in
    let rule2 = sl,Priority_Default,params,[],"'()","",sr in
    NONTERMINAL (slr,Priority_None),rule0::rule1::rule2::extras

  | Grammar_Plus h -> 
    let s,extras = mks h in
    let x = string_of_int (!pcounter) in incr pcounter;
    let slr = "__nerlist_"^x in
    let sl = "__nelist_"^x in
    let rule0 = slr,Priority_Default,params,[NONTERMINAL(sl,Priority_None)],"(reverse _1)","",sr in
    let rule1 = sl,Priority_Default,params,[NONTERMINAL(sl,Priority_None);s],"(cons _2 _1)","",sr in
    let rule2 = sl,Priority_Default,params,[s],"`(,_1)","",sr in
    NONTERMINAL (slr,Priority_None),rule0::rule1::rule2::extras

  | Grammar_Quest h -> 
    let s,extras = mks h in
    let x = string_of_int (!pcounter) in incr pcounter;
    let sl ="__opt_"^x in
    let rule1 = sl,Priority_Default,params,[s],"`(,_1)","",sr in
    let rule2 = sl,Priority_Default,params,[],"()","",sr in
    NONTERMINAL (sl,Priority_None), rule1::rule2::extras 

 | Grammar_Macro_Call (mac,args) -> assert false

and flatten sr pcounter params rhs =
  let rec aux inp out extras = match inp with
  | [] -> List.rev out,extras
  | h::t -> 
    let tok,rules = mksingle sr pcounter params h in
    aux t (tok::out) (rules@extras)
  in aux rhs [] [] 

and fixup_rule sr pcounter params rhs =
  let rhs,extras = flatten sr pcounter params rhs in
  let extras =
    List.map (fun (name,prio,params',prod,action,anote,sr) -> 
      assert (params' = []); (* no inner macros *)
      name,prio,params,prod , action,anote,sr
    ) 
    extras
  in
  rhs,extras

and fixup_alternatives pcounter name prio params dyalts =
  let rules =
    List.fold_left
      (fun rules (rhs,sr,action,anote) ->
        let prod,extras = fixup_rule sr pcounter params rhs in
        let action : string = cal_action prod action in
        ((name,prio,params,prod,action,anote,sr) :: extras) @ rules
      )
      [] dyalts
  in
  List.rev rules

let add_rule global_data local_data dssl rule =
  let m = local_data in
  let d = try Drules.find dssl m.drules with Not_found -> fresh_dssl in
  match rule with
  | Rule_Nop -> global_data,m

  | Rule_Scheme_rule (privacy,name,prio,params,dyalts) ->
     let rules = fixup_alternatives global_data.pcounter name prio params dyalts in
     let rules = List.fold_left (fun acc rule -> uniq_add rule acc) d.rules rules in
     let instances = [] in (* FIXME *)
     let privacy =
       match privacy with
       | Privacy_Private ->
          let n = !(global_data.pcounter) in incr (global_data.pcounter);
          let secret = "_"^name^"_"^string_of_int n in
          Drules.add name secret d.privacy
       | Privacy_Public -> d.privacy
     in
     let d = { d with rules = rules; privacy = privacy  } in
     let m = { m with drules = Drules.add dssl d m.drules; instances=instances } in
     global_data,m

  | Rule_Requires ls ->
     let d = { d with deps = ls @ d.deps } in
     let m = { m with drules = Drules.add dssl d m.drules } in
     global_data,m

  | Rule_Priorities p ->
    let d = { d with prios = p::d.prios } in
    let m = { m with drules = Drules.add dssl d m.drules } in
    global_data, m

  | Rule_Regex (name, re) -> (* do nothing at the moment *)
(*
print_endline ("Add Regex name=" ^ name);
*)
    let d = { d with regexps = (name,re)::d.regexps } in
    let m = { m with 
      drules = Drules.add dssl d m.drules;
(* This is WRONG but it suffices for testing .. later use something like
   syntax regdef fred = ... ; 
   for global regexps
*)
      global_regexps = (name,re)::m.global_regexps
    } 
    in
    global_data, m

(* replace non-terminal variables with non-terminal constants *)
(* Note: a non-terminal with a priority like x[>p] is still actually
   a specific non-terminal.

   This routine "monomorphises" a production, provided the 
   replacement map has a replacement for each parameter.
*)
(*
type paramsubst_t = string * (string * priority_relation_t) 

let rec substitute_vars (rpls:paramsubst_t list) (rhs: symbol_t list) =
  let rec aux inp out =
    match inp with
    | [] -> List.rev out
    | Grammar_Atom (NONTERMINAL (s,Priority_Var)) as h :: t ->
      let h = 
        try Grammar_Atom (NONTERMINAL (List.assoc s rpls))
        with Not_found -> h
      in
      aux t (h::out)
    | Grammar_Atom (NAME s) as h :: t ->
      let h = 
        try Grammar_Atom (NONTERMINAL (List.assoc s rpls))
        with Not_found -> h
      in
      aux t (h::out)
    | Grammar_Group dyalts :: tail ->
      let h = Grammar_Group (List.map (fun (syms,sr,action,anote) -> substitute_vars rpls syms,sr,action,anote) dyalts) in
      aux tail (h :: out)

    | h :: t -> aux t (h::out)
  in aux rhs []

let rec print_symbols (symbols:symbol_t list) = 
  List.iter (fun sym -> match sym with
    | Grammar_Atom tok -> print_string (Flx_prelex.name_of_token tok ^ "(" ^Flx_prelex.string_of_token tok ^ ") ")
    | Grammar_Group grp -> print_string "("; 
        List.iter (fun (alt,_,_,_)  -> print_string "| "; print_symbols alt; ) grp;
        print_string ") ";
    )
  symbols

let rec inst_dyalt counter instance_map unprocessed macros name prio (symbols, sr, action, anote) =
(*
if name = "extn0" then begin 
  print_endline "Inst a dyalt for extn0"; 
  print_symbols symbols;
end;
*)
  let rec aux inp out = match inp with
  | [] -> List.rev out 
  | Grammar_Atom (NAME s) :: 
    Grammar_Atom LESS :: 
    Grammar_Atom ( NAME sa) :: 
    Grammar_Atom GREATER :: 
    tail ->
(*
print_endline ("FOUND MACRO CALL " ^ s ^ "<" ^ sa ^ ">");
*)
    let sp = Priority_None in
    let maybe_instance = 
      try Some (Hashtbl.find instance_map (s,sa,sp) ) with Not_found -> None 
    in
    let newsym = 
      match maybe_instance with
      | None ->
        (* create a new nonterminal *)
        let x = string_of_int (!counter) in incr counter;
        let sl = s ^ "__inst_"^x in
(* print_endline ("Create new nonterminal " ^ sl); *)
        let pri = Priority_None in
        (* add mapping to hash table *)
        Hashtbl.add instance_map (s,sa,sp) (sl,pri);
        (* find all the rules for the macro *)
        let macros = List.filter (fun (name,rule) -> name = s) macros in
        (* now create a specialised copy of these rules 
          with the parameter replaced by the argument 
        *)
(* print_endline ("Found " ^ string_of_int (List.length macros) ^ " macros productions named " ^ s); *)
        List.iter (fun (name,rule) -> match rule with
        | Rule_Scheme_rule (privacy,name,prio,params,dyalts) ->
(* print_endline ("Found macro " ^ name); *)
          if List.length params <> 1 then begin
            print_endline ("wrong number of parameters for macro " ^ name ^ ", one required (at the moment)");
            assert false
          end;
          let param : string = List.hd params in
          let rpls = [param, (sa,sp)] in
          let dyalts = List.map 
            (
              fun (symbols, sr, action,anote) -> 
              substitute_vars rpls symbols, sr, action, anote
            ) 
            dyalts 
          in
          let new_rule = Rule_Scheme_rule (privacy,sl,prio,[],dyalts) in
(* print_endline ("Added monomorphised rule " ^ sl ^ " := " );  
List.iter (fun (syms,_,_,_) -> print_symbols syms; print_endline " EOR") dyalts; print_endline "----";
*)
          unprocessed := new_rule :: !unprocessed
        | _ -> assert false
        )
        macros
        ;
        Grammar_Atom (NONTERMINAL (sl,pri))
      | Some (s,p) -> Grammar_Atom (NONTERMINAL (s,p))
    in 
    aux tail (newsym::out)
  | Grammar_Group dyalts :: tail ->
    let newsym =  
      Grammar_Group (List.map (inst_dyalt counter instance_map unprocessed macros name prio) dyalts) 
    in
    aux tail (newsym::out)

  | h :: t -> aux t (h::out)
  in
  let symbols = aux symbols [] in 
(*
if name = "extn0" then begin 
  print_endline "Instantiated: a dyalt for extn0"; 
  print_symbols symbols;
end;
*)
  symbols,sr,action,anote

let inst_rule counter instance_map unprocessed macros rule : grammar_rule_t =
  match rule with
  | Rule_Scheme_rule (privacy,name,prio,[],dyalts) ->
(*
if name = "extn0" then begin print_endline "Instantiating dyalts for extn0"; end;
*)
    let dyalts = List.map (inst_dyalt counter instance_map unprocessed macros name prio) dyalts in
    Rule_Scheme_rule (privacy,name,prio,[],dyalts) 

  | _ -> print_endline "WOOPS!! Expected mono production"; assert false

let inst_rules counter instance_map macros monos : grammar_rule_t list = 
  let unprocessed = ref monos in
  let processed = ref [] in
  while List.length (!unprocessed) > 0 do
     let rule = List.hd (!unprocessed) in
     unprocessed := List.tl (!unprocessed);
     let new_rule = inst_rule counter instance_map unprocessed macros rule in
     processed := new_rule :: !processed;
  done;
  List.rev (!processed)

let instantiate_rules name counter rules =
  (* split rules up *)
  let macros, monos, others = 
    List.fold_left (fun (macros, monos, others) rule -> match rule with
     | Rule_Scheme_rule (privacy,name,prio,params,dyalts) ->
      if params = [] then macros, rule::monos, others
      else (name,rule) :: macros, monos, others

     | _ -> macros,monos,rule::others
   ) ([],[],[]) rules
  in
  let macros,monos,others = List.rev macros, List.rev monos, List.rev others in
  let instance_map = Hashtbl.create 97 in
  let monos = inst_rules counter instance_map macros monos in
  let result =  others @ monos  in
  result

*)
