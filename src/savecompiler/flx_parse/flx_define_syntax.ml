
open Lexing
open Flx_drules
open Ocs_types
open Sex_types
open Dyp
open Flx_string
open Flx_token
open Flx_drules
let catmap sep fn ls = String.concat sep (List.map fn ls)

let uniq_add elt lst =
  if List.mem elt lst then lst else (elt::lst)


let silly_strtoken k = Flx_prelex.string_of_token k

let string_of_processed_scheme_rule (priv,name,prio,toks,action,note,sr) =
  let priority = match prio with
  | Priority_Default -> "default_priority"
  | Priority_Name p -> p
  in
  let privacy = match priv with
  | Privacy_Private -> "private"
  | Privacy_Public ->  "public "
  in
  (privacy ^ 
    " Rule " ^ name ^ "["^priority^"] := " ^
    catmap " " silly_strtoken toks ^
    " =># " ^ (* action *) "ACTION"
  )

let string_of_grammar_rule rule = 
  match rule with
  | Rule_Processed_Scheme_rule rule -> string_of_processed_scheme_rule rule
  | _ -> "Other rule"

let find_macros macros name = 
  let ms = 
    List.fold_left (fun ms (mname,mdef) -> if name = mname then (mdef::ms) else ms) 
    [] 
    macros
  in
  let nparams = 
    List.fold_left (fun m mdef -> 
      match mdef with 
      | Rule_Unprocessed_Scheme_rule (_,_,_,params,_) -> 
        let n = List.length params in
        if m == -1 then n else if m == n then m else
        failwith ("Macro " ^ name ^ 
          " inconsistent number of parameters in productions, expected " ^ string_of_int n)
      | _ -> assert false
    )
    (-1)
    ms
  in
  nparams,ms

type mapping_t = (string  * token) list

let rec calargs sr pcounter dssls macros mapping processed unprocessed args =
   List.map (mksingle sr pcounter dssls macros mapping processed unprocessed) args

and mapnt mapping (s,p) = 
  try 
    let tok = List.assoc s mapping in
    match p with
    | Priority_None -> tok
    | _ -> failwith ("For macro parameter "^ s ^": use must be without priority")
  with Not_found -> NONTERMINAL (s,p)

(* Translate parser symbol into token *)
and mksingle sr pcounter dssls macros (mapping:mapping_t) processed unprocessed (sym:symbol_t): token =
  let mapnt (s,p) = mapnt mapping (s,p) in

  let mks sym = mksingle sr pcounter dssls macros mapping processed unprocessed sym in
  match sym with
  | Grammar_Nonterminal (s,p)  -> mapnt (s,p)
  | Grammar_String s  -> STRING s
  | Grammar_Regex r  -> REGEX r

  | Grammar_Group dyalts ->
    let x = string_of_int (!pcounter) in incr pcounter;
    let sl = "__grp_" ^ x in
    fixup_alternatives pcounter dssls macros Privacy_Private sl Priority_Default mapping processed unprocessed dyalts;
    NONTERMINAL (sl, Priority_None)

  | Grammar_Star h -> 
    let s = mks h in
    let x = string_of_int (!pcounter) in incr pcounter;
    let slr = "__rlist_"^x in
    let sl = "__list_"^x in
    let rule0 = Rule_Processed_Scheme_rule (Privacy_Private,slr,Priority_Default,[NONTERMINAL(sl,Priority_None)],"(reverse _1)","",sr) in
    let rule1 = Rule_Processed_Scheme_rule (Privacy_Private,sl,Priority_Default,[NONTERMINAL(sl,Priority_None);s],"(cons _2 _1)","",sr) in
    let rule2 = Rule_Processed_Scheme_rule (Privacy_Private,sl,Priority_Default,[],"'()","",sr) in
    processed := rule0::rule1::rule2::!processed;
    NONTERMINAL (slr,Priority_None)

  | Grammar_Plus h -> 
    let s = mks h in
    let x = string_of_int (!pcounter) in incr pcounter;
    let slr = "__nerlist_"^x in
    let sl = "__nelist_"^x in
    let rule0 = Rule_Processed_Scheme_rule (Privacy_Private,slr,Priority_Default,[NONTERMINAL(sl,Priority_None)],"(reverse _1)","",sr) in
    let rule1 = Rule_Processed_Scheme_rule (Privacy_Private,sl,Priority_Default,[NONTERMINAL(sl,Priority_None);s],"(cons _2 _1)","",sr) in
    let rule2 = Rule_Processed_Scheme_rule (Privacy_Private,sl,Priority_Default,[s],"`(,_1)","",sr) in
    processed := rule0::rule1::rule2::!processed;
    NONTERMINAL (slr,Priority_None)

  | Grammar_Quest h -> 
    let s = mks h in
    let x = string_of_int (!pcounter) in incr pcounter;
    let sl ="__opt_"^x in
    let rule1 = Rule_Processed_Scheme_rule (Privacy_Private,sl,Priority_Default,[s],"`(,_1)","",sr) in
    let rule2 = Rule_Processed_Scheme_rule (Privacy_Private,sl,Priority_Default,[],"()","",sr) in
    processed := rule1::rule2::!processed;
    NONTERMINAL (sl,Priority_None)

 | Grammar_Macro_Call (mac,args) ->
   let args = calargs sr pcounter dssls macros mapping processed unprocessed args in  
   let nparams, macs = find_macros macros mac in
(*
   print_endline ("Found " ^ string_of_int (List.length macs) ^ 
     " macros with " ^ string_of_int nparams ^ " params for " ^ mac); 
*) 
   let x = string_of_int (!pcounter) in incr pcounter;
   let sl = mac^ "_"^x in
   List.iter (fun rule ->
     match rule with
     | Rule_Unprocessed_Scheme_rule (_,_,prio,params,dyalts) ->
       (* Note: there is only one mapping at a time!  *)
       let new_mapping = List.map2 (fun param arg -> param,arg) params args in
       fixup_alternatives pcounter dssls macros Privacy_Private sl prio new_mapping processed unprocessed dyalts
     | _ -> assert false
     )
     macs
   ;
   NONTERMINAL (sl,Priority_None)

 | Grammar_External_Macro_Call (dssl_name,mac,args) ->
   let args = calargs sr pcounter dssls macros mapping processed unprocessed args in  
   let dssl = 
     try Drules.find dssl_name dssls 
     with Not_found -> 
       failwith ("DSSL " ^ dssl_name ^ " not found in external macro call in "^Flx_srcref.short_string_of_src sr)
   in 
   let macros = dssl.macros in
   let nparams, macs = find_macros macros mac in
(*
   print_endline ("Found " ^ string_of_int (List.length macs) ^ 
     " macros with " ^ string_of_int nparams ^ " params for " ^ mac); 
*) 
   let x = string_of_int (!pcounter) in incr pcounter;
   let sl = mac^ "_"^x in
   List.iter (fun rule ->
     match rule with
     | Rule_Unprocessed_Scheme_rule (_,_,prio,params,dyalts) ->
       (* Note: there is only one mapping at a time!  *)
       let new_mapping = List.map2 (fun param arg -> param,arg) params args in
       fixup_alternatives pcounter dssls macros Privacy_Private sl prio new_mapping processed unprocessed dyalts
     | _ -> assert false
     )
     macs
   ;
   NONTERMINAL (sl,Priority_None)



and flatten sr pcounter dssls macros mapping processed unprocessed (rhs:symbol_t list) =
  let rec aux inp out = match inp with
  | [] -> List.rev out
  | h::t -> 
    let tok = mksingle sr pcounter dssls macros mapping processed unprocessed h in
    aux t (tok::out)
  in aux rhs []  

and fixup_alternatives pcounter dssls macros privacy name prio mapping processed unprocessed dyalts =
  List.iter
    (fun (rhs,sr,action,anote) ->
      let prod = flatten sr pcounter dssls macros mapping processed unprocessed rhs in
      let action : string = Flx_action.cal_action prod action in
      processed := Rule_Processed_Scheme_rule (privacy, name,prio,prod,action,anote,sr) :: !processed
    )
    dyalts

let inst_rule counter dssls macros processed unprocessed (rule : grammar_rule_t) =
  let mapping = [] in
  match rule with
  | Rule_Unprocessed_Scheme_rule (privacy,name,prio,[],dyalts) ->
    fixup_alternatives counter dssls macros privacy name prio mapping processed unprocessed dyalts

  | _ -> print_endline "WOOPS!! Expected mono production"; assert false

let inst_rules counter dssls macros monos : grammar_rule_t list = 
  let unprocessed = ref monos in
  let processed = ref [] in
  while List.length (!unprocessed) > 0 do
     let rule = List.hd (!unprocessed) in
     unprocessed := List.tl (!unprocessed);
     inst_rule counter dssls macros processed unprocessed rule;
  done;
  List.rev (!processed)

module RuleSet = Set.Make ( struct type t = rule_t let compare = compare end)

let addentry rulemap name stuff =
  let set : RuleSet.t = 
    try Drules.find name rulemap with Not_found -> RuleSet.empty 
  in
  let set = RuleSet.add stuff set in
  let rulemap = Drules.add name set rulemap in
  rulemap

let reduce_rules (rules : grammar_rule_t list) = 
  let rulemap = List.fold_left (fun rulemap rule -> match rule with
    | Rule_Processed_Scheme_rule (priv,name,prio,toks,act,note,sr as stuff) -> 
      addentry rulemap name stuff
    | _ -> assert false
    )
    Drules.empty
    rules
  in
  (*
  print_endline ("Got rulemap, " ^ string_of_int (Drules.cardinal rulemap) ^ " keys");
*)
  let rulemap2 = Drules.filter (fun name ruleset -> RuleSet.cardinal ruleset = 1) rulemap in
  let rulemap3 = Drules.fold (fun name ruleset acc -> 
    let (priv,_,prio,toks,action,note,sr) as rule = RuleSet.choose ruleset in 
    if List.length toks = 1 && prio = Priority_Default && priv = Privacy_Private && action = "_1" then
      Drules.add name rule acc
    else acc
    )
    rulemap2 
    Drules.empty 
  in
(*
  if Drules.cardinal rulemap3 > 0 then
  print_endline ("Got " ^ string_of_int (Drules.cardinal rulemap3) ^ " single productions to single symbol");
  Drules.iter
  (fun name (priv,_,prio,toks,action,note,sr) ->
    let priority = match prio with
    | Priority_Default -> "default_priority"
    | Priority_Name p -> p
    in
    let privacy = match priv with
    | Privacy_Private -> "private"
    | Privacy_Public ->  "public "
    in
    print_endline (privacy ^ 
      " Rule " ^ name ^ "["^priority^"] := " ^
      catmap " " silly_strtoken toks ^
      " =># " ^ action
    )
  )
  rulemap3;
*)
  (* Now apply reductions *)
  let mapping = Drules.fold (fun name (priv,_,prio,toks,action,note,sr) acc -> 
    match toks with
    | [tok] -> (name,tok) :: acc
    | _ -> assert false
  )
  rulemap3 
  []
  in 
  let maptok mapping tok = match tok with
    | NONTERMINAL (s,Priority_None) -> (try List.assoc s mapping with Not_found -> tok)
    | _ -> tok
  in
  let maprule mapping rule = match rule with
    | Rule_Processed_Scheme_rule (priv,name,prio,toks,act,note,sr) -> 
      let toks = List.map (maptok mapping) toks in
      Rule_Processed_Scheme_rule (priv,name,prio,toks,act,note,sr)
    | _ -> rule
  in
  let rules2 = List.map (
    fun rule -> 
      let rule2 = maprule mapping rule in
(*
      if rule <> rule2 then begin
       print_endline ("OLD: " ^ string_of_grammar_rule rule);
       print_endline ("NEW: " ^ string_of_grammar_rule rule2);
      end;
*)
      rule2
    ) 
    rules 
  in 
  rules2


let process_rules name counter dssls rules : (string * grammar_rule_t) list *  grammar_rule_t list =
  (* split rules up *)
  let macros, monos, others = 
    List.fold_left (fun (macros, monos, others) rule -> match rule with
     | Rule_Unprocessed_Scheme_rule (privacy,name,prio,params,dyalts) ->
      if params = [] then macros, rule::monos, others
      else (name,rule) :: macros, monos, others

     | _ -> macros,monos,rule::others
   ) ([],[],[]) rules
  in
  let macros,monos,others = List.rev macros, List.rev monos, List.rev others in
  let monos = inst_rules counter dssls macros monos in
  let monos = reduce_rules monos in
  let result =  others @ monos  in
  macros,result


