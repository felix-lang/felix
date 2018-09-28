open Parse_tree
open Lexing
open Printf
open Dypgen_parser
open Dypgen_lexer
open Extract_type

let _ = Dyp.dypgen_verbose := 0

let input_file, input_file_short =
  let input_file = !Argument.string_ref in
  let input_file_short = Filename.chop_extension input_file in
  if !Argument.use_cpp then
    (let input_file_cpp = input_file_short^".cpp.dyp" in
    let ec = Sys.command
      ("cpp "^(!Argument.cpp_options)^input_file^" > "^input_file_cpp) in
    if ec <> 0 then exit 2;
    input_file_cpp, input_file_short)
  else input_file, input_file_short

let output_file = input_file_short^".ml"
let temp_output_file =
  if !Argument.no_mli then output_file
  else input_file_short^"_temp.ml"
let extract_type = input_file_short^".extract_type"
let output_file_mli = input_file_short^".mli"


let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)
let () =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_file };
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = input_file }

let parse_result =
  try Dypgen_parser.main Dypgen_lexer.token lexbuf
  with Failure msg -> (
    let b = ref true in
    let () = match !start_dypgen_comment with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nDypgen comment not terminated\n"
        pos.pos_fname line col
    in
    if !start_ocaml_type<>dummy_pos then (
      let line = !start_ocaml_type.pos_lnum in
      let col = !start_ocaml_type.pos_cnum - !start_ocaml_type.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nOcaml type statement not terminated\n"
        !start_ocaml_type.pos_fname line col);
    if !start_pattern<>dummy_pos then (
      let line = !start_pattern.pos_lnum in
      let col = !start_pattern.pos_cnum - !start_pattern.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nOcaml pattern not terminated\n"
        !start_pattern.pos_fname line col);
    if !start_ocaml_code<>dummy_pos then (
      let line = !start_ocaml_code.pos_lnum in
      let col = !start_ocaml_code.pos_cnum - !start_ocaml_code.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nOcaml code not terminated\n"
        !start_ocaml_code.pos_fname line col);
    if !start_string<>dummy_pos then (
      let line = !start_string.pos_lnum in
      let col = !start_string.pos_cnum - !start_string.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nString not terminated\n"
        !start_string.pos_fname line col);
    (*let () = match !start_bracket with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nBracket not closed\n"
        pos.fname line col
    in*)
    let () = match !start_curlyb with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nCurly brace not closed\n"
        pos.pos_fname line col
    in
    let () = match !start_ocaml_comment with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\ndypgen comment not terminated\n"
        pos.pos_fname line col
    in
    if !b then (
      let line2 = lexbuf.lex_curr_p.pos_lnum in
      let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
      let pos1 = lexeme_start_p lexbuf in
      let line1 = pos1.pos_lnum in
      let col1 = pos1.pos_cnum - pos1.pos_bol in
      if line1=line2 then
        fprintf stderr "File \"%s\", line %d, characters %d-%d:\nLexing failed with message: %s\n"
          pos1.pos_fname line2 col1 col2 msg
      else
        fprintf stderr "File \"%s\", from l:%d, c:%d to l:%d, c:%d :\nLexing failed with message: %s\n"
          pos1.pos_fname line1 col1 line2 col2 msg);
    exit 2)
  | Dyp.Syntax_error -> (
      let line2 = lexbuf.lex_curr_p.pos_lnum in
      let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
      let pos1 = lexeme_start_p lexbuf in
      let line1 = pos1.pos_lnum in
      let col1 = pos1.pos_cnum - pos1.pos_bol in
      if line1=line2 then
        fprintf stderr "File \"%s\", line %d, characters %d-%d\nSyntax error\n"
          pos1.pos_fname line2 col1 col2
      else
        fprintf stderr "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nSyntax error\n"
          pos1.pos_fname line1 col1 line2 col2;
    exit 2)



let
  (topheader_main,topheader_main_pos),
  (header_main,header_main_pos),
  token_list,
  relation,
  non_terminal_start_list,
  generic_merge,
  cons_list,
  nt_type_list,
  single_nt_list,
  add_cons_list,
  ppi_layout,
  grammar,
  (*(trailer_main,(trailer_main_pos,trailer_main_offset)),
  (topmli_code,(topmli_code_pos,topmli_code_offset)),
  (midmli_code,(midmli_code_pos,midmli_code_offset)),
  (mli_code,(mli_code_pos,mli_code_offset)),*)
  (trailer_main,trailer_main_pos),
  (topmli_code,topmli_code_pos),
  (midmli_code,midmli_code_pos),
  (mli_code,mli_code_pos),
  lexer
  =
    let mltop,c1,ppi,lexer,g,c2,mlitop,mlimid,c3 =
      fst (List.hd parse_result) in
    (mltop,c1,ppi.token_list,ppi.relation,ppi.start,
    ppi.generic_merge,ppi.cons,ppi.nt_type,ppi.single_nt,
    ppi.additional_cons,ppi.layout,
    g,c2,mlitop,mlimid,c3,lexer)



let test_version_match =
  "let _ =\n  if \""^Argument.dypgen_version^"\" <> Dyp.version
  then (Printf.fprintf stderr
    \"version mismatch, dypgen version "^Argument.dypgen_version^
    " and dyplib version %s\\n\" Dyp.version;\n  exit 2)\n\n"


let regexp_decl, aux_lexer, main_lexer = match lexer with
  | None -> [],[],[]
  | Some (a,b,c) -> a,b,c

let main_lexer = List.fold_left
  (fun ml (re,c) -> (re,"__dypgen_layout",c)::ml)
  main_lexer ppi_layout

(*let regexp_decl = List.fold_left
  (fun l -> function Regexp_decl x -> x::l | _ -> l) [] lexer

let lexer_rules = List.fold_left
  (fun l -> function Lexer_rule x -> x::l | _ -> l) [] lexer*)

let obj_pref = if !Argument.pv_obj then "`" else ""
let token_pref = if !Argument.pv_token then "`" else ""






(*let global_data_type = if global_data_type="" then "unit"
  else "("^global_data_type^")"
let local_data_type = if local_data_type="" then "unit"
  else "("^local_data_type^")"*)

let append_string_to_buffer buf str =
  Buffer.add_string buf str; buf

let ($) = append_string_to_buffer



(* string_list ["a";"b";"c"] returns "[a;b;c]" *)
let string_list sl =
  let aux code s = code^s^";" in
  let code = List.fold_left aux "[" sl in
  let string_length = String.length code in
  (if code="" then "" else String.sub code 0 (string_length-1))^"]"


let dummy_code = "", Lexing.dummy_pos
let dummy_p = -1,-1,-1,""
(*let dummy_pat_syn = "_", (Pat_syn ""), Lexing.dummy_pos*)
let dummy_pat_inh = "_", (Pat_inh ""), Lexing.dummy_pos



let grammar, max_eps =
  let rht = Hashtbl.create (List.length grammar) in
  let max_eps = ref 0 in
  let make_key lhs p symb_list =
    lhs, p,
    List.map (function
      | (Symb_non_terminal ((nt,_),(p,_),ntp,_)), _ ->
          (`nt (nt,p,ntp)), true
      | (Symb_non_terminal_NL ((nt,_),(p,_),ntp,_)), _ ->
          (`nt (nt,p,ntp)), false
      | (Symb_terminal (ter,_)), _ -> (`ter ter), true
      | (Symb_terminal_NL (ter,_)), _ -> (`ter ter), false
      | (Symb_regexp re), _ -> (`re re), true
      | (Symb_regexp_NL re), _ -> (`re re), false
      | (Symb_early_action a), _ -> (`ea a), false)
      symb_list
  in
  let make_epsilon n =
    let nt = "0_"^(string_of_int n) in
    (Symb_non_terminal ((nt,dummy_p),
    ("No_priority",dummy_p),Pr_eq,dummy_code)),
    ("_",(Pat_syn nt),Lexing.dummy_pos)
  in
  let aux ra =
    let lhs, p, symb_list, action, ro = ra in
    if not (List.exists (function
      | (Symb_non_terminal ((nt,_),_,_,inh)), _
      | (Symb_non_terminal_NL ((nt,_),_,_,inh)), _
      when inh <> ("",Lexing.dummy_pos) -> true
      | (Symb_early_action _), _ -> true | _ -> false) symb_list)
    then ra else
      (let key = make_key lhs p symb_list in
      let n = try Hashtbl.find rht key with _ -> 0 in
      Hashtbl.replace rht key (n+1);
      max_eps := max (n+1) !max_eps;
      let eps = make_epsilon (n+1) in
      lhs, p, eps::symb_list, action, ro)
  in
  let grammar = List.map aux grammar in
  grammar, !max_eps



let grammar =
  let make_epsilon_rule n =
    let nt = "0_"^(string_of_int n) in
    ((nt,[dummy_pat_inh]),
    ("default_priority",dummy_p),[],
    ("",(Lexing.dummy_pos, false)),(true,true))
  in
  let rec add_epsilon_rules n res =
    if n = 0 then res else
    add_epsilon_rules (n-1) ((make_epsilon_rule n)::res)
  in
  add_epsilon_rules max_eps grammar





let insert_line_number = "\n# insert-line-number \""^temp_output_file^"\"\n"
let insert_line_number_mli = "\n# insert-line-number \""^output_file_mli^"\"\n"
let sharp_line_number fname = function 0 | -1 -> "\n"
  | lnum -> "\n# "^(string_of_int lnum)^" \""^fname^"\"\n"

let space_string n = String.make (max n 0) ' '


let dummy_line = "\nlet _ = () (* dummy line to improve OCaml error location *)"

let topheader_main = if topheader_main="" then "\n" else
  (sharp_line_number topheader_main_pos.pos_fname topheader_main_pos.pos_lnum)^
  (space_string (topheader_main_pos.pos_cnum-topheader_main_pos.pos_bol))^
  topheader_main^dummy_line^insert_line_number
let header_main = if header_main="" then "\n" else
  (sharp_line_number header_main_pos.pos_fname header_main_pos.pos_lnum)^
  (space_string (header_main_pos.pos_cnum-header_main_pos.pos_bol))^
  header_main^dummy_line^insert_line_number
let trailer_main = if trailer_main = "" then "" else
  (sharp_line_number trailer_main_pos.pos_fname trailer_main_pos.pos_lnum)^
  (space_string (trailer_main_pos.pos_cnum-trailer_main_pos.pos_bol))^
  trailer_main^dummy_line^insert_line_number
let topmli_code = if topmli_code = "" then "\n" else
  (sharp_line_number topmli_code_pos.pos_fname topmli_code_pos.pos_lnum)^
  (space_string (topmli_code_pos.pos_cnum-topmli_code_pos.pos_bol))^
  topmli_code^insert_line_number_mli
let midmli_code = if midmli_code = "" then "\n" else
  (sharp_line_number midmli_code_pos.pos_fname midmli_code_pos.pos_lnum)^
  (space_string (midmli_code_pos.pos_cnum-midmli_code_pos.pos_bol))^
  midmli_code^insert_line_number_mli
let mli_code = if mli_code = "" then "\n" else
  (sharp_line_number mli_code_pos.pos_fname mli_code_pos.pos_lnum)^
  (space_string (mli_code_pos.pos_cnum-mli_code_pos.pos_bol))^
  mli_code^insert_line_number_mli



let nt_type_map =
  let f2 typ nt_type_map nt =
    if typ = "No type" then nt_type_map
    else String_map.add nt typ nt_type_map
  in
  let f1 nt_type_map (typ,nt_list) =
    List.fold_left (f2 typ) nt_type_map nt_list
  in
  let nt_type_map = List.fold_left f1 String_map.empty nt_type_list in
  let f3 nt_type_map (nt,typ) = f2 typ nt_type_map nt in
  List.fold_left f3 nt_type_map non_terminal_start_list



(*let _ =
  print_endline "nt_type_map:";
  String_map.iter (fun a b -> print_endline (a^" "^b)) nt_type_map*)



let code_undef_nt =
  if !Argument.undef_nt then "  let undef_nt = true\n"
  else "  let undef_nt = false\n"



let code_type_token, (*code_export_module,*) token_map =
  let lbra,rbra = if !Argument.pv_token then " [","]" else "","" in
  let code_type_token = "type token ="^lbra^"\n" in
  (*let token_map =
    List.fold_left
      (fun tm (tok,typ) -> String_map.add tok typ tm)
      String_map.empty token_list in*)
  (*let token_map =
    List.fold_left
      (fun tm (_,_,sl,_,_) -> List.fold_left
        (fun tm -> function ((Symb_terminal (tok,_)),_) ->
          String_map.add tok "" tm | _ -> tm)
        tm sl)
      token_map grammar in*)
  let token_map =
    List.fold_left (fun tm (_,n,c) ->
      if c = ("",Lexing.dummy_pos) then String_map.add n "No_type" tm
      else
        if String_map.mem n tm then tm
        else String_map.add n "Unknown" tm)
    String_map.empty main_lexer in
  let aux (code_type_token,token_map) (tok,typ) =
    if typ = "No_type" then
      (code_type_token^"  | "^token_pref^tok^"\n"),
      (String_map.add tok typ token_map)
    else
      (code_type_token^"  | "^token_pref^tok^" of "^typ^"\n"),
      (String_map.add tok typ token_map)
  in
  let code_type_token, token_map =
    List.fold_left aux (code_type_token,token_map) token_list
  in
  let code_type_token = code_type_token^rbra^"\n" in
  (if !Argument.emit_token_type && not !Dypgen_parser.use_dyplex
    then code_type_token else ""),
  (*"module Export_type =\nstruct\n"^
  code_type_token ^"end\ninclude Export_type\n\n",*)
  token_map




let (*code_token_name_decl,*)token_name_map,code_token_nb, token_nb =
  (*let code_token_name_decl = "type token_name = int\n" in*)
  let token_name_map = String_map.empty in
  (*let token_name_map = String_map.add "dummy" 0 String_map.empty in
  let token_name_map = String_map.add "epsilon" 1 token_name_map in*)
  let aux tok _ ((*code,*)n,token_name_map) =
    (*(code^"  let t_"^tok^" = "^(string_of_int n)^"\n",*)
    (n+1),String_map.add tok n token_name_map
  in
  let n,token_name_map =
    String_map.fold aux token_map
      (*(1+(List.length non_terminal_start_list),token_name_map)*)
      (0,token_name_map)
  in
  (*code_token_name_decl,*) token_name_map,
  "  let token_nb = "^(string_of_int n)^"\n", n



let code_get_token_name, code_str_token, code_str_token_name, code_token_name_array =
  let code_get_token_name =
    if !Dypgen_parser.use_dyplex then
      "  let get_token_name () = 0\n"
      (* here () after get_token_name is important to have
      the type unit instead of the type parameter 'token in
      the type parameters of parser_pilot *)
    else
    let code = "  let get_token_name t = match t with\n" in
    let aux tok typ code =
      let tok_id =
        string_of_int (String_map.find tok token_name_map) in
      if typ = "No_type"
      then code^"    | "^token_pref^tok^" -> "^tok_id^"\n"
      else code^"    | "^token_pref^tok^" _ -> "^tok_id^"\n" in
    String_map.fold aux token_map code
  in
  let code_str_token =
    if !Dypgen_parser.use_dyplex then
      "  let str_token _ = failwith \"str_token must not be called with dypgen lexers\"\n" else
    let code = "  let str_token t = match t with\n" in
    let aux tok typ code =
      if typ = "No_type"
      then code^"    | "^token_pref^tok^" -> \""^tok^"\"\n"
      else if typ = "int"
      then code^"    | "^token_pref^tok^" i -> \""^tok^
        "(\"^(string_of_int i)^\")\"\n"
      else if typ = "string"
      then code^"    | "^token_pref^tok^" s -> \""^tok^"(\"^s^\")\"\n"
      else code^"    | "^token_pref^tok^" _ -> \""^tok^"\"\n"
    in
    String_map.fold aux token_map code
  in
  let code_token_name_array, code_str_token_name =
    (*let aux tok _ res = ("    \""^tok^"\"")::res in
    let aux_dummy_tok str (nts,_) =
      (str^"    \"dummy_token_"^nts^"\";\n")
    in
    let dummy_tok_l =
      List.fold_left aux_dummy_tok "" non_terminal_start_list
    in
    "  let token_name_array =\n  [|\"token_epsilon\";\n"^dummy_tok_l^
    (String.concat ";\n" (String_map.fold aux token_map []))^"|]\n",*)
    
    let name_array = Array.make token_nb "-unknown token-" in
    String_map.iter
      (fun name id -> name_array.(id) <- name) token_name_map;
    let rec aux l i = if i = -1 then l else
      aux (("\""^name_array.(i)^"\"")::l) (i-1) in
    "  let token_name_array =\n  [|"^
    (String.concat ";\n    " (aux [] (token_nb-1)))^"|]\n",
    "  let str_token_name t = Dyp_symbols_array.token_name_array.(t)\n"
  in
  code_get_token_name, code_str_token, code_str_token_name,
  code_token_name_array



let (*code_ter_of_string,*) code_ter_string_list =
  let code = "  let ter_string_list = [" in
  let aux str n cl =
    ");"::(string_of_int n)::("\n      (\""^str^"\",")::cl in
  let code_list = String_map.fold aux token_name_map [code] in
  let code_list = List.rev ("]\n"::code_list) in
  (*"  let ter_of_string =
      List.fold_left (fun tsm (s,i) -> String_ter_map.add s i tsm)
           String_ter_map.empty ter_string_list\n",*)
  String.concat "" code_list



let map_card m =
  String_map.fold (fun _ _ i-> i+1) m 0



let nt_par_set =
  let aux1 ntpm ld = match ld with
    | (Symb_non_terminal ((nt,pos),_,_,inh)),_
    | (Symb_non_terminal_NL ((nt,pos),_,_,inh)),_ ->
        let b = inh <> dummy_code in
        (try
          let pos0, b0 = String_map.find nt ntpm in
          if b0 = b then ntpm else
          (let (l1,c11,c12,f1), (l2,c21,c22,f2) =
            if b then pos, pos0 else pos0, pos in
          fprintf stderr "Inconsistency with non terminal `%s'.\nIt has a Caml parameter in file \"%s\" line %d, characters %d-%d, and no parameter in file \"%s\" line %d, characters %d-%d.\n"
          nt f1 l1 c11 c12 f2 l2 c21 c22;
          exit 2)
        with Not_found ->
          String_map.add nt (pos, b) ntpm)
    | _ -> ntpm
  in
  let aux2 ntpm ((lhs_nt,_),_,ld_list,_,_) =
    List.fold_left aux1 ntpm ld_list
  in
  let nt_par_map =
    List.fold_left aux2 String_map.empty grammar
  in
  String_map.fold
    (fun nt (_,b) ntps -> if b then String_set.add nt ntps else ntps)
    nt_par_map String_set.empty



let grammar, nt_par_set =
  let eanb = ref 0 in
  let nt_par_l = ref [] in
  let aux newg ((lhs_nt, lhs_pat_l) as lhs, p, symb_list, action, ro) =
    match lhs_pat_l with [lhs_pattern] ->
    let rec make_inh_code l = function
      | 0 -> if String_set.mem lhs_nt nt_par_set then "_0" else ""
      | 1 ->
          let l =
            if String_set.mem lhs_nt nt_par_set
            then "_0, _1"::l
            else "_1"::l
          in
          "("^(String.concat ", " l)^")"
      | i when i>1 -> make_inh_code (("_"^(string_of_int i))::l) (i-1)
      | _ -> assert false
    in
    let make_ea (ar, prev_pat) = function
      | (Symb_early_action (code, (code_pos, b))), (pat, _, pat_pos) ->
          let nt = "dypgen__early_action_"^(string_of_int !eanb) in
          let pattern = pat, (Pat_syn nt), pat_pos in
          incr eanb;
          let rule =
            (nt, List.rev prev_pat), ("default_priority", dummy_p), [],
            (code, (code_pos, b)), (true, true)
          in
          rule::ar, pattern::prev_pat
      | _, pattern -> ar, pattern::prev_pat
    in
    let eanb_old = !eanb in
    let prev_pat =
      if String_set.mem lhs_nt nt_par_set
      then [lhs_pattern] else [dummy_pat_inh]
      (* dummy_pat_inh is useful as a place_holder for good
      numbering of action variables, it will be skipped. *)
    in
    let add_rules, _ =
      let symb_list = match symb_list with
        | _::tl -> tl
        | _ -> []
      in
      List.fold_left make_ea ([], prev_pat) symb_list
    in
    let r =
      let symb_list, _, _ = List.fold_left
        (fun (symb_list, eanb, i) -> function
          | (Symb_early_action _), (pat, _, pat_pos) ->
              let nt = "dypgen__early_action_"^(string_of_int eanb) in
              let pattern = pat, (Pat_syn nt), pat_pos in
              if i>0 || String_set.mem lhs_nt nt_par_set
              then nt_par_l := nt::!nt_par_l;
              let inh_code = (make_inh_code [] i), Lexing.dummy_pos in
              let symb = ((Symb_non_terminal ((nt,dummy_p),
                ("No_priority",dummy_p),Pr_eq,inh_code)), pattern)
              in
              symb::symb_list, eanb+1, i+1
          | s -> s::symb_list, eanb, i+1)
        ([], eanb_old, -1) symb_list
      in
      let symb_list = List.rev symb_list in
      (lhs, p, symb_list, action, ro)
    in
    r::(add_rules@newg)
    | _ -> assert false
  in
  let g = List.rev (List.fold_left aux [] grammar) in
  g,
  List.fold_left (fun nt_par_set nt -> String_set.add nt nt_par_set)
    nt_par_set !nt_par_l



(*let nt_par_set =
  let rec aux nt_par_set = function
    | 0 -> nt_par_set
    | i ->
        let nt = "dypgen__early_action_"^(string_of_int (i-1)) in
        aux (String_set.add nt nt_par_set) (i-1)*)



let non_terminal_map, non_terminal_set =
  let non_terminal_set =
    let aux1 st_set ld = match ld with
      | (Symb_non_terminal ((nt,pos),_,_,inh)),_
      | (Symb_non_terminal_NL ((nt,pos),_,_,inh)),_ ->
          String_set.add nt st_set
      | _ -> st_set
    in
    let aux2 (st_set1,st_set2) ((lhs_nt,_),_,ld_list,_,_) =
      (String_set.add lhs_nt st_set1),
      (List.fold_left aux1 st_set2 ld_list)
    in
    let nt_set_lhs, nt_set_rhs =
      List.fold_left aux2 (String_set.empty, String_set.empty) grammar
    in
    let foldfun entryp_set (ep,_) = String_set.add ep entryp_set in
    let entryp_set =
      List.fold_left foldfun String_set.empty non_terminal_start_list
    in
    let nt_not_in_lhs = String_set.union nt_set_rhs entryp_set in
    let nt_not_in_lhs = String_set.diff nt_not_in_lhs nt_set_lhs in
    let nt_not_in_rhs = String_set.diff nt_set_lhs nt_set_rhs in
    let nt_not_in_rhs = String_set.diff nt_not_in_rhs entryp_set in
    let f hs nt =
      (*print_endline ("File \""^input_file^"\":");*)
      if !Argument.werror then
        (fprintf stderr "Error: non terminal `%s' is never in a %s\n" nt hs;
        exit 2)
      else
        print_endline ("Warning: non terminal `"^nt^"' is never in a "^hs)
    in
    String_set.iter (f "left-hand side.") nt_not_in_lhs;
    String_set.iter (f "right-hand side.") nt_not_in_rhs;
    let nt_set =
      String_set.union entryp_set
      (String_set.union nt_set_lhs nt_set_rhs)
    in
    let foldfun2 nt_set nt = String_set.add nt nt_set in
    let nt_set = List.fold_left foldfun2 nt_set single_nt_list in
    let foldfun4 nt_set symb =
      let first = int_of_char symb.[0] in
      if first>64 && first<91 then nt_set
      else String_set.add symb nt_set
    in
    let foldfun3 nt_set (_,symb_list) =
      List.fold_left foldfun4 nt_set symb_list
    in
    List.fold_left foldfun3 nt_set cons_list
  in
  let aux nt_string (n,nt_map) = (
    (n+1),String_map.add nt_string n nt_map)
  in
  let _, non_terminal_map =
    String_set.fold aux non_terminal_set
      (1,String_map.empty)
  in
  non_terminal_map, non_terminal_set



let inh_cons_map =
  String_set.fold (fun nt icm -> String_map.add nt ("Inh_"^nt) icm)
  nt_par_set String_map.empty



module Ordered_str2 =
struct
  type t = string * string
  let compare = Pervasives.compare
end
module Str2_map = Map.Make(Ordered_str2)


let symb_cons_map, cons_entry_table, var_cons_map =
  
  let cm =
    let cm = String_set.fold
      (fun nt cm ->
        if nt.[0] = '0' then String_map.add nt "Obj_dypgen__epsilon" cm
        else String_map.add nt ("Obj_"^nt) cm )
      non_terminal_set String_map.empty in
    String_map.fold
      (fun t _ cm -> String_map.add t ("Obj_"^t) cm) token_map cm
  in
  
  let vcm =
    List.fold_left (fun vcm (identl, _) -> match identl with
      | name::args ->
          if Extract_type.match_Arg_ (Lexing.from_string name)
          then
            if !Argument.werror then
              (fprintf stderr
              "Error: the lexer name `%s' contains the string `_Arg_'\n" name;
              exit 2)
            else printf
              "Warning: the lexer name `%s' contains the string `_Arg_'\n" name;
          let vcm = Str2_map.add (name,"") ("Lex_"^name) vcm in
          List.fold_left (fun vcm arg ->
            Str2_map.add (name,arg) ("Lex_"^name^"_Arg_"^arg) vcm)
          vcm args
      | _ -> assert false)
    Str2_map.empty aux_lexer
  in
  
  let cm = List.fold_left
    (fun cm (cons, symb_list) ->
      List.fold_left (fun cm symb -> String_map.add symb cons cm)
      cm symb_list)
    cm cons_list in
  (*Printf.printf "cardinal symb_cons_map = %d\n" (map_card m);*)
  let card = List.length non_terminal_start_list in
  let ht = Hashtbl.create card in
  List.iter
    (fun (nt, _) ->
      let cons = String_map.find nt cm in
      let set =
        try Hashtbl.find ht cons
        with Not_found -> String_set.empty
      in
      let set = String_set.add nt set in
      Hashtbl.replace ht cons set)
    non_terminal_start_list;
  cm, ht, vcm



(*let _ =
  print_endline "symb_cons_map";
  String_map.iter (fun a b -> print_endline (a^" "^b)) symb_cons_map*)



let code_main_lexer =
  let l1 = List.map
    (fun (r,tname,_) ->
    "\n  (\""^tname^"\",("^(Dyp.print_regexp r)^"))") main_lexer
  in
  let l2 = List.map
    (*(fun (_,tname,(c,(lnum,offset))) ->*)
    (fun (_,tname,(c,pos)) ->
    let typ =
      try String_map.find tname token_map
      with Not_found -> assert false
    in
    let cons =
      try String_map.find tname symb_cons_map
      with Not_found -> assert false
    in
    let c =
      if typ = "No_type" then
        (let c = if c="" then c else
          "let _ = ("^
          (sharp_line_number pos.pos_fname pos.pos_lnum)^
          (space_string (pos.pos_cnum - pos.pos_bol))^
          c^insert_line_number^") in "
        in
        "(fun lexbuf -> "^c^obj_pref^cons^")")
      else if c="" then
        (Printf.fprintf stderr
          "Error: line %d, an action is expected for terminal %s\n"
           pos.pos_lnum tname; exit 2)
      else "(fun lexbuf -> "^obj_pref^cons^
        (sharp_line_number pos.pos_fname (pos.pos_lnum-1))^
        "(\n"^(space_string (pos.pos_cnum - pos.pos_bol))^
        "("^c^"):'dypgen__"^cons^")"^insert_line_number^")"
    in
    let ter_id =
      try String_map.find tname token_name_map
      with Not_found -> assert false
    in
    "\n  "^(string_of_int ter_id)^",("^c^")")
    main_lexer
  in
  let l1, l2 = match l1, l2 with
    | [], [] ->
      ["\"dummy_entry\",Dyp.RE_Eof_char"],
      ["0,(fun _ -> "^obj_pref^"Lexeme_matched \"\")"]
    | _ -> l1, l2
  in
  "(["^
  (String.concat ";" l1)^"],\n["^
  (*(if !Dypgen_parser.use_dyplex then
    " 0,(fun _ -> "^obj_pref^"Lexeme_matched \"\");"
  (* dummy line added to avoid a type error:
  Dyp.lexbuf, contains type variables that cannot be generalized *)
  else "")^*)
  (String.concat ";" l2)^"])"



let code_aux_lexer =
  let aux_lex_list = List.map
    (fun (aux_name_args,aux_def) ->
    let l1 = List.map
      (fun (re,_) ->
      "\n    ("^(Dyp.print_regexp re)^")"
      ) aux_def
    in
    match aux_name_args with [] -> assert false
    | aux_name::aux_args ->
    (*let argnb = List.length aux_args in*)
    let l2 = List.map
      (*(fun (_,(code,((lnum,offset),_))) ->*)
      (fun (_,(code,(pos,_))) ->
      let cons =
        try Str2_map.find (aux_name,"") var_cons_map
        with Not_found -> assert false
      in
      let code_var_list =
        let l = List.map (fun arg ->
          try
            let cons = Str2_map.find (aux_name,arg) var_cons_map in
           "("^obj_pref^cons^" ("^arg^":'dypgen__"^cons^"))"
          with Not_found -> assert false) aux_args
        in
        String.concat ";" l
      in
      "(fun __dypgen_av_list lexbuf -> (match __dypgen_av_list with ["^
      code_var_list^"] -> "^obj_pref^cons^
      (sharp_line_number pos.pos_fname (pos.pos_lnum-1))^
      "(\n"^(space_string (pos.pos_cnum - pos.pos_bol))^
      "("^code^"):'dypgen__"^cons^")"^insert_line_number^
      "  | _ -> failwith \"lexing: bad action variable list when calling lexer user action\"))"
      ) aux_def
    in
    "(\""^aux_name^"\",(["^
    (String.concat ";" l1)^"],["^
    (String.concat ";" l2)^"]))"
    ) aux_lexer
  in
  "["^
  (String.concat ";" aux_lex_list)^"]\n\n"



let code_regexp_decl =
  let l = List.map
    (fun (rname,r) ->
    "\n  (\""^rname^"\",("^(Dyp.print_regexp r)^"))")
    regexp_decl
  in
  "let __dypgen_regexp_decl = ["^
  (String.concat ";" l)^"]\n\n"



let code_aux_lexer_fun =
  let l = List.map (fun (aux_name_args,_) ->
    match aux_name_args with [] -> assert false
    | name::args ->
    let code_var_list, args_string =
      let l = List.map (fun arg ->
        try
          let cons = Str2_map.find (name,arg) var_cons_map in
          "("^obj_pref^cons^" ("^arg^":'dypgen__"^cons^"))",
          "("^arg^":'dypgen__"^cons^")"
        with Not_found -> assert false) args
      in
      let l1, l2 = List.fold_left
        (fun (k,l) (x,y) -> x::k,y::l) ([],[]) l in
      String.concat ";" l1,
      String.concat " " l2
    in
    let cons = Str2_map.find (name,"") var_cons_map in
    "let "^name^" "^args_string^" lexbuf =\n"^
    "  match Dyp.lex \""^name^"\" ["^code_var_list^"] lexbuf with\n"^
    "  | "^obj_pref^cons^
    " x -> (x:'dypgen__"^cons^")\n  | _ -> failwith \"lexer `"^name^
    "' returned a wrong obj constructor\"\nin\n"(*,
    "and __dypgen_dummy_fun "^args_string^" lexbuf =\n"^
    "  (("^name^" "^args_string^" lexbuf):'dypgen__"^cons^")\n\n"*))
    aux_lexer
  in
  (*let l1, l2 = List.fold_left
    (fun (k,l) (x,y) -> x::k,y::l) ([],[]) l in*)
  String.concat "" l
  (*String.concat "" l2*)



let code_merge_warning =
  if !Argument.merge_warning
  then "  let merge_warning = true\n"
  else "  let merge_warning = false\n"



let code_lexbuf_position =
  if !Dypgen_parser.use_dyplex then
  "let dypgen_lexbuf_position lexbuf = Dyp.dyplex_lexbuf_position lexbuf\n\n"
  else "let dypgen_lexbuf_position lexbuf =
  (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)\n\n"



let code_entry_points =
  let aux str (nts,_) =
    str^"    \""^nts^"\";\n"
  in
  let nts_list = List.fold_left aux "" non_terminal_start_list in
  let string_length = String.length nts_list in
  let nts_list =  if nts_list="" then ""
    else String.sub nts_list 0 (string_length-1) in
  "  let entry_points = [\n"^nts_list^"]\n"



let priority_set =
  let aux1 (str_set:String_set.t) rel = match rel with
    | Rel_list l -> List.fold_left (fun set p -> String_set.add p set) str_set l
    | Rel_single p1 -> String_set.add p1 str_set
  in
  let priority_set1 = List.fold_left aux1 String_set.empty relation in
  let aux1 st_set ld = match ld with
    | (Symb_non_terminal (_,(p,(line,col1,col2,fname)),_,_)),_
    | (Symb_non_terminal_NL (_,(p,(line,col1,col2,fname)),_,_)),_ ->
         if p="No_priority" then st_set else
         (if String_set.mem p priority_set1=false && p<>"default_priority"
         then
           (printf "File \"%s\", line %d, characters %d-%d:\n"
           fname line col1 col2;
           if !Argument.werror then
             (fprintf stderr "Error: the priority `%s' is not declared\n" p;
             exit 2)
           else
             printf "Warning: the priority `%s' is not declared\n" p);
        String_set.add p st_set)
    | _ -> st_set
  in
  let aux2 st_set (_,(p,(line,col1,col2,fname)),ld_list,_,_) =
    let st_set = String_set.add p st_set in
    if String_set.mem p priority_set1=false && p<>"default_priority" then
      (printf "File \"%s\", line %d, characters %d-%d:\n"
      fname line col1 col2;
      if !Argument.werror then
        (fprintf stderr "Error: the priority `%s' is not declared\n" p;
        exit 2)
      else
        printf "Warning: the priority `%s' is not declared\n" p);
    List.fold_left aux1 st_set ld_list
  in
  let priority_set = List.fold_left aux2 priority_set1 grammar in
  priority_set



let code_relations =
  let buf = Buffer.create 10000 in
  let _ = buf $ "  let relations = [\n" in
  let f2 s = let _ = buf $ "\"" $ s $ "\";" in () in
  let f1 l =
    let _ = buf $ "    [" in
    (List.iter f2 l);
    let _ = buf $ "];\n" in ()
  in
  let rel = List.filter (fun x -> match x with Rel_list _ -> true | _ -> false) relation in
  let rel = List.map (fun x -> match x with Rel_list y -> y | _ -> assert false) rel in
  (List.iter f1 rel);
  Buffer.contents (buf $ "  ]\n")



let code_global_local_data =
"let global_data = ()
let local_data = ()
let global_data_equal = (==)
let local_data_equal = (==)\n\n"



let cons_type_map =
  let aux1 symb typ map =
    if typ = "Unknown" then map else
    let cons =
      try String_map.find symb symb_cons_map
      with Not_found -> assert false
    in
    String_map.add cons typ map
  in
  let ct_map = String_map.fold aux1 token_map String_map.empty in
  let ct_map = String_map.fold aux1 nt_type_map ct_map in
  String_map.add "Lexeme_matched" "string" ct_map



let code_nt_cons_list, (*cons_of_nt,*) cons_map, cons_set, cons_array, code_test_cons =
  
  let aux1 _ cons s = String_set.add cons s in
  let s = String_map.fold aux1 symb_cons_map String_set.empty in
  
  let s = String_set.add "Lexeme_matched" s in
  
  let aux0 s cons = String_set.add cons s in
  let s = List.fold_left aux0 s add_cons_list in
  let s = Str2_map.fold (fun _ cons s -> String_set.add cons s)
    var_cons_map s in
  let s = String_map.fold (fun _ cons s -> String_set.add cons s)
    inh_cons_map s in
  
  let cons_array = Array.make (String_set.cardinal s) "" in
  let aux2 cons (codl,m,n) =
    let has_arg =
      try
        let typ = String_map.find cons cons_type_map in
        if typ = "No_type" then false else true
      with Not_found -> true
    in
    if has_arg then
      (cons_array.(n) <- cons;
      let code_fun =
        "  (fun x -> match x with "^obj_pref^cons^
        " _ -> true | _ -> false)" in
      (";\n"::code_fun::codl,
      String_map.add cons n m, n+1))
    else (codl,m,n)
  in
  let code_tc = "let __dypgen_test_cons () =  [|\n" in
  let code, cons_map, _ =
    String_set.fold aux2 s ([code_tc],String_map.empty,0)
  in
  
  let code = List.rev ("|]\n\n"::(List.tl code)) in
  let code_tc = String.concat "" code in
  
  (*let cons_of_nt =
    Array.make ((String_set.cardinal non_terminal_set)+1) 0
  in*)
  let aux3 symb cons (l,i) =
    let first = int_of_char symb.[0] in
    if (first>64 && first<91) || symb="__dypgen_layout" then
      (l,i)
    else
      let cons_i = String_map.find cons cons_map in
      (*cons_of_nt.(i) <- cons_i;*)
      (*(";\n"::(string_of_int cons_i)::"    "::l, (i+1))*)
      (";\n"::")"::(string_of_int cons_i)::"\","::symb::"    (\""::l, (i+1))
  in
  let code_ntcl = "  let nt_cons_list =\n  [\n" in
  let code,_ = String_map.fold aux3 symb_cons_map ([code_ntcl],1) in
  let code = List.rev ("]\n"::(List.tl code)) in
  let code_ntcl = String.concat "" code in
  code_ntcl, (*cons_of_nt,*) cons_map, s, cons_array, code_tc




let cons_no_type_set, code_str_cons =
  let aux2 cons _ set = String_set.add cons set in
  let typed_cons_set = String_map.fold aux2 cons_type_map String_set.empty in
  let cons_no_type_set = String_set.diff cons_set typed_cons_set in
  
  let buf = Buffer.create 10000 in
  let _ = buf $ "  let str_cons o = match o with\n" in
  let aux cons _ =
    let has_arg =
      try
        let typ = String_map.find cons cons_type_map in
        if typ = "No_type" then false else true
      with Not_found -> true
    in
    if has_arg then
      buf $ "    | " $ obj_pref $ cons $ " _ -> \"" $ cons $"\"\n"
    else buf
  in
  let _ = String_set.fold aux cons_set buf in
  let _ = buf $
    "    | _ -> failwith \"str_cons, unexpected constructor\"\n" in
  let code_str_cons = Buffer.contents buf in
  
  cons_no_type_set, code_str_cons




let code_cons_array =
  let buf = Buffer.create 10000 in
  Buffer.add_string buf "  let cons_array = [|\n";
  for i=0 to Array.length cons_array -1 do
    Buffer.add_string buf ("    \""^cons_array.(i)^"\";\n")
  done;
  Buffer.add_string buf "  |]\n";
  Buffer.contents buf




let make_type_var cons =
  try
    let pref = String.sub cons 4 14 in
    let symb = String.sub cons 19 (String.length cons - 19) in
    if not (String_set.mem symb non_terminal_set) then cons else
    let cons1 = String_map.find symb symb_cons_map in
    match pref with
      | "dypgen__star__"
      | "dypgen__plus__" -> cons1^" list"
      | "dypgen__option" -> cons1^" option"
      | _ -> cons
  with Invalid_argument _ | Not_found -> cons



let code_parser =
  
  let aux ((lhs_nt, lhs_pat_l), (prio,_), symb_list,
  ocaml_code, (allow_layout_inside, allow_layout_follows)) =
    let make_Ter nl ter line col1 col2 fname code =
      let _ = (try String_map.find ter token_map
        with Not_found -> (
          fprintf stderr "File \"%s\", line %d, characters %d-%d:\n"
          fname line col1 col2;
          fprintf stderr "Token `%s' not declared\n" ter; exit 2))
      in
      code^"Dyp.Ter"^nl^" \""^ter^"\";"
    in
    let code_prio p eq =
      if p = "No_priority" then "Dyp.No_priority "
      else (match eq with
        | Pr_eq -> "Dyp.Eq_priority "
        | Pr_lesseq -> "Dyp.Lesseq_priority "
        | Pr_less -> "Dyp.Less_priority "
        | Pr_greater -> "Dyp.Greater_priority "
        | Pr_greatereq -> "Dyp.Greatereq_priority ")^"\""^p^"\""
    in
    let aux2 code ld = match ld with
      | (Symb_regexp re), _ ->
          code^"Dyp.Regexp ("^(Dyp.print_regexp re)^");"
      | (Symb_regexp_NL re), _ ->
          code^"Dyp.Regexp_NL ("^(Dyp.print_regexp re)^");"
      | (Symb_terminal (ter,(line,col1,col2,fname))), _ ->
          make_Ter "" ter line col1 col2 fname code
      | (Symb_terminal_NL (ter,(line,col1,col2,fname))), _ ->
          make_Ter "_NL" ter line col1 col2 fname code
      | (Symb_non_terminal ((ntn,_),(p,_),eq,_)), _ ->
          code^"Dyp.Non_ter (\""^ntn^"\","^(code_prio p eq)^");"
      | (Symb_non_terminal_NL ((ntn,_),(p,_),eq,_)), _ ->
          code^"Dyp.Non_ter_NL (\""^ntn^"\","^(code_prio p eq)^");"
      | (Symb_early_action _), _ -> assert false
    in
    
    let code_literal_list = List.fold_left aux2 "" symb_list in
    
    let string_length = (String.length code_literal_list) in
    let code_literal_list =
      if string_length = 0 then code_literal_list
      else if code_literal_list="" then ""
        else String.sub code_literal_list 0 (string_length-1)
    in
    let rule_options =
      "["^(if allow_layout_inside then "" else "Dyp.No_layout_inside;")^
      (if allow_layout_follows then "" else "Dyp.No_layout_follows;")^"]"
    in
    let code_rule =
      "(\""^lhs_nt^"\",["^ code_literal_list^ "],\""^prio^
      "\","^rule_options^")" in
    
    let make_type_var_aux nt =
      (*Printf.printf "make_type_var nt=%s\n" nt;*)
      let cons = String_map.find nt symb_cons_map in
      (*Printf.printf "cons=%s\n" cons;*)
      make_type_var cons
    in
    
    let code_var_list, code_inherited_val =
      let f (code_vl, code_iv, n, n') = function
        | (Symb_regexp _), (pat, (Pat_syn pat_typ), pos)
        | (Symb_regexp_NL _), (pat, (Pat_syn pat_typ), pos) ->
            if pat = "_" then
              (" "^obj_pref^"Lexeme_matched _"^(string_of_int n))::code_vl,
              code_iv, n+1, n'+1
            else
            (" "^obj_pref^"Lexeme_matched ("^
            (sharp_line_number pos.pos_fname pos.pos_lnum)^
            (space_string (pos.pos_cnum - pos.pos_bol))^
            "("^pat^":string)"^
            insert_line_number^
            " as _"^(string_of_int n)^")")::code_vl,
            code_iv, n+1, n'+1
        | (Symb_terminal (ter,_)), (pat, (Pat_syn pat_typ), pos)
        | (Symb_terminal_NL (ter,_)), (pat, (Pat_syn pat_typ), pos) ->
            let typ = String_map.find ter token_map in
            if typ = "No_type" then
              (" _"^(string_of_int n))::code_vl, code_iv, n+1, n'+1
            else
              let typ =
                try
                  let typ = String_map.find pat_typ token_map in
                  if typ = "Unknown" then
                    Some ("'dypgen__"^(make_type_var_aux ter))
                  else Some typ
                with Not_found -> None
              in
              (obj_pref^(String_map.find ter symb_cons_map)^" "^
              " ("^(sharp_line_number pos.pos_fname pos.pos_lnum)^
              (space_string (pos.pos_cnum - pos.pos_bol))^
              (match typ with None -> pat | Some t -> "("^pat^":"^t^")")^
              insert_line_number^" as _"^(string_of_int n)^")")::code_vl,
              code_iv, n+1, n'+1
        | (Symb_non_terminal ((nt,_),_,_,(code, code_pos))),
            (pat, (Pat_syn pat_typ), pos)
        | (Symb_non_terminal_NL ((nt,_),_,_,(code, code_pos))),
            (pat, (Pat_syn pat_typ), pos)
          when nt.[0] <> '0' ->
            let code_var = (*aux "" n patternl in*)
              " ("^(sharp_line_number pos.pos_fname pos.pos_lnum)^
              (space_string (pos.pos_cnum - pos.pos_bol))^
              (try "("^pat^":'dypgen__"^
              (make_type_var_aux nt)^")"
              with Not_found -> pat)^
              insert_line_number^" as _"^(string_of_int n)^")"
            in
            let code_iv =
              if code = "" then code_iv else
                let typ = "'dypgen__"^(String_map.find nt inh_cons_map) in
                let code_var_list = String.concat ";" (List.rev code_vl) in
                (String.concat ""
                  [(string_of_int n')^",\n(fun __dypgen_ol __dypgen_pos";
                  " __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld";
                  " __dypgen_di __dypgen_p __dypgen_nl ->\n";
                  "(Dyp.Tools.transform_inh_val ";
                  "(fun dyp __dypgen_av_list -> (match (";
                  "__dypgen_av_list) with [";
                  code_var_list;"] -> ";obj_pref;
                  (String_map.find nt inh_cons_map);" ";
                  (sharp_line_number code_pos.pos_fname (code_pos.pos_lnum-1));
                  "(\n";(space_string (code_pos.pos_cnum - code_pos.pos_bol));
                  "(";code;"):";typ;")";
                  insert_line_number;
                  " | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos";
                  " __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld";
                  " __dypgen_di __dypgen_p __dypgen_nl)"])::code_iv
            in
            (obj_pref^(String_map.find nt symb_cons_map)^
            " ("^code_var^")")::code_vl, code_iv,
            (* The extra parentheses around code_var are necessary
              for non terminals generated by partial actions. *)
            n+1, n'+1
        | _ ->
            code_vl, code_iv, n, n'+1
      in
      let code_vl =
        if String_set.mem lhs_nt nt_par_set then
          let patl, _ = List.fold_left
            (fun (patl, i) ((lhs_pat, pat_typ, lhs_pat_pos) as pattern) ->
              if pattern = dummy_pat_inh then patl, i+1 else
              let typ =
                "("^lhs_pat^
                (match pat_typ with
                  | Pat_syn pt ->
                      if pt = "#Lexeme_matched" then ":string" else
                      ":"^(try String_map.find pt nt_type_map
                      with Not_found -> "'dypgen__"^(make_type_var_aux pt))
                  | Pat_inh pt -> ":'dypgen__"^
                      (try (String_map.find pt inh_cons_map)
                      with Not_found -> assert false)
                )^")"
              in
              (" ("^(sharp_line_number lhs_pat_pos.pos_fname
                 lhs_pat_pos.pos_lnum)^
              (space_string (lhs_pat_pos.pos_cnum - lhs_pat_pos.pos_bol))^
              typ^
              insert_line_number^" as _"^(string_of_int i)^")")::patl,
              i+1)
            ([], 0) lhs_pat_l
          in
          let pat = String.concat ",\n" (List.rev patl) in
          [obj_pref^(String_map.find lhs_nt inh_cons_map)^"("^pat^")"]
        else []
      in
      let code_vl, code_iv, _, _ =
        List.fold_left f (code_vl,[],1,1) symb_list in
      let code_vl = String.concat ";" (List.rev code_vl) in
      let code_iv = String.concat ";" (List.rev code_iv) in
      code_vl, code_iv
    in

    let code_action =
      let action, (pos, b) = ocaml_code in
      let typ =
        try
          String_map.find lhs_nt nt_type_map
        with Not_found -> "'dypgen__"^
          (make_type_var_aux lhs_nt)
      in
      String.concat ""
        (if b then
          ["Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl";
          " __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p";
          " __dypgen_nl ->\n";
          "(Dyp.Tools.transform_action ";
          "(fun dyp __dypgen_av_list -> (match (";
          "__dypgen_av_list) with [";
          code_var_list;"] -> ";
          " let res = ";
          (sharp_line_number pos.pos_fname (pos.pos_lnum-1));
          "(\n";(space_string (pos.pos_cnum - pos.pos_bol));
          "(";action;"):";typ;" * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)";
        (* The extra parentheses around action are useful when the action
        is empty, it converts it to unit. *)
          insert_line_number;
          "  in ";
          obj_pref;(String_map.find lhs_nt symb_cons_map);
          "(fst res), snd res\n";
          " | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl";
          " __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p";
          " __dypgen_nl)"]
        else
          let action =
            if action = "###" then
              if code_var_list = " _1" then "None"
              else "Some _1"
            else action
          in
          ["Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl";
          " __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p";
          " __dypgen_nl ->\n";
          "(Dyp.Tools.transform_action ";
          "(fun dyp __dypgen_av_list -> (match (";
          "__dypgen_av_list) with [";
          code_var_list;"] -> ";obj_pref;
          (String_map.find lhs_nt symb_cons_map);" ";
          (sharp_line_number pos.pos_fname (pos.pos_lnum-1));
          "(\n";(space_string (pos.pos_cnum - pos.pos_bol));
          "(";action;"):";typ;")";
        (* The extra parentheses around action are useful when the action
        is empty, it converts it to unit. *)
          insert_line_number;
          ",[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos";
          " __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di";
          " __dypgen_p __dypgen_nl)"]
        )
    in
    
    String.concat ""
      ["(";code_rule;",\n";code_action;",\n[";code_inherited_val;"])"]
  in
  let list_code_rapf = List.map aux grammar in
  let rec aux res sl = match sl with
    | [s] -> "[\n"::s::res
    | s::t -> aux ("\n;\n"::s::res) t
    | [] -> failwith "empty grammar"
  in
  let list_code_rapf = aux ["]"] list_code_rapf in
  String.concat "" list_code_rapf



let code_parser_lexer =
  String.concat ",\n\n"
  ["let __dypgen_ra_list, __dypgen_main_lexer, __dypgen_aux_lexer =\n"^
  code_aux_lexer_fun^
  code_parser; code_main_lexer; code_aux_lexer(*^code_aux_lexer_dummy_fun*)]



(*let parse_function_code =
"let __dypgen_dummy_marker_1 = ()
let parse parser entry
  ?(global_data=global_data) ?(local_data=local_data)
  ?(lexpos=dypgen_lexbuf_position)
  lexfun lexbuf =
  Dyp.parse parser entry ~global_data:global_data ~local_data:local_data ~lexpos:lexpos lexfun lexbuf\n\n"*)

let code_main_2 =
  let aux =
    if !Dypgen_parser.use_dyplex then
      (fun str (nts,_) -> try
  str^"let "^nts^
  " ?(global_data=global_data) ?(local_data=local_data) lexbuf =
  let pf = Dyp.lexparse (pp ()) \""^nts^"\" ~global_data:global_data
    ~local_data:local_data ~match_len:dypgen_match_length
    ~keep_data:dypgen_keep_data
    ~use_rule_order:dypgen_use_rule_order
    ~use_all_actions:dypgen_use_all_actions
    ~choose_token:dypgen_choose_token lexbuf in
  let aux1 (o,p) = match o with
    | "^obj_pref^(String_map.find nts symb_cons_map)^" r -> (r,p)
    | _ -> failwith \"Wrong type for entry result\" in
  List.map aux1 pf\n\n" with Not_found -> failwith "code_main_2")
    else (fun str (nts,_) -> try
  str^"let "^nts^
  " ?(global_data=global_data) ?(local_data=local_data) f lexbuf =
  let pf = Dyp.parse (pp ()) \""^nts^"\" ~global_data:global_data
    ~local_data:local_data ~match_len:dypgen_match_length
    ~keep_data:dypgen_keep_data
    ~use_rule_order:dypgen_use_rule_order
    ~use_all_actions:dypgen_use_all_actions
    ~lexpos:dypgen_lexbuf_position f lexbuf in
  let aux1 (o,p) = match o with
    | "^obj_pref^(String_map.find nts symb_cons_map)^" r -> (r,p)
    | _ -> failwith \"Wrong type for entry result\" in
  List.map aux1 pf\n\n" with Not_found -> failwith "code_main_2")
  in
"let __dypgen_dummy_marker_2 = ()
let pp () = Dyp.make_parser
  __dypgen_ra_list Dyp_priority_data.relations global_data local_data
  (Dyp.Tools.make_nt_cons_map Dyp_symbols_array.nt_cons_list)
  Dyp_symbols_array.entry_points
  
  "^(string_of_bool !Argument.merge_warning)^" "^
  (string_of_int token_nb)^" "^
  (string_of_bool !Argument.undef_nt)^"
  
  Dyp_aux_functions.get_token_value
  Dyp_symbols.get_token_name Dyp_symbols.str_token
  global_data_equal local_data_equal (__dypgen_test_cons ())
  Dyp_symbols_array.str_cons
  Dyp_symbols_array.cons_array Dyp_aux_functions.cons_table
  (Dyp.Tools.array_of_list __dypgen_merge_list)
  dypgen_lexbuf_position __dypgen_regexp_decl __dypgen_main_lexer
  __dypgen_aux_lexer Dyp_symbols.ter_string_list
  (fun lexbuf -> "^obj_pref^"Lexeme_matched (Dyp.lexeme lexbuf))
  "^(string_of_bool !Dypgen_parser.use_dyplex)^(*"
  "^(string_of_bool !Argument.use_all_actions)^*)"\n

let __dypgen_dummy_marker_5 = ()\n\n"^
  "let __dypgen_dummy_marker_3 = ()\n\n"^
  (List.fold_left aux "" non_terminal_start_list)^
  "\nlet __dypgen_dummy_marker_4 = ()\n\n"






let code_type_obj, cons_no_type_array =
  let cons_no_type_array =
    Array.make (String_set.cardinal cons_no_type_set) ""
  in
  let code_obj =
    let use_type_var cons =
      try
        let pref = String.sub cons 4 14 in
        match pref with
          | "dypgen__star__"
          | "dypgen__plus__"
          | "dypgen__option" ->
            let symb = String.sub cons 19 (String.length cons - 19) in
            not (String_set.mem symb non_terminal_set)
          | _ -> true
      with Invalid_argument _ -> true
    in
    if String_set.cardinal cons_no_type_set = 0 then
      "type obj ="
    else
      let aux cons (codl,n) =
        cons_no_type_array.(n) <- cons;
        (*try
          let _ = String_map.find cons cons_type_map in
          codl, n+1
        with Not_found ->*)
          if use_type_var cons then
            ", "::("'dypgen__"^cons)::codl, n+1
          else codl, n+1
      in
      let codl, _ =
        String_set.fold aux cons_no_type_set (["type ("],0)
      in
      let codl = List.rev (") obj ="::(List.tl codl)) in
      String.concat "" codl
  in
  
  let aux cons codl =
    let typ =
      try String_map.find cons cons_type_map
      with Not_found -> "'dypgen__"^(make_type_var cons)
    in
    let typ =
      if typ = "No_type" then ""
      else " of "^typ
    in
    ("  | "^obj_pref^cons^typ^"\n")::codl
  in
  let codl = String_set.fold aux cons_set [] in
  let codl =
    if !Dypgen_parser.use_dyplex then
      ("  | "^obj_pref^"Dypgen__dummy_obj_cons\n")::codl
    else codl
  in
  let code = String.concat "" (List.rev codl) in
  
  code_obj^
  (if !Argument.pv_obj then " [\n" else "\n")^
  code^
  (if !Argument.pv_obj then " ]\n\n" else "\n"),
  cons_no_type_array



let code_merge_functions =
  let aux cons_s _ code = code^"let dyp_merge_"^cons_s^
  " = Dyp.Tools.keep_zero\n" in
  (String_map.fold aux cons_map "")^
  (*"let keep_all ol o = o::ol\n"^
  "let Dyp.keep_oldest ol _ =
  let rec aux l = match l with [] -> [] | [c] -> [c] | _::t -> aux t in
  aux ol\n"^
  "let keep_newest _ o = [o]\n"^*)
  "let dyp_merge = Dyp.keep_one\n"

let code_merge_array =
  let aux4 gmf gmm nt = String_map.add nt gmf gmm in
  let aux3 gmm (gmf,nt_l) = List.fold_left (aux4 gmf) gmm nt_l in
  let gen_merge_map = List.fold_left aux3 String_map.empty generic_merge in
  let aux1 cons_s _ code = code^"let dyp_merge_"^cons_s^
    " l =\n"^
    try let gen_merge = String_map.find cons_s gen_merge_map in
      "  "^gen_merge^" l\n"
    with Not_found -> (
      "  match dyp_merge_"^cons_s^" l with\n"^
      "    | ([],_,_) -> dyp_merge l\n"^
      "    | res -> res\n")
  in
  let aux2 cons_s _ ma =
    let has_arg =
      try
        let typ = String_map.find cons_s cons_type_map in
        if typ = "No_type" then false else true
      with Not_found -> true
    in
    if has_arg then
  ";\n"::("(fun l -> (
  let f1 (o,gd,ld) = match o with "^obj_pref^cons_s^" ob -> (ob,gd,ld)
    | _ -> failwith \"type error, bad obj in dyp_merge_"^cons_s^"\"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_"^cons_s^" l in
  let f2 o = "^obj_pref^cons_s^" o in
  (List.map f2 ol, gd, ld)))")::ma
    else ma
  in
  let merge_array_l = List.rev
    ("]\n\n"::(List.tl (String_map.fold aux2 cons_map
     ["let __dypgen_merge_list = ["])))
  in
  let code_merge_array = String.concat "" merge_array_l in
  
  (String_map.fold aux1 cons_map "")^"\n"^
  code_merge_array^"\n\n"



let code_get_token_value =
  if !Dypgen_parser.use_dyplex then
    "  let get_token_value _ = "^obj_pref^"Dypgen__dummy_obj_cons\n"
  else
  let aux tok typ code =
    let cons = String_map.find tok symb_cons_map in
    let s = if typ = "No_type" then " -> "^obj_pref^cons^"\n"
      else " x -> "^obj_pref^cons^" x\n"
    in
    code^"    | "^token_pref^tok^s
  in
  "  let get_token_value t = match t with\n"^
  (String_map.fold aux token_map "")





let parser_codl = [
  topheader_main;
  test_version_match;
  code_type_token;

  "module Dyp_symbols =\nstruct\n";
  (*code_non_terminal_decl;*)
  (*code_token_name_decl;*)
  code_get_token_name;
  code_str_token;
  code_ter_string_list;
  "end\n\n";

  (if !Argument.pv_obj then "" else code_type_obj);

  "module Dyp_symbols_array =\nstruct\n";
  (*code_str_non_ter;*)
  code_token_name_array;
  code_nt_cons_list;
  code_str_cons;
  code_cons_array;
  code_entry_points;
  "end\n\n";

  (*"module Dyp_parameters =\nstruct\n";
  code_token_nb;
  code_undef_nt;
  code_str_token_name;
  (*code_priority_names;*)
  code_merge_warning;
  "end\n\n";*)

  (*code_main_1;*)

  code_lexbuf_position;
  "module Dyp_aux_functions =\nstruct\n";
  (*code_datadyn;*)
  code_get_token_value;
  (*code_transform_av_list;*)
  "  let cons_table = Dyp.Tools.hashtbl_of_array Dyp_symbols_array.cons_array\n";
  "end\n\n";

  "module Dyp_priority_data =\nstruct\n";
  (*code_priority_def;*)
  (*code_prio_data;*)
  code_relations;
  "end\n\n";

  code_global_local_data;
  code_merge_functions;
  "let dypgen_match_length = `shortest\n";
  "let dypgen_choose_token = `first\n";
  "let dypgen_keep_data = `both\n";
  "let dypgen_use_rule_order = "^
  (string_of_bool !Argument.use_rule_order)^"\n";
  "let dypgen_use_all_actions = "^
  (string_of_bool !Argument.use_all_actions)^"\n";

  header_main;

  (*parse_function_code;*)
  (*code_aux_lexer_fun;*)
  code_parser_lexer;
  code_regexp_decl;
  code_merge_array;
  code_test_cons;
  (* if test_cons is defined before the header then it may
  cause an error like:
    This expression has type (('a, 'b, 'c) obj -> bool) array
    but is here used with type (('a, 'b, type_N) obj -> bool) array
    The type constructor type_N would escape its scope *)
  code_main_2;
  trailer_main]

let parser_code = String.concat "" parser_codl



let () = Insert_linenum.buffer := String.copy parser_code
let lexbuf = Lexing.from_string parser_code
let parser_code = Insert_linenum.insert_linenum lexbuf

let dest_file = open_out temp_output_file
let () = output_string dest_file parser_code
let () = close_out dest_file



let () = if !Argument.no_mli then () else
  (let ec =
    if !Argument.command = "" then
      Sys.command
        ("ocamlc "^(!Argument.ocamlc_options)^" -i -impl "^temp_output_file^
        " > "^extract_type)
    else
      Sys.command !Argument.command
        (*((!Argument.command)^" -i "^output_file^" > "^extract_type)*)
  in
  if ec = 2 then exit 2;
  let error_regexp filename =
    let (a,b,c,d,e,f) = "@","onzon","e",".","gmail","com" in
    let ema = c^d^b^a^e^d^f in
    fprintf stderr "Error while extracting strings from %s\nPlease send me the file %s at %s to fix this bug\n" filename filename ema
  in
  let parser_code_mli =
    let token_type, obj_type, parser_type, entry_fun_type_map =
      let extract_obj_file = open_in extract_type in
      let lexbuf = Lexing.from_channel extract_obj_file in
      let token_type, obj_type, parser_type, entry_fun_type_map =
        try
          let emit_pp = !Argument.emit_pp in
          let a =
            if !Argument.pv_token || not !Argument.emit_token_type
              || !Dypgen_parser.use_dyplex
            then "" else Extract_type.token_type lexbuf in
          let b =
            if !Argument.pv_obj ||
              not (emit_pp || !Argument.emit_obj_type) then ""
            else Extract_type.obj_type lexbuf in
          let c = if emit_pp then Extract_type.parser_type lexbuf
            else "" in
          let d = Extract_type.fun_type String_map.empty lexbuf in
          a,b,c,d
        with Failure _ -> error_regexp extract_type; exit 2
      in
      close_in extract_obj_file;
      token_type, obj_type, parser_type, entry_fun_type_map
    in
    let aux str (nts,_) =
      let fun_type =
        try String_map.find nts entry_fun_type_map
        with Not_found -> assert false
      in
      let lexbuf2 = Lexing.from_string fun_type in
      let slist = List.rev (remove_tpar [] lexbuf2) in
      let fun_type = String.concat "" slist in
      str^"val "^nts^" :"^fun_type^"\n"
    in
    topmli_code^
    token_type^
    obj_type^
    midmli_code^
    parser_type^
    (*("val pp :"^parser_type^"\n\n")^*)
    (List.fold_left aux "" non_terminal_start_list)^
    mli_code
  in
  Insert_linenum.buffer := String.copy parser_code_mli;
  let lexbuf = Lexing.from_string parser_code_mli in
  let parser_code_mli = Insert_linenum.insert_linenum lexbuf in
  let dest_file_mli = open_out output_file_mli in
  output_string dest_file_mli parser_code_mli;
  close_out dest_file_mli;
  let lexbuf = Lexing.from_string parser_code in
  (try Insert_linenum.replace_filename parser_code input_file_short lexbuf
  with Failure _ -> (error_regexp (input_file_short^".ml.temp"); exit 2));
  let dest_file = open_out output_file in
  output_string dest_file parser_code;
  close_out dest_file)
