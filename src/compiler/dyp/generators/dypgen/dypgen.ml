open Parse_tree
open Lexing
open Printf
open Dypgen_lexer


let input_file = !(Argument.string_ref)
let input_file_short = Filename.chop_extension input_file
let output_file = input_file_short^".ml"
let output_file_mli = input_file_short^".mli"


let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)

let parse_result =
  try Dypgen_parser.main Dypgen_lexer.token lexbuf
  with Failure _ -> (
    let b = ref true in
    let () = match !start_dypgen_comment with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nDypgen comment not terminated\n"
        input_file line col
    in
    if !start_ocaml_type<>dummy_pos then (
      let line = !start_ocaml_type.pos_lnum in
      let col = !start_ocaml_type.pos_cnum - !start_ocaml_type.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nOcaml type statement not terminated\n"
        input_file line col);
    if !start_pattern<>dummy_pos then (
      let line = !start_pattern.pos_lnum in
      let col = !start_pattern.pos_cnum - !start_pattern.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nOcaml pattern not terminated\n"
        input_file line col);
    if !start_ocaml_code<>dummy_pos then (
      let line = !start_ocaml_code.pos_lnum in
      let col = !start_ocaml_code.pos_cnum - !start_ocaml_code.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nOcaml code not terminated\n"
        input_file line col);
    if !start_string<>dummy_pos then (
      let line = !start_string.pos_lnum in
      let col = !start_string.pos_cnum - !start_string.pos_bol in
      let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nString not terminated\n"
        input_file line col);
    let () = match !start_bracket with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nBracket not closed\n"
        input_file line col
    in
    let () = match !start_curlyb with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nCurly brace not closed\n"
        input_file line col
    in
    let () = match !start_ocaml_comment with
      | [] -> ()
      | pos::_ ->
          let line = pos.pos_lnum in
          let col = pos.pos_cnum - pos.pos_bol in
          let () = b:= false in
      fprintf stderr "File \"%s\", line %d, character %d:\nDypgen comment not terminated\n"
        input_file line col
    in
    if !b then (
      let line2 = lexbuf.lex_curr_p.pos_lnum in
      let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
      let pos1 = lexeme_start_p lexbuf in
      let line1 = pos1.pos_lnum in
      let col1 = pos1.pos_cnum - pos1.pos_bol in
      if line1=line2 then
        fprintf stderr "File \"%s\", line %d, characters %d-%d:\nLexing failed\n"
          input_file line2 col1 col2
      else
        fprintf stderr "File \"%s\", from l:%d, c:%d to l:%d, c:%d :\nLexing failed\n"
          input_file line1 col1 line2 col2);
    exit 2)
  | Dyp.Syntax_error -> (
      let line2 = lexbuf.lex_curr_p.pos_lnum in
      let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
      let pos1 = lexeme_start_p lexbuf in
      let line1 = pos1.pos_lnum in
      let col1 = pos1.pos_cnum - pos1.pos_bol in
      if line1=line2 then
        fprintf stderr "File \"%s\", line %d, characters %d-%d\nSyntax error\n"
          input_file line2 col1 col2
      else
        fprintf stderr "File \"%s\", from l:%d, c:%d to l:%d,c:%d\nSyntax error\n"
          input_file line1 col1 line2 col2;
    exit 2)



let
  (topheader_main,(topheader_main_pos,topheader_main_offset)),
  (header_main,(header_main_pos,header_main_offset)),
  token_list,
  relation,
  non_terminal_start_list,
  generic_merge,
  cons_list,
  nt_type_list,
  single_nt_list,
  add_cons_list,
  global_data_type,
  local_data_type,
  grammar,
  (trailer_main,(trailer_main_pos,trailer_main_offset)),
  (topmli_code,(topmli_code_pos,topmli_code_offset)) ,
  (mli_code,(mli_code_pos,mli_code_offset))
  =
    let mltop,c1,ppi,g,c2,mlitop,c3 = fst (List.hd parse_result) in
    (mltop,c1,ppi.token_list,ppi.relation,ppi.start,
    ppi.generic_merge,ppi.cons,ppi.nt_type,ppi.single_nt,
    ppi.additional_cons,ppi.gd_type,ppi.ld_type,
    g,c2,mlitop,c3)


let obj_pref = if !Argument.pv_obj then "`" else ""
let token_pref = if !Argument.pv_token then "`" else ""



let global_data_type = if global_data_type="" then "unit"
  else "("^global_data_type^")"
let local_data_type = if local_data_type="" then "unit"
  else "("^local_data_type^")"

let append_string_to_buffer buf str =
  Buffer.add_string buf str; buf

let ($) = append_string_to_buffer



(* string ["a";"b";"c"] returns "[a;b;c]" *)
let string_list sl =
  let aux code s = code^s^";" in
  let code = List.fold_left aux "[" sl in
  let string_length = String.length code in
  (if code="" then "" else String.sub code 0 (string_length-1))^"]"



let grammar =

  let n_first l n =
    let rec aux l1 l n =
      if n=0 then (List.rev l1,l) else match l with
        | h::t ->
            aux (h::l1) t (n-1)
        | [] -> failwith "grammar n_first"
    in
    aux [] l n
  in
  (*let rec n_first l n =
    if n=0 then [],l else match l with
      | h::t ->
          let l1,l2 = n_first t (n-1) in h::l1,l2
      | [] -> failwith "grammar n_first"
  in*)
  let return_n n =
    let rec aux res n =
      if n=0 then res
      else aux ("_"^(string_of_int n)^","^res) (n-1)
    in
    aux "" n
  in

  let aux (new_gr,newnt_nb) ra =

    let (lhs_nt,prio,ld_list,(par_act_l:((action_desc*int)*pattern_desc) list),
      (ocaml_code,ocaml_code_pos)) = ra in
    if par_act_l = [] then
    ((lhs_nt,prio,ld_list,(par_act_l:((action_desc*int)*pattern_desc) list),
      ((ocaml_code,("","")),ocaml_code_pos))::new_gr,newnt_nb)
    else

    let rec f new_gr newnt_nb litl last_pos res_nb
              (par_act_l:((action_desc*int)*pattern_desc) list) patternl =
      match par_act_l with
      | [] -> (new_gr,newnt_nb,litl,res_nb,patternl)
      | (((ac_code,i),pos),patl)::tl ->
          let arg_nb = pos-last_pos in
          (*Printf.printf "pos:%d, last_pos:%d, litl len:%d\n" pos last_pos (List.length litl);*)
          let new_nt = if last_pos = 0 then [] else
            [(Obj_non_terminal ("dypgen__nt_"^(string_of_int (newnt_nb-1)),
            ("No_priority",(-1,-1,-1)),Pr_eq,res_nb)),patternl]
          in
          let litl1,litl2 = n_first litl arg_nb in
          let new_litl = new_nt@litl1 in
          let patternl = List.map (fun (_,x) -> x) new_litl in
          let patternl = List.flatten patternl in
          let patternl = patternl@patl in
          let ac_code = ac_code,(" ("^(return_n (res_nb+arg_nb)),")") in
          let new_gr =
            ("dypgen__nt_"^(string_of_int newnt_nb),("default_priority",(-1,-1,-1)),
            new_litl,[],(ac_code,i))::new_gr
          in
          f new_gr (newnt_nb+1) litl2 pos (res_nb+arg_nb+1) tl patternl
    in

    let new_gr,newnt_nb,litl,res_nb,patternl =
      f new_gr newnt_nb ld_list 0 0 (List.rev par_act_l) []
    in
    let new_nt =
      ((Obj_non_terminal ("dypgen__nt_"^(string_of_int (newnt_nb-1)),
      ("No_priority",(-1,-1,-1)),Pr_eq,res_nb)),patternl)
    in
    let new_litl = new_nt::litl in
    let new_gr =
      (lhs_nt,prio,new_litl,[],((ocaml_code,("","")),ocaml_code_pos))::new_gr
    in
    (new_gr,newnt_nb)
  in
  let g,_ = List.fold_left aux ([],0) grammar in g


let insert_line_number = "\n# insert-line-number \""^output_file^"\"\n"
let insert_line_number_mli = "\n# insert-line-number \""^output_file_mli^"\"\n"
let sharp_line_number lnum = "\n# "^(string_of_int lnum)^" \""^input_file^"\"\n"

let space_string n = String.make (max n 0) ' '


let topheader_main = if topheader_main="" then "\n" else
  (sharp_line_number topheader_main_pos)^(space_string topheader_main_offset)^
  topheader_main^insert_line_number
let header_main = if header_main="" then "\n" else
  (sharp_line_number header_main_pos)^(space_string header_main_offset)^
  header_main^insert_line_number
let trailer_main = if trailer_main = "" then ""
  else(sharp_line_number trailer_main_pos)^(space_string trailer_main_offset)^
  trailer_main^insert_line_number
let topmli_code = if topmli_code = "" then "\n" else
  (sharp_line_number topmli_code_pos)^(space_string topmli_code_offset)^
  topmli_code^insert_line_number_mli
let mli_code = if mli_code = "" then "\n" else
  (sharp_line_number mli_code_pos)^(space_string mli_code_offset)^
  mli_code^insert_line_number_mli


module Ordered_string =
struct
  type t = string
  let compare = Pervasives.compare
end

module String_set = Set.Make(Ordered_string)
module String_map = Map.Make(Ordered_string)



let nt_type_map =
  let f2 typ nt_type_map nt =
    String_map.add nt typ nt_type_map
  in
  let f1 nt_type_map (typ,nt_list) =
    List.fold_left (f2 typ) nt_type_map nt_list
  in
  let nt_type_map = List.fold_left f1 String_map.empty nt_type_list in
  let f3 nt_type_map (nt,typ) = f2 typ nt_type_map nt in
  List.fold_left f3 nt_type_map non_terminal_start_list



let code_undef_nt =
  if !Argument.undef_nt then "  let undef_nt = true\n"
  else "  let undef_nt = false\n"



let code_type_token, (*code_export_module,*) token_map =
  let lbra,rbra = if !Argument.pv_token then " [","]" else "","" in
  let code_type_token = "type token ="^lbra^"\n" in
  let aux (code_type_token,token_map) (tok,typ) =
    if typ = "No_type" then
      (code_type_token^"  | "^token_pref^tok^"\n"),
      (String_map.add tok typ token_map)
    else
      (code_type_token^"  | "^token_pref^tok^" of ("^typ^")\n"),
      (String_map.add tok typ token_map)
  in
  let code_type_token, token_map =
    List.fold_left aux (code_type_token,String_map.empty) token_list
  in
  let code_type_token = code_type_token^rbra^"\n"
  in
  (if !Argument.emit_token_type then code_type_token else ""),
  (*"module Export_type =\nstruct\n"^
  code_type_token ^"end\ninclude Export_type\n\n",*)
  token_map



let code_token_name_decl,token_name_map,code_token_nb, token_nb =
  (*let code_token_name_decl = "type token_name = int\n" in*)
  let token_name_map = String_map.empty in
  (*let token_name_map = String_map.add "dummy" 0 String_map.empty in
  let token_name_map = String_map.add "epsilon" 1 token_name_map in*)
  let aux (code,n,token_name_map) (tok,_) =
    (code^"  let t_"^tok^" = "^(string_of_int n)^"\n",
    (n+1),String_map.add tok n token_name_map)
  in
  let code_token_name_decl,n,token_name_map =
    List.fold_left aux
      ("",1+(List.length non_terminal_start_list),token_name_map)
      token_list
  in
  code_token_name_decl, token_name_map,
  "  let token_nb = "^(string_of_int n)^"\n", n



let code_get_token_name, code_str_token, code_str_token_name, code_token_name_array =
  let code_get_token_name =
    "  let get_token_name t = match t with\n"
  in
  let aux code (tok,typ) =
    if typ = "No_type" then code^"    | "^token_pref^tok^" -> t_"^tok^"\n"
    else code^"    | "^token_pref^tok^" _ -> t_"^tok^"\n"
  in
  let code_get_token_name =
    List.fold_left aux code_get_token_name token_list
  in
  let code_str_token =
    "  let str_token t = match t with\n"
  in
  let aux code (tok,typ) =
    if typ = "No_type" then code^"    | "^token_pref^tok^" -> \""^tok^"\"\n"
    else if typ = "int"
    then code^"    | "^token_pref^tok^" i -> \""^tok^"(\"^(string_of_int i)^\")\"\n"
    else if typ = "string" then code^"    | "^token_pref^tok^" s -> \""^tok^"(\"^s^\")\"\n"
    else code^"    | "^token_pref^tok^" _ -> \""^tok^"\"\n"
  in
  let code_str_token =
    (List.fold_left aux code_str_token token_list)
  in
  let code_token_name_array, code_str_token_name =
    let rec aux res token_list = match token_list with
      | (tok,_)::t::l -> aux (res^"    \""^tok^"\";\n") (t::l)
      | [(tok,_)] -> res^"    \""^tok^"\""
      | _ -> assert false
    in
    let aux_dummy_tok str (nts,_) =
      (str^"    \"dummy_token_"^nts^"\";\n")
    in
    let dummy_tok_l =
      List.fold_left aux_dummy_tok "" non_terminal_start_list
    in
    "  let token_name_array =\n  [|\"token_epsilon\";\n"^dummy_tok_l^
    (aux "" token_list)^"|]\n",
    "  let str_token_name t = Dyp_symbols_array.token_name_array.(t)\n"
  in
  code_get_token_name, code_str_token, code_str_token_name,
  code_token_name_array



let code_ter_of_string, code_ter_string_list, code_String_ter_map =
  let code = "  let ter_string_list = [" in
  let aux str n cl = ");"::(string_of_int n)::("\n      (\""^str^"\",")::cl in
  let code_list = String_map.fold aux token_name_map [code] in
  let code_list = List.rev ("]\n"::code_list) in
  "  let ter_of_string =\n    List.fold_left (fun tsm (s,i) -> String_ter_map.add s i tsm)\n     String_ter_map.empty ter_string_list\n",
  String.concat "" code_list,
  "  module Ordered_string =
  struct
    type t = string
    let compare = Pervasives.compare
  end
  module String_ter_map = Map.Make(Ordered_string)\n"



let map_card m =
  String_map.fold (fun _ _ i-> i+1) m 0



let code_non_terminal_decl, non_terminal_map, non_terminal_set =
  let code_non_terminal_decl = "" in
  let non_terminal_set =
    let aux1 st_set ld = match ld with
      | (Obj_terminal _),_ -> st_set
      | (Obj_non_terminal (nt,_,_,_)),_ -> String_set.add nt st_set
    in
    let aux2 (st_set1,st_set2) (lhs_nt,_,ld_list,_,_) =
      (String_set.add lhs_nt st_set1),(List.fold_left aux1 st_set2 ld_list)
    in
    let nt_set_lhs,nt_set_rhs =
      List.fold_left aux2 (String_set.empty,String_set.empty) grammar
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
      print_endline ("File \""^input_file^"\":");
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
  let aux nt_string (code1,n,nt_map) = (
    code1^"  let "^nt_string^" = "^(string_of_int n)^"\n",
    (n+1),String_map.add nt_string n nt_map)
  in
  let code_non_terminal_decl, non_terminal_number,
    non_terminal_map =
      String_set.fold aux non_terminal_set
      (code_non_terminal_decl,1,String_map.empty)
  in
  (*Printf.printf "cardinal non_terminal_set = %d\n"
    (String_set.cardinal non_terminal_set);
  Printf.printf "cardinal non_terminal_map = %d\n"
    (map_card non_terminal_map);*)
  code_non_terminal_decl,non_terminal_map,non_terminal_set



let symb_cons_map =

  let aux nt cm = String_map.add nt ("Obj_"^nt) cm in
  let cm = String_set.fold aux non_terminal_set String_map.empty in
  let aux t _ cm = String_map.add t ("Obj_"^t) cm in
  let cm = String_map.fold aux token_map cm in

  let aux2 cons cm symb = String_map.add symb cons cm in
  let aux cm (cons,symb_list) =
    List.fold_left (aux2 cons) cm symb_list
  in
  let cm = List.fold_left aux cm cons_list in
  (*Printf.printf "cardinal symb_cons_map = %d\n" (map_card m);*)
  cm



let code_merge_warning =
  if !Argument.merge_warning
  then "  let merge_warning = true\n"
  else "  let merge_warning = false\n"



let code_lexbuf_position = if !Argument.lexer = "ocamllex" then
  "  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)\n"
  else "  let lexbuf_position _ = (Lexing.dummy_pos,Lexing.dummy_pos)\n"



let code_nt_functions, code_str_non_ter =
  let aux (str,n) (nts,_) =
    (str^"    (Dyp_symbols."^nts^","^(string_of_int n)^");\n"),(n+1)
  in
  let nts_list,_ = List.fold_left aux ("",1) non_terminal_start_list in
  let string_length = String.length nts_list in
  let nts_list =  if nts_list="" then ""
    else String.sub nts_list 0 (string_length-1) in
  let ntna s code = code^"    \""^s^"\";\n" in
  let nt_ar = (String_set.fold ntna non_terminal_set "") in
  let string_length = String.length nt_ar in
  let nt_ar = String.sub nt_ar 0 (string_length-1) in
  "  let entry_points = [\n"^nts_list^"]\n"(*^
  "  let str_non_terminal nt =\n    try Dyp_symbols_array.non_ter_array.(nt)\n"^
  "    with Invalid_argument _ -> (\"new_nt_\"^(string_of_int nt))\n"*),
  "  let str_non_ter =\n  [|\"S'\";\n"^nt_ar^"|]\n"



(*let Dyp_parameters_signature =
  let aux_tok code (tok,_) = code^"val token_"^tok^" : int\n" in
  let token_id_type = List.fold_left aux_tok "" token_list in
  let aux_nt nt code = code^"val "^nt^" : int\n" in
  let non_ter_id_type = String_set.fold aux_nt non_terminal_set "" in
  "sig\n"^
  token_id_type^
  non_ter_id_type^
  "end"*)



let code_main_1 =
"module Dyp_runtime = Dyp.Make_dyp(Dyp_parameters)\n"^
"module Dyp_engine = Dyp_runtime.Parser_"^(!Argument.priority_enforcement)^"\n\n"



let code_priority_def,priority_set,code_str_prio =
  let code_str_prio =
    "  let str_prio = [|\n    \"default_priority\"" in
  let aux1 (str_set:String_set.t) rel = match rel with
    | Rel_list l -> List.fold_left (fun set p -> String_set.add p set) str_set l
    | Rel_single p1 -> String_set.add p1 str_set
  in
  let priority_set1 = List.fold_left aux1 String_set.empty relation in
  let aux1 st_set ld = match ld with
    | (Obj_terminal _),_ -> st_set
    | (Obj_non_terminal (_,(p,(line,col1,col2)),_,_)),_ -> if p="No_priority" then st_set
        else (if String_set.mem p priority_set1=false && p<>"default_priority" then (
          printf "File \"%s\", line %d, characters %d-%d:\n" input_file line col1 col2;
          printf "Warning: the priority `%s' is not declared\n" p);
        String_set.add p st_set)
  in
  let aux2 st_set (_,(p,(line,col1,col2)),ld_list,_,_) =
    let st_set = String_set.add p st_set in
    if String_set.mem p priority_set1=false && p<>"default_priority" then (
          printf "File \"%s\", line %d, characters %d-%d:\n" input_file line col1 col2;
          printf "Warning: the priority `%s' is not declared\n" p);
    List.fold_left aux1 st_set ld_list
  in
  let priority_set = List.fold_left aux2 priority_set1 grammar in
  let aux p (code,code_pn,n) =
      if p="default_priority" then (code,code_pn,n) else
      ((code^"  let priority_data, "^p^
      " = Dyp.insert_priority priority_data \""^p^"\"\n"),
      code_pn^";\n    \""^p^"\"",
      (n+1))
  in
  let code_priority_def,code_str_prio,_ =
    String_set.fold aux priority_set
      ("  let priority_data, default_priority =\n"^
      "    Dyp.insert_priority Dyp.empty_priority_data \"default_priority\"\n",
      code_str_prio,1)
  in
  code_priority_def,priority_set,code_str_prio^"|]\n"


let code_global_local_data =
"let global_data = ()
let local_data = ()
let global_data_equal = (==)
let local_data_equal = (==)\n\n"


let cons_type_map =
  let aux1 symb typ map =
    let cons =
      try String_map.find symb symb_cons_map
      with Not_found -> assert false
    in
    String_map.add cons typ map
  in
  let ct_map = String_map.fold aux1 token_map String_map.empty in
  String_map.fold aux1 nt_type_map ct_map


let code_cons_of_nt, cons_of_nt, cons_map, cons_set, cons_array, code_test_cons =

  let aux1 _ cons s = String_set.add cons s in
  let s = String_map.fold aux1 symb_cons_map String_set.empty in

  let aux0 s cons = String_set.add cons s in
  let s = List.fold_left aux0 s add_cons_list in

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
  let code_tc = "let __dypgen_test_cons =  [|\n" in
  let code, cons_map, _ =
    String_set.fold aux2 s ([code_tc],String_map.empty,0)
  in

  let code = List.rev ("|]\n\n"::(List.tl code)) in
  let code_tc = String.concat "" code in

  let cons_of_nt =
    Array.make ((String_set.cardinal non_terminal_set)+1) 0
  in
  let aux3 symb cons (l,i) =
    let first = int_of_char symb.[0] in
    if first>64 && first<91 then
      (l,i)
    else
      let cons_i = String_map.find cons cons_map in
      cons_of_nt.(i) <- cons_i;
      (";\n"::(string_of_int cons_i)::"    "::l, (i+1))
  in
  let code_cont = "  let cons_of_nt =\n  [|0;\n" in
  (* first corresponds to S' (which has no constructor anyway). *)
  let code,_ = String_map.fold aux3 symb_cons_map ([code_cont],1) in
  let code = List.rev ("|]\n"::(List.tl code)) in
  let code_cont = String.concat "" code in
  code_cont, cons_of_nt, cons_map, s, cons_array, code_tc



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



let code_datadyn =
  let code_datadyn =
    "  let datadyn = Dyp_runtime.Tools.init_datadyn [\n"
  in
  let aux1 nt_str (codl,nt) =
    let i = cons_of_nt.(nt) in
    ";\n"::("    \""^nt_str^"\","^(string_of_int i)^
    ",\""^cons_array.(i)^"\"")::codl,
    nt+1
  in
  let codl,_ = String_set.fold aux1 non_terminal_set ([code_datadyn],1) in
  let codl = "\n    ][\n"::(List.tl codl) in
  let aux2 cons _ codl = ";\n"::("    \""^cons^"\"")::codl in
  let codl = String_map.fold aux2 cons_map codl in
  let codl = List.rev ("]\n"::(List.tl codl)) in
  String.concat "" codl



(*let code_dypgen_toolbox_type =
"type ('obj,'data,'local_data) dypgen_toolbox = {
  mutable global_data : 'data;
  mutable local_data : 'local_data;
  mutable priority_data : Dyp.priority_data;
  mutable add_rules : (Dyp.rule * (
    ('obj,'data,'local_data) dypgen_toolbox -> 'obj list -> 'obj)) list;
  mutable remove_rules : Dyp.rule list;
  mutable will_shift : bool;
  mutable next_state : out_channel option;
  mutable next_grammar : out_channel option;
  symbol_start : unit -> int;
  symbol_start_pos : unit -> Lexing.position;
  symbol_end : unit -> int;
  symbol_end_pos : unit -> Lexing.position;
  rhs_start : int -> int;
  rhs_start_pos : int -> Lexing.position;
  rhs_end : int -> int;
  rhs_end_pos : int -> Lexing.position;
  add_nt : string -> string -> Dyp.non_ter;
  find_nt : string -> Dyp.non_ter * string;
  print_state : out_channel -> unit;
  print_grammar : out_channel -> unit;
}\n\n"*)



(*let code_transform_action =
"  open Dyp
  let rec transform_action a =
    Dyp.Dyp_special_types.Dypgen_action(fun av_list symbol_pos
    position_list data_arg datadyn local_data_arg prio_data debug_infos ->
      let __dypgen_datadyn = ref datadyn in
      let dyp = {
        global_data = data_arg;
        local_data = local_data_arg;
        priority_data = prio_data;
        add_rules = [];
        remove_rules = [];
        will_shift = true;
        next_state = None;
        next_grammar = None;
        symbol_start = (fun () -> (fst symbol_pos).Lexing.pos_cnum);
        symbol_start_pos = (fun () -> fst symbol_pos);
        symbol_end = (fun () -> (snd symbol_pos).Lexing.pos_cnum);
        symbol_end_pos = (fun () -> snd symbol_pos);
        rhs_start = (fun i -> (fst (List.nth position_list (i-1))).Lexing.pos_cnum);
        rhs_start_pos = (fun i -> fst (List.nth position_list (i-1)));
        rhs_end = (fun i -> (snd (List.nth position_list (i-1))).Lexing.pos_cnum);
        rhs_end_pos = (fun i -> snd (List.nth position_list (i-1)));
        add_nt = (fun nt cons -> Dyp_runtime.Tools.add_nt nt cons __dypgen_datadyn);
        find_nt = (fun (s:string) -> Dyp_runtime.Tools.find_nt s !__dypgen_datadyn);
      print_state =
        debug_infos.Dyp.Dyp_special_types.prt_state;
      print_grammar =
        debug_infos.Dyp.Dyp_special_types.prt_grammar
      }
      in
      let new_obj = a dyp av_list in
      let mapfun (r,ac) = (r,(transform_action ac)) in
      let add_rules_transformed = List.map mapfun dyp.add_rules in
      (new_obj,dyp.will_shift,dyp.global_data,!__dypgen_datadyn,
      dyp.local_data,add_rules_transformed,dyp.remove_rules,
      dyp.priority_data,dyp.next_state,dyp.next_grammar))\n"*)



let code_grammar =

  let aux (lhs_nt,(prio,_),ld_list,par_act_l,ocaml_code) =
    let aux2 code ld = match ld with
      | (Obj_terminal (ter,(line,col1,col2))),_ ->
          let _ = (try String_map.find ter token_map
            with Not_found -> (
              fprintf stderr "File \"%s\", line %d, characters %d-%d:\n" input_file line col1 col2;
              fprintf stderr "Token `%s' not declared\n" ter; exit 2))
          in
          code^"Dyp.Ter Dyp_symbols.t_"^ter^";"
      | (Obj_non_terminal (ntn,(p,_),eq,_)),_ ->
          let code_p =
            if p = "No_priority" then "Dyp.No_priority "
            else (match eq with
              | Pr_eq -> "Dyp.Eq_priority "
              | Pr_lesseq -> "Dyp.Lesseq_priority "
              | Pr_less -> "Dyp.Less_priority "
              | Pr_greater -> "Dyp.Greater_priority "
              | Pr_greatereq -> "Dyp.Greatereq_priority ")^"Dyp_priority_data."^p
          in
          code^"Dyp.Non_ter (Dyp_symbols."^ntn^","^code_p^");"
    in

    let code_literal_list = List.fold_left aux2 "" ld_list in

    let string_length = (String.length code_literal_list) in
    let code_literal_list =
      if string_length = 0 then code_literal_list
      else if code_literal_list="" then ""
        else String.sub code_literal_list 0 (string_length-1)
    in
    let code_rule = "(Dyp_symbols."^lhs_nt^",["^ code_literal_list^ "],Dyp_priority_data."^prio^")" in

    let code_var_list =
      let f (code,n) lit = match lit with
        | (Obj_terminal (ter,_)),patternl ->
            let typ = String_map.find ter token_map in
            if typ = "No_type" then
              code^" _"^(string_of_int n)^";", n+1
            else
              let pat,pat_typ,(lnum,offset) = List.hd patternl in
              code^"`Real_obj ("^obj_pref^(String_map.find ter symb_cons_map)^" "^
              " ("^(sharp_line_number lnum)^
              (space_string offset)^
              (try "("^pat^":"^(String_map.find pat_typ token_map)^")"
              with Not_found -> pat)^
              insert_line_number^" as _"^(string_of_int n)^"));",
              n+1
        | (Obj_non_terminal (nt,_,_,res_nb)),patternl ->
            let rec aux res n patl = match patl with
              | [pat,nt,(lnum,offset)] ->
                  res^" ("^(sharp_line_number lnum)^
                  (space_string offset)^
                  (try "("^pat^":'dypgen__"^(String_map.find nt symb_cons_map)^")"
                  with Not_found -> pat)^
                  (*(try "("^pat^":"^
                    (try String_map.find lhs_nt nt_type_map
                    with Not_found ->
                      "'dypgen__"^(String_map.find nt symb_cons_map))^")"
                  with Not_found -> pat)^*)
                  insert_line_number^
                  " as _"^(string_of_int n)^")"
              | (pat,nt,(lnum,offset))::tl ->
                  aux (res^" ("^(sharp_line_number lnum)^
                  (space_string offset)^
                  (try "("^pat^":'dypgen__"^(String_map.find nt symb_cons_map)^")"
                  with Not_found -> pat)^
                  insert_line_number^
                  " as _"^(string_of_int n)^")"^
                  ",") (n+1) tl
              | _ -> failwith "code_var_list"
            in
            let str = aux "" n patternl in
            (*code^"`Real_obj ("^obj_pref^"Obj_"^nt^" ("^str^"));",*)
            code^"`Real_obj ("^obj_pref^(String_map.find nt symb_cons_map)^" ("^str^"));",
            (* The extra parentheses around str are necessary
              for non terminals generated by partial actions. *)
            (*code^"`Real_obj ("^obj_pref^
            (String_map.find nt symb_cons_map)^str^");",*)
            n+(List.length patternl)
      in
      let c,_ = List.fold_left f ("",1) ld_list in
      let s_length = (String.length c) in
      if s_length = 0 then c
      else String.sub c 0 (s_length-1)
    in

    let code_action =
      let (action,(header_act,trailer_act)),((lnum,offset),b) = ocaml_code in
      let typ =
        try
          String_map.find lhs_nt nt_type_map
        with Not_found -> "'dypgen__"^
          (String_map.find lhs_nt symb_cons_map)
      in
      String.concat ""
        (if b then
          ["Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match ";
          "(Dyp_runtime.Tools.transform_action ";
          "(fun dyp __dypgen_av_list -> (match ";
          "(Dyp_aux_functions.transform_av_list ";
          "__dypgen_av_list) with [";
          code_var_list;"] -> ";
          " let res = ";
          (sharp_line_number (lnum-1));"(\n";(space_string offset);
          "(";action;"):";typ;" * ('obj,'gd,'ld) Dyp.dyp_action list)";
        (* The extra parentheses around action are useful when the action
        is empty, it converts it to unit. *)
          insert_line_number;
          "  in ";
          obj_pref;(String_map.find lhs_nt symb_cons_map);
          header_act;"(fst res)";trailer_act;", snd res\n";
          " | _ -> raise Dyp.Giveup))) with ";
          "Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h)"]
        else
          ["Dyp.Dyp_special_types.Dypgen_action (fun a b c d e f g h -> match ";
          "(Dyp_runtime.Tools.transform_action ";
          "(fun dyp __dypgen_av_list -> (match ";
          "(Dyp_aux_functions.transform_av_list ";
          "__dypgen_av_list) with [";
          code_var_list;"] -> ";obj_pref;
          (String_map.find lhs_nt symb_cons_map);" ";header_act;
          (sharp_line_number (lnum-1));"(\n";(space_string offset);
          "(";action;"):";typ;")";
        (* The extra parentheses around action are useful when the action
        is empty, it converts it to unit. *)
          insert_line_number;trailer_act;
          ",[] | _ -> raise Dyp.Giveup))) with ";
          "Dyp.Dyp_special_types.Dypgen_action ac_fun -> ac_fun a b c d e f g h)"]
        )
    in

    String.concat "" ["(";code_rule;",";code_action;")"]
  in
  let list_code_rapf = List.map aux grammar in
  let rec aux res sl = match sl with
    | [s] -> "let __dypgen_ra_list =\n[\n"::s::res
    | s::t -> aux ("\n;\n"::s::res) t
    | [] -> failwith "empty grammar"
  in
  let list_code_rapf = aux ["]\n\n"] list_code_rapf in
  String.concat "" list_code_rapf

  (*let code_grammar =*)
    (*let rec aux sl = match sl with
      | [] -> ""
      | [s] -> s
      | s::t -> s^"\n;\n"^(aux t)
    in "let __dypgen_ra_list =\n[\n"^
    (aux list_code_rapf)^"]\n\n"*)
  (*in
  code_grammar^
  "let current_grammar,nt_nb,map_po,user_g = make_grammar rapf_list __dypgen_priority_data\n"*)



let code_transform_av_list =
  let aux cons code =
    try
      let typ = String_map.find cons cons_type_map in
      if typ="No_type" then
        code^"      | "^obj_pref^cons^" -> `Dummy_obj\n"
      else code
    with Not_found -> code
  in
  let code_match_dummy = String_set.fold aux cons_set "" in
"  let transform_av_list l =
    let f o = match o with\n"^code_match_dummy^
  "      | x -> `Real_obj x
    in
    List.map f l\n"



let code_prio_data =
  (*let code_relation_data = "let __dypgen_priority_data = Dyp.empty_priority_data\n" in*)
  let aux code rel = match rel with
    | Rel_list l ->
        let code_rel_list = List.fold_left (fun c s -> c^s^";") "" l in
        let string_length = String.length code_rel_list in
        let code_rel_list = "["^
          (if code_rel_list="" then ""
           else (String.sub code_rel_list 0 (string_length-1)))
          ^"]" in
        code^"  let priority_data = Dyp.add_list_relations priority_data "
        ^code_rel_list^"\n"
    | Rel_single p -> code(*^"let __dypgen_priority_data = Dyp.insert_priority __dypgen_priority_data "^p^"\n"*)
  in
  (List.fold_left aux "" relation)




(*
module Pparam =
struct
  let default_priority = 0
  type token = Token_void
  type obj = Obj_void
  type token_name = int
  let dummy_token_name = token_dummy_to_marshal
  let token_epsilon = token_epsilon_to_marshal
  let compare_token_name t1 t2 = Pervasives.compare t1 t2
  let get_name t = 0
  let str_token t = ""
  let str_token_name t = ""
  type Dyp.non_ter = int
  let entry_points =
    let map_fun (nts,_) = String_map.find nts non_terminal_map in
    List.map map_fun non_terminal_start_list
  let compare_ntn nt1 nt2 = Pervasives.compare nt1 nt2
  let str_non_terminal nt = string_of_int nt
  type data = Data_void
  let compare_data = (=)
  let default_data = Data_void
  type datadyn = Datadyn_void
  let default_datadyn = Datadyn_void
  let default_obj = Obj_void
  let get_value t = Obj_void
  type automaton_kind = `LR1 | `LALR
  let automaton_kind = if automaton_is_`LR1 then `LR1 else `LALR
  let merge_warning = false
end

module Prio =
struct
  type priority = int
end

module Pparam_relation =
struct
  include Prio
  include Dyp.Priority_by_relation.Make(Prio)
  include Pparam
end

module Pparam_set =
struct
  include Prio
  include Dyp.Priority_by_set.Make(Prio)
  include Pparam
end

module P_relation = Dyp.Parser.Make(Pparam_relation)
module P_set = Dyp.Parser.Make(Pparam_set)

let cst c = (function i -> c)

module Calc_aut_relation =
struct
  open Pparam_relation
  open P_relation
  let rhs_lit obj_lit = match obj_lit with
    | Obj_terminal ter -> Dyp.Ter (String_map.find ter token_name_map)
    | Obj_non_terminal (ntn,p,eq) ->
        let prio_nt =
          if p = "No_priority" then No_priority
          else
            let prio = String_map.find p priority_map in
            if eq then Lesseq_priority prio else Less_priority prio
        in
        Dyp.Non_ter (String_map.find ntn non_terminal_map,prio_nt)
  let aux (nt,_,l,ac_desc) =
    let ac = match ac_desc with
      | Dynamic_action _ -> Dyp.Dyp_special_types.Dypgen_action(fun _ _ _ _ _ _ -> Obj_void,true,default_data,
          default_datadyn, [], [], Dyp.empty_priority_data)
      | _ -> Classic(fun _ _ _ _ _ -> Obj_void,true,default_data)
    in
    (((String_map.find nt non_terminal_map),List.map rhs_lit l),ac,cst 0)
  let rapf_list = if set = [] then List.map aux grammar else []

  let grammar_for_sa = if set = [] then add_grammar empty_grammar rapf_list else empty_grammar
  let marshaled_automaton = if set = [] then
    Marshal.to_string (create_saved_automaton grammar_for_sa) []
    else ""
end

module Calc_aut_set =
struct
  open Pparam_set
  open P_set
  let aux priority_set_map (x,pl) =
    if pl=["priority declaration"] then priority_set_map else
    let f s = String_map.find s priority_map in
    let ntn_p = change_ntp_list empty_ntp (List.map f pl) in
    String_map.add x ntn_p priority_set_map
  let priority_set_map = if set=[] then String_map.empty else
    List.fold_left aux String_map.empty set

  let rhs_lit obj_lit = match obj_lit with
    | Obj_terminal ter -> Dyp.Ter (String_map.find ter token_name_map)
    | Obj_non_terminal (ntn,p,eq) ->
        let prio_nt =
          if p = "No_priority" then No_priority
          else
            let prio = String_map.find p priority_set_map in
            if eq then failwith("equal with priority_by_set")
            else prio
        in
        Dyp.Non_ter (String_map.find ntn non_terminal_map,prio_nt)
  let aux (nt,_,l,ac_desc) =
    let ac = match ac_desc with
      | Dynamic_action _ -> Dyp.Dyp_special_types.Dypgen_action(fun _ _ _ _ _ _ -> Obj_void,true,default_data,
          default_datadyn, [], [], Dyp.empty_priority_data)
      | _ -> Classic(fun _ _ _ _ _ -> Obj_void,true,default_data)
    in
    (((String_map.find nt non_terminal_map),List.map rhs_lit l),ac,cst 0)
  let rapf_list = if set=[] then [] else List.map aux grammar

  let grammar_for_sa = if set = [] then empty_grammar else add_grammar empty_grammar rapf_list
  let marshaled_automaton = if set = [] then "" else
    Marshal.to_string (create_saved_automaton grammar_for_sa) []
end




let code_marshaled_automaton =
  let marshaled_automaton =
    if set = [] then Calc_aut_relation.marshaled_automaton
    else Calc_aut_set.marshaled_automaton
  in
  let marshaled_automaton = String.escaped marshaled_automaton in
  "let marshaled_automaton = \""^marshaled_automaton^"\"\n"
*)


let code_main_2 =
  let aux str (nts,_) = try
  str^"let "^nts^" ?(global_data=global_data) ?(local_data=local_data) f lexbuf =
  let pf = Dyp_engine.glrParse __dypgen_automaton Dyp_aux_functions.get_token_value
    Dyp_symbols.get_token_name Dyp_symbols.str_token
    Dyp_symbols."^nts^" __dypgen_data_equal __dypgen_test_cons Dyp_symbols_array.str_cons (Dyp_runtime.Tools.array_of_list __dypgen_merge_list) global_data local_data __dypgen_ra_list Dyp_priority_data.priority_data Dyp_symbols_array.str_non_ter f lexbuf
    Dyp_aux_functions.lexbuf_position in
  let aux1 (o,p) = match o with
    | "^obj_pref^(String_map.find nts symb_cons_map)^" r -> (r,p)
    | _ -> failwith \"Wrong type for entry result\" in
  List.map aux1 pf\n\n" with Not_found -> failwith "code_main_2"
  in
(*"let saved_automaton = Marshal.from_string marshaled_automaton 0
let automaton = complete_automaton saved_automaton current_grammar default_data default_datadyn __dypgen_priority_data merge_map merge\n"^*)
"let __dypgen_automaton = Dyp_engine.create_parsing_device __dypgen_ra_list Dyp_priority_data.priority_data "^
!Argument.aut_kind^" global_data local_data Dyp_aux_functions.datadyn Dyp_symbols_array.str_non_ter Dyp_symbols_array.cons_of_nt Dyp_symbols_array.str_prio\n\n"^
  "let __dypgen_data_equal = {\n"^
  "  Dyp_runtime.Tools.global_data_equal = global_data_equal;\n"^
  "  Dyp_runtime.Tools.local_data_equal = local_data_equal }\n\n"^
  (List.fold_left aux "" non_terminal_start_list)



(*let code_type_obj =
  (*if !Argument.pv_obj then "" else*)
  let code_obj =
    let aux nt code =
      if List.exists (fun (nts,_) -> nts=nt) non_terminal_start_list
      then code else code^"'"^nt^","
    in
    let type_param =(String_set.fold aux non_terminal_set "") in
    let string_length = String.length type_param in
    (if type_param="" then ""
      else "("^(String.sub type_param 0 (string_length-1))^")")^
    " obj"
  in
  let aux1 tok typ code = if typ = "No_type" then code^"  | "^obj_pref^"Obj_"^tok^"\n"
    else code^"  | "^obj_pref^"Obj_"^tok^" of ("^typ^")\n"
  in
  let aux2 nt (code,conss) =
    let cons = String_map.find nt symb_cons_map in
    if String_set.mem cons conss then (code,conss) else
    let code =
      try
        let (_,start_type) =
          List.find (fun (nts,_) -> nts=nt) non_terminal_start_list
        in code^"  | "^obj_pref^cons^" of ("^start_type^")\n"
      with Not_found -> code^"  | "^obj_pref^cons^" of '"^nt^"\n"
    in
    code, (String_set.add cons conss)
  in
  let code,_ = (String_set.fold aux2 non_terminal_set ("",String_set.empty)) in
  "type "^
  code_obj^
  " ="^
  (if !Argument.pv_obj then " [\n" else "\n")^
  (String_map.fold aux1 token_map "")^code^
  (if !Argument.pv_obj then " ]\n\n" else "\n")*)

let code_type_obj =
  let code_obj =
    if String_set.cardinal cons_no_type_set = 0 then
      "type obj ="
    else
      let aux cons codl = ", "::("'a_"^cons)::codl in
      let codl = String_set.fold aux cons_no_type_set ["type ("] in
      let codl = List.rev (") obj ="::(List.tl codl)) in
      String.concat "" codl
  in

  let aux cons codl =
    let typ =
      try String_map.find cons cons_type_map
      with Not_found -> "'a_"^cons
    in
    let typ =
      if typ = "No_type" then ""
      else " of "^typ
    in
    ("  | "^obj_pref^cons^typ^"\n")::codl
  in
  let codl = String_set.fold aux cons_set [] in
  let code = String.concat "" (List.rev codl) in

  code_obj^
  (if !Argument.pv_obj then " [\n" else "\n")^
  code^
  (if !Argument.pv_obj then " ]\n\n" else "\n")



let code_merge_functions =
  let aux cons_s _ code = code^"let dyp_merge_"^cons_s^
  " = Dyp_runtime.Tools.keep_zero\n" in
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
  let aux code (tok,typ) =
    let cons = String_map.find tok symb_cons_map in
    let s = if typ = "No_type" then " -> "^obj_pref^cons^"\n"
      else " x -> "^obj_pref^cons^" x\n"
    in
    code^"    | "^token_pref^tok^s
  in
  "  let get_token_value t = match t with\n"^
  (List.fold_left aux "" token_list)





let parser_codl = [
  topheader_main;
  code_type_token;

  "module Dyp_symbols =\nstruct\n";
  code_non_terminal_decl;
  code_token_name_decl;
  code_get_token_name;
  code_str_token;
  code_ter_string_list;
  code_String_ter_map;
  code_ter_of_string;
  "end\n\n";

  code_type_obj;

  "module Dyp_symbols_array =\nstruct\n";
  code_str_non_ter;
  code_str_prio;
  code_token_name_array;
  code_cons_of_nt;
  code_str_cons;
  "end\n\n";

  "module Dyp_parameters =\nstruct\n";
  code_token_nb;
  code_undef_nt;
  code_nt_functions;
  code_str_token_name;
  code_merge_warning;
  "end\n\n";

  code_main_1;

  "module Dyp_aux_functions =\nstruct\n";
  code_datadyn;
  code_get_token_value;
  code_lexbuf_position;
  code_transform_av_list;
  "end\n\n";

  "module Dyp_priority_data =\nstruct\n";
  code_priority_def;
  code_prio_data;
  "end\n\n";

  code_global_local_data;
  code_merge_functions;

  header_main;

  code_grammar;
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



let parser_code_mli =
  let entry_code =
    if !Argument.lexer = "ocamllex"
    then " -> (Lexing.lexbuf -> token) -> Lexing.lexbuf -> "
    else if !Argument.lexer = "ulex"
    then " -> (Ulexing.lexbuf -> token) -> Ulexing.lexbuf -> "
    else " -> ('a -> token) -> 'a -> "
  in
  let aux str (nts,start_type) =
    str^"val "^nts^" : ?global_data:"^global_data_type^" -> ?local_data:"^
    local_data_type^entry_code^"(("^start_type^") * Dyp.priority) list\n"
  in
  let aux2 p code = if p="default_priority" then code
    else code^"  val "^p^" : Dyp.priority\n"
  in
  topmli_code ^
  code_type_token^
  "module Dyp_priority_data :\nsig\n"^
  "  val priority_data : Dyp.priority_data\n"^
  "  val default_priority : Dyp.priority\n"^
  (String_set.fold aux2 priority_set "")^
  "end\n\n"^
  (List.fold_left aux "" non_terminal_start_list)^
  mli_code



let () = Insert_linenum.buffer := String.copy parser_code
let lexbuf = Lexing.from_string parser_code
let parser_code = Insert_linenum.insert_linenum lexbuf
(*let parser_code = parser_code^code_marshaled_automaton^code_main_2*)
(*let parser_code = parser_code^code_main_2*)

let () = Insert_linenum.buffer := String.copy parser_code_mli
let lexbuf = Lexing.from_string parser_code_mli
let parser_code_mli = Insert_linenum.insert_linenum lexbuf

let dest_file = open_out output_file
let dest_file_mli = open_out output_file_mli

let () = output_string dest_file parser_code
let () = output_string dest_file_mli parser_code_mli
let () = close_out dest_file
let () = close_out dest_file_mli

