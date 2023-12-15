{
open Lexing

module Ordered_string =
struct
  type t = string
  let compare = Stdlib.compare
end

module String_map = Map.Make(Ordered_string)

let string_buf = Buffer.create 20

}

let newline = ('\010' | '\013' | "\013\010")
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246'
   '\248'-'\255' '\'' '0'-'9']
let blank = [' ' '\009' '\012']
let var_type = ''' lowercase identchar *
let comma = ',' newline ? ' ' *

rule fun_type map = parse
  | ' ' * "val __dypgen_dummy_marker_3 : unit" newline
    { fun_type_2 map "" lexbuf }
  | [^'\010''\013'] + { fun_type map lexbuf }
  | newline { fun_type map lexbuf }

and parser_type = parse
  | newline ' ' * "val __dypgen_dummy_marker_2 : unit" newline
    ' ' * "val pp :"
    { Buffer.clear string_buf;
    Buffer.add_string string_buf "val pp :";
    extract_pp_type lexbuf }
  | [^'\010''\013'] + { parser_type lexbuf }
  | newline { parser_type lexbuf }

and token_type = parse
  | ' ' * "type " (var_type | ('(' var_type (comma var_type)+ ')'))?
    newline ? ' '* "token =" [^'\010''\013'] * newline
      { Buffer.clear string_buf;
      Buffer.add_string string_buf (Lexing.lexeme lexbuf);
      extract_type_2 lexbuf }
  | [^'\010''\013'] * newline
    { token_type lexbuf }

and obj_type = parse
  | ' ' * "type " (var_type | ('(' var_type (comma var_type)+ ')'))?
    newline ? ' '* "obj =" [^'\010''\013'] * newline
      { Buffer.clear string_buf;
      Buffer.add_string string_buf (Lexing.lexeme lexbuf);
      extract_type_2 lexbuf }
  | [^'\010''\013'] * newline
    { obj_type lexbuf }

and extract_type_2 = parse
  | ' ' * ("module " | "type " | "val ") [^'\010''\013'] * newline
      { Buffer.contents string_buf }
  | [^'\010''\013'] * newline
    { Buffer.add_string string_buf (Lexing.lexeme lexbuf);
    extract_type_2 lexbuf }

and extract_pp_type = parse
  | ' ' * "val __dypgen_dummy_marker_5 : unit" newline
      { Buffer.add_string string_buf "\n";
      let s = (*TODO String.copy*) (Buffer.contents string_buf) in
      Buffer.clear string_buf;
      let lexbuf2 = Lexing.from_string s in
      let slist = List.rev (remove_tpar [] lexbuf2) in
      let s = String.concat "" slist in
      let lexbuf2 = Lexing.from_string s in
      fix_variant s lexbuf2;
      s }
  | [^'\010''\013'] * newline
      { Buffer.add_string string_buf (Lexing.lexeme lexbuf);
      extract_pp_type lexbuf }

and fun_type_2 map curr_val = parse
  | ' ' * "val __dypgen_dummy_marker_4 : unit" newline
    { let m =
      if curr_val <> "" then
        let s = Buffer.contents string_buf in
        let lexbuf2 = Lexing.from_string s in
        let slist = List.rev (remove_tpar [] lexbuf2) in
        let s = String.concat "" slist in
        let lexbuf2 = Lexing.from_string s in
        fix_variant s lexbuf2;
        String_map.add curr_val s map
      else map in
    Buffer.clear string_buf;
    m }
  | ' ' * "val " ((lowercase identchar *) as ident) " :"
    (([^'\010''\013'] * newline) as s)
    { let m =
      if curr_val <> "" then
        String_map.add curr_val
        ((*TODO String.copy*) (Buffer.contents string_buf)) map
      else map in
    Buffer.clear string_buf;
    Buffer.add_string string_buf s;
    fun_type_2 m ident lexbuf }
  | [^'\010''\013'] * newline
      { Buffer.add_string string_buf (Lexing.lexeme lexbuf);
      fun_type_2 map curr_val lexbuf }

and remove_tpar slist = parse
  | "'_" identchar + { remove_tpar ("unit"::slist) lexbuf }
  | eof { slist }
  | [^'''] + eof { (Lexing.lexeme lexbuf)::slist }
  | [^'''] + { remove_tpar ((Lexing.lexeme lexbuf)::slist) lexbuf }
  | ''' [^'_'] [^'''] *
      { remove_tpar ((Lexing.lexeme lexbuf)::slist) lexbuf }

and replace_tpar oldtp newtp = parse
  | [^'''] * eof { Buffer.add_string string_buf (Lexing.lexeme lexbuf) }
  | ''' ['a'-'z'] ['0'-'9']* [' ''\010''\013'','')']
    { let r = Bytes.of_string(Lexing.lexeme lexbuf) in
    let len = Bytes.length r in
    let s = (Bytes.sub r 1 (len-2)) in
    if s = oldtp then
      (let s = Bytes.of_string ("'"^newtp^" ") in
      let len2 = Bytes.length s in
      Bytes.set s (len2-1) (Bytes.get r (len-1));
      Buffer.add_bytes string_buf s)
    else
      Buffer.add_bytes string_buf r;
    replace_tpar oldtp newtp lexbuf }
  | [^'''] + { Buffer.add_string string_buf (Lexing.lexeme lexbuf);
    replace_tpar oldtp newtp lexbuf }
  | ''' { Buffer.add_string string_buf "'";
    replace_tpar oldtp newtp lexbuf }

and fix_variant fun_typ = parse
  | [^'_''['] * eof { () }
  | "_[" ['<''>']
    { let i = Lexing.lexeme_start lexbuf in
    let fun_typ2 = Bytes.of_string fun_typ in
    Bytes.set fun_typ2 i  ' '; Bytes.set fun_typ2 (i+2)  ' ';
    fix_variant (Bytes.to_string fun_typ2) lexbuf}
  | "[" ['<''>']
    { let i = Lexing.lexeme_start lexbuf in
    let fun_typ2 = Bytes.of_string fun_typ in
    Bytes.set fun_typ2 (i+1)  ' ';
    fix_variant (Bytes.to_string fun_typ2) lexbuf}
  | [^'_''['] + { fix_variant fun_typ lexbuf }
  | ['_''['] { fix_variant fun_typ lexbuf }

and remove_conjunctive fun_typ = parse
  | [^' '] * eof { () }
  | " of "
    { fst_in_conj fun_typ lexbuf; remove_conjunctive fun_typ lexbuf }
  | [^' '] + { remove_conjunctive fun_typ lexbuf }
  | ' ' { remove_conjunctive fun_typ lexbuf }

and remove_conj_2 fun_typ = parse
  | [^' '')'']'] + { remove_conj_2 fun_typ lexbuf }
  | " of "
    { fst_in_conj fun_typ lexbuf; remove_conjunctive fun_typ lexbuf }
  | ' ' { remove_conjunctive fun_typ lexbuf }
  | [')'']'] { () }

and fst_in_conj fun_typ = parse
  | [^'&''(''['']''|'] + { fst_in_conj fun_typ lexbuf }
  | '&' { let start_pos = Lexing.lexeme_start lexbuf in
    let end_pos = end_of_conj lexbuf in
    Bytes.fill fun_typ start_pos (end_pos-start_pos) ' ' }
  | ['(''['] { remove_conj_2 fun_typ lexbuf;
    fst_in_conj fun_typ lexbuf }
  | ['|'']'] { () }

and end_of_conj = parse
  | [^'(''['']''|''\010''\013'] + { end_of_conj lexbuf }
  | ['(''['] { match_paren lexbuf; end_of_conj lexbuf }
  | newline ? ' ' * ['|'']'] { Lexing.lexeme_start lexbuf }
  | newline { end_of_conj lexbuf }

and match_paren = parse
  | [^'(''['')'']'] { match_paren lexbuf }
  | ['(''['] { match_paren lexbuf; match_paren lexbuf }
  | [')'']'] { () }

and match_Arg_ = parse
  | "_Arg_" { true }
  | eof { false }
  | _ { match_Arg_ lexbuf }
