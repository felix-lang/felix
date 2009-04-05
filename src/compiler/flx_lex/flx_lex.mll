{
open Flx_util
open Flx_token
open Flx_string
open Big_int
open Flx_exceptions
open Flx_lexstate
open Flx_preproc

let lexeme = Lexing.lexeme
let lexeme_start = Lexing.lexeme_start
let lexeme_end = Lexing.lexeme_end

let substr = String.sub
let len = String.length

(* string parsers *)
let decode_qstring s = let n = len s in unescape (substr s 0 (n-1))
let decode_dstring s = let n = len s in unescape (substr s 0 (n-1))
let decode_qqqstring s = let n = len s in unescape (substr s 0 (n-3))
let decode_dddstring s = let n = len s in unescape (substr s 0 (n-3))

let decode_raw_qstring s = let n = len s in substr s 0 (n-1)
let decode_raw_dstring s = let n = len s in substr s 0 (n-1)
let decode_raw_qqqstring s = let n = len s in substr s 0 (n-3)
let decode_raw_dddstring s = let n = len s in substr s 0 (n-3)

exception Ok of int
exception SlashSlash of int
exception SlashAst of int

(* WARNING: hackery: adjust this when lex expression 'white'
   is adjutsed
*)

}

(* ====================== REGULAR DEFINITIONS ============================ *)
(* special characters *)
let quote = '\''
let dquote = '"'
let slosh = '\\'
let linefeed = '\n'
let tab = '\t'
let space = ' '
let formfeed = '\012'
let vtab = '\011'
let carriage_return = '\013'
let underscore = '_'

(* character sets *)
let bindigit = ['0'-'1']
let octdigit = ['0'-'7']
let digit = ['0'-'9']
let hexdigit = digit | ['A'-'F'] | ['a'-'f']
let lower = ['a'-'z']
let upper = ['A'-'Z']
(* let letter = lower | upper *)
let letter = lower | upper
let hichar = ['\128'-'\255']
let white = space | tab

(* nasty: form control characters *)
let form_control = linefeed | carriage_return | vtab | formfeed
let newline_prefix = linefeed | carriage_return
let newline = formfeed | linefeed  | carriage_return linefeed
let hash = '#'

let ordinary = letter | digit | hichar |
  '!' | '$' | '%' | '&' | '(' | ')' | '*' |
  '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' |
  '=' | '>' | '?' | '@' | '[' | ']' | '^' | '_' |
  '`' | '{' | '|' | '}' | '~'

(* any sequence of these characters makes one or more tokens *)
(* MISSING: # should be in here, but can't be supported atm
  because preprocessor # uses a conditional, and just errors
  out if the # isn't at the start of a line .. needs fixing,
  not sure how to fix it
*)

let symchar =
  '!' | '$' | '%' | '&' | '(' | ')' | '*' |
  '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' |
  '=' | '>' | '?' | '@' | '[' | ']' | '^' |
  '`' | '{' | '|' | '}' | '~' | '#' | '\\'

let printable = ordinary | quote | dquote | slosh | hash

(* identifiers *)
let ucn =
    "\\u" hexdigit hexdigit hexdigit hexdigit
  | "\\U" hexdigit hexdigit hexdigit hexdigit hexdigit hexdigit hexdigit hexdigit

let prime = '\''
let idletter = letter | underscore | hichar | ucn
let identifier = idletter (idletter | digit | prime )*

(* integers *)
let bin_lit  = '0' ('b' | 'B') (underscore? bindigit) +
let oct_lit  = '0' ('o' | 'O') (underscore? octdigit) +
let dec_lit  = ('0' ('d' | 'D'| "d_" | "D_"))? digit (underscore? digit) *
let hex_lit  = '0' ('x' | 'X') (underscore? hexdigit)  +
let fastint_type_suffix = 't'|'T'|'s'|'S'|'i'|'I'|'l'|'L'|'v'|'V'|"ll"|"LL"
let exactint_type_suffix =
    "i8" | "i16" | "i32" | "i64"
  | "u8" | "u16" | "u32" | "u64"
  | "I8" | "I16" | "I32" | "I64"
  | "U8" | "U16" | "U32" | "U64"

let signind = 'u' | 'U'

let suffix =
    '_'? exactint_type_suffix
  | ('_'? fastint_type_suffix)? ('_'? signind)?
  | ('_'? signind)? ('_'? fastint_type_suffix)?

let int_lit = (bin_lit | oct_lit | dec_lit | hex_lit) suffix

(* floats: Follows ISO C89, except that we allow underscores *)
let decimal_string = digit (underscore? digit) *
let hexadecimal_string = hexdigit (underscore? hexdigit) *

let decimal_fractional_constant =
  decimal_string '.' decimal_string?
  | '.' decimal_string

let hexadecimal_fractional_constant =
  ("0x" |"0X")
  (hexadecimal_string '.' hexadecimal_string?
  | '.' hexadecimal_string)

let decimal_exponent = ('E'|'e') ('+'|'-')? decimal_string
let binary_exponent = ('P'|'p') ('+'|'-')? decimal_string

let floating_suffix = 'L' | 'l' | 'F' | 'f' | 'D' | 'd'
let floating_literal =
  (
    decimal_fractional_constant decimal_exponent? |
    hexadecimal_fractional_constant binary_exponent?
  )
  floating_suffix?

(* Python strings *)
let qqq = quote quote quote
let ddd = dquote dquote dquote

let escape = slosh _

let dddnormal = ordinary | hash | quote | escape | white | newline
let dddspecial = dddnormal | dquote dddnormal | dquote dquote dddnormal

let qqqnormal = ordinary | hash | dquote | escape | white | newline
let qqqspecial = qqqnormal | quote qqqnormal | quote quote qqqnormal

let raw_dddnormal = ordinary | hash | quote | slosh | white | newline
let raw_dddspecial = raw_dddnormal | dquote raw_dddnormal | dquote dquote raw_dddnormal

let raw_qqqnormal = ordinary | hash | dquote | slosh | space | newline
let raw_qqqspecial = raw_qqqnormal | quote raw_qqqnormal | quote quote raw_qqqnormal

let qstring = (ordinary | hash | dquote | escape | white) * quote
let dstring = (ordinary | hash | quote | escape | white) * dquote
let qqqstring = qqqspecial * qqq
let dddstring = dddspecial * ddd

let raw = 'r' | 'R'
let see = 'c' | 'C'
let rqc = raw see | see raw

let raw_qstring = (ordinary | hash | dquote | escape | white) * quote
let raw_dstring =  (ordinary | hash | quote | escape | white) * dquote

let raw_qqqstring = raw_qqqspecial * qqq
let raw_dddstring = raw_dddspecial * ddd

let not_hash_or_newline = ordinary | quote | dquote | white | slosh
let not_newline = not_hash_or_newline | hash
let quoted_filename = dquote (ordinary | hash | quote | white | slosh)+ dquote

(* ====================== PARSERS ============================ *)
(* string lexers *)

(* ----------- BASIC STRING -----------------------------------*)

rule parse_qstring state ctor = parse
| qstring {
      state#inbody;
      ctor (
        state#get_srcref lexbuf,
        state#decode decode_qstring (lexeme lexbuf)
      )
  }
| _ {
    [ERRORTOKEN (
      state#get_srcref lexbuf,
      "' string"
    )]
  }

and parse_dstring state ctor = parse
| dstring {
      state#inbody;
      ctor (
        state#get_srcref lexbuf,
        state#decode decode_dstring (lexeme lexbuf)
      )
  }
| _ {
    state#inbody;
    [ERRORTOKEN (
      state#get_srcref lexbuf,
      "\" string"
    )]
  }

and parse_qqqstring state ctor = parse
| qqqstring {
      state#inbody;
      ctor (
        state#get_srcref lexbuf,
        state#decode decode_qqqstring (lexeme lexbuf)
      )
  }
| _ {
    state#inbody;
    [ERRORTOKEN (
      state#get_srcref lexbuf,
      "''' string"
    )]
  }

and parse_dddstring state ctor = parse
| dddstring {
      state#inbody;
       ctor (
        state#get_srcref lexbuf,
        state#decode decode_dddstring (lexeme lexbuf)
      )
  }
| _ {
    state#inbody;
    [ERRORTOKEN (
      state#get_srcref lexbuf,
      "\"\"\" string"
    )]
  }

(* ----------- RAW STRING -----------------------------------*)
and parse_raw_qstring state ctor = parse
| raw_qstring {
      state#inbody;
      ctor (
        state#get_srcref lexbuf,
        state#decode decode_raw_qstring (lexeme lexbuf)
      )
  }
| _ {
    state#inbody;
    [ERRORTOKEN (
     state#get_srcref lexbuf,
    "raw ' string")]
  }

and parse_raw_dstring state ctor = parse
| raw_dstring {
      state#inbody;
      ctor (
        state#get_srcref lexbuf,
        state#decode decode_raw_dstring (lexeme lexbuf)
      )
  }
| _ {
    state#inbody;
    [ERRORTOKEN (
      state#get_srcref lexbuf,
        "raw \" string"
    )]
  }

and parse_raw_qqqstring state ctor = parse
| raw_qqqstring {
      state#inbody;
      ctor (
        state#get_srcref lexbuf,
        state#decode decode_raw_qqqstring (lexeme lexbuf)
      )
  }
| _ { state#inbody;
  [ERRORTOKEN (
    state#get_srcref lexbuf,
    "raw ''' string")] }

and parse_raw_dddstring state ctor = parse
| raw_dddstring {
      state#inbody;
      ctor (
        state#get_srcref lexbuf,
        state#decode decode_raw_dddstring (lexeme lexbuf)
      )
  }
| _ {
     [ERRORTOKEN (
       state#get_srcref lexbuf,
       lexeme lexbuf)
     ]
   }

and parse_hashbang state = parse
| not_newline * newline {
    begin
      state#newline lexbuf;
      let lex = lexeme lexbuf in
      let n = String.length lex in
      [COMMENT_NEWLINE  (String.sub lex 0 (n-1))]
    end
  }
| _ { [ERRORTOKEN (
        state#get_srcref lexbuf,
  lexeme lexbuf)] }

and parse_C_comment state = parse
| "/*" {
      state#append_comment (lexeme lexbuf);
      state#incr_comment;
      parse_C_comment state lexbuf
  }
| newline {
      state#newline lexbuf;
      state#append_comment (lexeme lexbuf);
      parse_C_comment state lexbuf
  }
| "*/" {
      state#append_comment (lexeme lexbuf);
      state#decr_comment;
      if state#comment_level > 0
      then parse_C_comment state lexbuf
      else ()
      ;
      state#inbody
  }
| _ {
      state#append_comment (lexeme lexbuf);
      parse_C_comment state lexbuf
  }

and parse_line state = parse
| not_newline * (newline | eof)
  {
    state#newline lexbuf;
    lexeme lexbuf
  }

and parse_preprocessor state start_location start_position = parse
| ( not_newline* slosh space* newline)* not_newline* newline
| ( not_newline* hash space* newline) (not_hash_or_newline not_newline* newline)+
  {
    let toks = handle_preprocessor state lexbuf
      (lexeme lexbuf) pre_flx_lex start_location start_position
    in
    toks
  }


and pre_flx_lex state = parse
(* eof is not eaten up, so parent will find eof and emit ENDMARKER *)
| "//" not_newline * (newline | eof) {
      state#newline lexbuf;
      let lex = lexeme lexbuf in
      let n = String.length lex in
      [COMMENT_NEWLINE  (String.sub lex 2 (n-3))]
  }

| "/*" {
      state#set_comment (lexeme lexbuf);
      parse_C_comment state lexbuf;
      [COMMENT (state#get_comment)]
  }

| int_lit {
      state#inbody;
      let sr = state#get_srcref lexbuf in
      let s = lexeme lexbuf in
      let n = String.length s in
      let converter, first =
        if n>1 && s.[0]='0'
        then
          match s.[1] with
          | 'b' | 'B' -> binbig_int_of_string,2
          | 'o' | 'O' -> octbig_int_of_string,2
          | 'd' | 'D' -> decbig_int_of_string,2
          | 'x' | 'X' -> hexbig_int_of_string,2
          | _         -> decbig_int_of_string,0
        else decbig_int_of_string,0
      in
      let k = ref (n-1) in
      let t =
        if n >= 2 && s.[n-2]='i' && s.[n-1]='8'
        then (k:=n-2; "int8")
        else if n >= 2 && s.[n-2]='u' && s.[n-1]='8'
        then (k:=n-2; "uint8")
        else if n >= 3 && s.[n-3]='i' && s.[n-2]='1' && s.[n-1]='6'
        then (k:=n-3; "int16")
        else if n >= 3 && s.[n-3]='u' && s.[n-2]='1' && s.[n-1]='6'
        then (k:=n-3; "uint16")

        else if n >= 3 && s.[n-3]='i' && s.[n-2]='3' && s.[n-1]='2'
        then (k:=n-3; "int32")
        else if n >= 3 && s.[n-3]='u' && s.[n-2]='3' && s.[n-1]='2'
        then (k:=n-3; "uint32")

        else if n >= 3 && s.[n-3]='i' && s.[n-2]='6' && s.[n-1]='4'
        then (k:=n-3; "int64")
        else if n >= 3 && s.[n-3]='u' && s.[n-2]='6' && s.[n-1]='4'
        then (k:=n-3; "uint64")

        else begin
          let sign = ref "" in
          let typ = ref "int" in
          begin try while !k>first do
            (match s.[!k] with
            | 'u' | 'U' -> sign := "u"
            | 't' | 'T' -> typ := "tiny"
            | 's' | 'S' -> typ := "short"
            | 'i' | 'I' -> typ := "int"
            | 'l' | 'L' ->
              typ :=
                if !typ = "long" then "vlong" else "long"
            | 'v' | 'V' -> typ := "vlong"
            | _ -> raise Not_found
            );
            decr k
          done with _ -> () end;
          incr k;
          !sign ^ !typ
        end
      in
      let d = String.sub s first (!k-first) in
      let v = (converter d) in
        [INTEGER (sr, t, v)]
  }

| floating_literal {
    state#inbody;
    let str = lexeme lexbuf in
    let n = String.length str in
    let last_char = str.[n-1] in
    match last_char with
    | 'l'|'L' ->
      [FLOAT (state#get_srcref lexbuf,"ldouble", strip_us (String.sub str 0 (n-1)))]
    | 'f'|'F' ->
      [FLOAT (state#get_srcref lexbuf,"float",strip_us (String.sub str 0 (n-1)))]
    | _ ->
      [FLOAT (state#get_srcref lexbuf,"double",strip_us str)]
  }

(* Python strings *)
| quote  { state#inbody; parse_qstring state (fun x -> [STRING x]) lexbuf }
| qqq    { state#inbody; parse_qqqstring state (fun x -> [STRING x]) lexbuf }
| dquote { state#inbody; parse_dstring state (fun x -> [STRING x]) lexbuf }
| ddd    { state#inbody; parse_dddstring state (fun x -> [STRING x]) lexbuf }

(* Python raw strings *)
| ('r'|'R') quote  { state#inbody; parse_raw_qstring state (fun x -> [STRING x]) lexbuf }
| ('r'|'R') qqq    { state#inbody; parse_raw_qqqstring state (fun x -> [STRING x]) lexbuf }
| ('r'|'R') dquote { state#inbody; parse_raw_dstring state (fun x -> [STRING x]) lexbuf }
| ('r'|'R') ddd    { state#inbody; parse_raw_dddstring state (fun x -> [STRING x]) lexbuf }


(* C strings: type char*  *)
| ('c'|'C') quote  { state#inbody; parse_qstring state (fun x -> [CSTRING x]) lexbuf }
| ('c'|'C') qqq    { state#inbody; parse_qqqstring state (fun x -> [CSTRING x]) lexbuf }
| ('c'|'C') dquote { state#inbody; parse_dstring state (fun x -> [CSTRING x]) lexbuf }
| ('c'|'C') ddd    { state#inbody; parse_dddstring state (fun x -> [CSTRING x]) lexbuf }

(* raw C strings: type char*  *)
| rqc quote  { state#inbody; parse_raw_qstring state (fun x -> [CSTRING x]) lexbuf }
| rqc qqq    { state#inbody; parse_raw_qqqstring state (fun x -> [CSTRING x]) lexbuf }
| rqc dquote { state#inbody; parse_raw_dstring state (fun x -> [CSTRING x]) lexbuf }
| rqc ddd    { state#inbody; parse_raw_dddstring state (fun x -> [CSTRING x]) lexbuf }


(* Format strings *)
| ('f'|'F') quote  { state#inbody; parse_qstring state (fun x -> [FSTRING x]) lexbuf }
| ('f'|'F') qqq    { state#inbody; parse_qqqstring state (fun x -> [FSTRING x]) lexbuf }
| ('f'|'F') dquote { state#inbody; parse_dstring state (fun x -> [FSTRING x]) lexbuf }
| ('f'|'F') ddd    { state#inbody; parse_dddstring state (fun x -> [FSTRING x]) lexbuf }

(* interpolated strings *)
| ('q'|'Q') quote  { state#inbody; parse_qstring state (fun x -> [QSTRING x]) lexbuf }
| ('q'|'Q') qqq    { state#inbody; parse_qqqstring state (fun x -> [QSTRING x]) lexbuf }
| ('q'|'Q') dquote { state#inbody; parse_dstring state (fun x -> [QSTRING x]) lexbuf }
| ('q'|'Q') ddd    { state#inbody; parse_dddstring state (fun x -> [QSTRING x]) lexbuf }

(* wide strings *)
| ('w' | 'W') quote  { state#inbody; parse_qstring state (fun x -> [WSTRING x]) lexbuf }
| ('w' | 'W') qqq    { state#inbody; parse_qqqstring state (fun x -> [WSTRING x]) lexbuf }
| ('w' | 'W') dquote { state#inbody; parse_dstring state (fun x -> [WSTRING x]) lexbuf }
| ('w' | 'W') ddd    { state#inbody; parse_dddstring state (fun x -> [WSTRING x]) lexbuf }

(* UTF32 strings *)
| ('u' | 'U') quote  { state#inbody; parse_qstring state (fun x -> [USTRING x]) lexbuf }
| ('u' | 'U') qqq    { state#inbody; parse_qqqstring state (fun x -> [USTRING x]) lexbuf }
| ('u' | 'U') dquote { state#inbody; parse_dstring state (fun x -> [USTRING x]) lexbuf }
| ('u' | 'U') ddd    { state#inbody; parse_dddstring state (fun x -> [USTRING x]) lexbuf }

(* keyword strings *)
| ('k' | 'K') quote  { state#inbody; parse_qstring state (fun x -> [USER_KEYWORD x]) lexbuf }
| ('k' | 'K') qqq    { state#inbody; parse_qqqstring state (fun x -> [USER_KEYWORD x]) lexbuf }
| ('k' | 'K') dquote { state#inbody; parse_dstring state (fun x -> [USER_KEYWORD x]) lexbuf }
| ('k' | 'K') ddd    { state#inbody; parse_dddstring state (fun x -> [USER_KEYWORD x]) lexbuf }

(* name strings *)
| ('n' | 'N') quote  { state#inbody; parse_qstring state (fun x -> [NAME x]) lexbuf }
| ('n' | 'N') qqq    { state#inbody; parse_qqqstring state (fun x -> [NAME x]) lexbuf }
| ('n' | 'N') dquote { state#inbody; parse_dstring state (fun x -> [NAME x]) lexbuf }
| ('n' | 'N') ddd    { state#inbody; parse_dddstring state (fun x -> [NAME x]) lexbuf }


(* this MUST be after strings, so raw strings take precedence
  over identifiers, eg r'x' is a string, not an identifier,
  but x'x' is an identifier .. yucky ..
*)
| identifier {
      state#inbody;
      let s = lexeme lexbuf in
      let s' = Flx_id.utf8_to_ucn s in
      let src = state#get_srcref lexbuf in
      try [
        let keywords = state#get_keywords in
        let n = String.length s' in
        if n >= Array.length keywords then raise Not_found;
        let keywords = keywords.(n) in
        (List.assoc s' keywords) (src,s')
      ]
      with Not_found ->
      [Flx_keywords.map_flx_keywords src s']
  }

(* whitespace *)
| white + {
      (* we do NOT say 'inbody' here: we want to accept
         #directives with leading spaces
      *)
      let spaces=lexeme lexbuf in
      let column = ref 0 in
      let n = String.length spaces in
      for i=0 to n-1 do match spaces.[i] with
        | '\t' -> column := ((!column + 8) / 8) * 8
        | ' ' -> incr column
        | _ -> raise (Failure "Error in lexer, bad white space character")
      done;
      [WHITE  (!column)]
  }

| slosh { [SLOSH] }

| symchar + {
    let s = lexeme lexbuf in
    let n = String.length s in
    let s',con,lim =
      try
        for i = 0 to n - 1 do
          if s.[i] = '/' && i+1<n then begin
            if s.[i+1] = '/' then raise (SlashSlash i);
            if s.[i+1] = '*' then raise (SlashAst i)
          end
        done;
        raise (Ok n)
      with
      | SlashSlash i -> String.sub s 0 i,`SlashSlash,i
      | SlashAst i -> String.sub s 0 i,`SlashAst,i
      | Ok i -> String.sub s 0 i,`Ok,i
    in
    let atstart = state#is_at_line_start in
    state#inbody;
    let toks = state#tokenise_symbols  lexbuf s' in
    let toks =
      match toks,atstart with
      | [HASH _],true ->
        let x = state#get_loc in
        let y = lexbuf.Lexing.lex_curr_p in
        parse_preprocessor state
          { x with buf_pos = x.buf_pos }
          { y with Lexing.pos_fname = y.Lexing.pos_fname }
          lexbuf
      | [HASHBANG _ | HASHBANGSLASH _ ],true  ->
        (*
        print_endline "IGNORING HASHBANG";
        *)
        parse_hashbang state lexbuf
      | _ when con = `SlashSlash ->
        (*
        print_endline "EMBEDDED //";
        *)
        let lead = String.sub s (lim+2) (n-lim-2) in
        let lex = parse_line state lexbuf in
        let m = String.length lex in
        toks @ [COMMENT_NEWLINE  (lead ^ String.sub lex 0 (m-1))]

      | _ when con = `SlashAst ->
        (*
        print_endline "EMBEDDED /*";
        *)
        (* NOTE THIS WILL NOT HANDLE /**/ or any other
          sequence x/*xxxx*/ where the x's are special
          In particular x/***************/ will fail.
        *)
        let lead = String.sub s (lim+2) (n-lim-2) in
        state#set_comment lead;
        parse_C_comment state lexbuf;
        toks @ [COMMENT (state#get_comment)]

      | _ -> toks
    in toks
  }

(* end of line *)
| newline {
      state#newline lexbuf;
      [NEWLINE ]
  }

(* end of file *)
| eof {
  if state#get_condition = `Subscan then [ENDMARKER] else
  if state#condition_stack_length <> 1
  then
    let sr = state#get_srcref lexbuf in
    let sr = Flx_srcref.slift sr in
    Flx_exceptions.clierr sr "Unmatched #if at end of file"
  else
    [ENDMARKER]
  }

(* Anything else is an error *)
| _ {
    state#inbody;
    [
      ERRORTOKEN
      (
        state#get_srcref lexbuf, lexeme lexbuf
      )
    ]
  }

{
}
