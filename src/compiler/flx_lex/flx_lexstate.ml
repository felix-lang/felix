open Flx_util
open Flx_token
open Flx_string
open Big_int
open Flx_exceptions
open Flx_ast
open List

let special_tokens =
  [
    ("$",(fun (sr,s)-> DOLLAR sr));
    ("?",(fun (sr,s)-> QUEST sr));
    ("!",(fun (sr,s)-> EXCLAMATION sr));
    ("(",(fun (sr,s)-> LPAR sr));
    (")",(fun (sr,s)-> RPAR sr));
    ("[",(fun (sr,s)-> LSQB sr));
    ("]",(fun (sr,s)-> RSQB sr));
    ("{",(fun (sr,s)-> LBRACE sr));
    ("}",(fun (sr,s)-> RBRACE sr));
    (":",(fun (sr,s)-> COLON sr));
    (",",(fun (sr,s)-> COMMA sr));
    (";",(fun (sr,s)-> SEMI sr));
    ("+",(fun (sr,s)-> PLUS sr));
    ("-",(fun (sr,s)-> MINUS sr));
    ("*",(fun (sr,s)-> STAR sr));
    ("/",(fun (sr,s)-> SLASH sr));
    ("|",(fun (sr,s)-> VBAR sr));
    ("&",(fun (sr,s)-> AMPER sr));
    ("<",(fun (sr,s)-> LESS sr));
    (">",(fun (sr,s)-> GREATER sr));
    ("=",(fun (sr,s)-> EQUAL sr));
    (".",(fun (sr,s)-> DOT sr));
    ("%",(fun (sr,s)-> PERCENT sr));
    ("`",(fun (sr,s)-> BACKQUOTE sr));
    ("~",(fun (sr,s)-> TILDE sr));
    ("^",(fun (sr,s)-> CIRCUMFLEX sr));
    ("#",(fun (sr,s)-> HASH sr));
    ("$$",(fun (sr,s)-> DOLLARDOLLAR sr));
    ("&<",(fun (sr,s)-> ANDLESS sr));
    ("&>",(fun (sr,s)-> ANDGREATER sr));
    ("==",(fun (sr,s)-> EQEQUAL sr));
    ("!=",(fun (sr,s)-> NOTEQUAL sr));
    ("<=",(fun (sr,s)-> LESSEQUAL sr));
    (">=",(fun (sr,s)-> GREATEREQUAL sr));
    ("<<",(fun (sr,s)-> LEFTSHIFT sr));
    (">>",(fun (sr,s)-> RIGHTSHIFT sr));
    ("**",(fun (sr,s)-> STARSTAR sr));
    ("<:",(fun (sr,s)-> LESSCOLON sr));
    (":>",(fun (sr,s)-> COLONGREATER sr));
    ("..",(fun (sr,s)-> DOTDOT sr));
    ("::",(fun (sr,s)-> COLONCOLON sr));
    ("++",(fun (sr,s)-> PLUSPLUS sr));
    ("--",(fun (sr,s)-> MINUSMINUS sr));
    ("+=",(fun (sr,s)-> PLUSEQUAL sr));
    ("-=",(fun (sr,s)-> MINUSEQUAL sr));
    ("*=",(fun (sr,s)-> STAREQUAL sr));
    ("/=",(fun (sr,s)-> SLASHEQUAL sr));
    ("%=",(fun (sr,s)-> PERCENTEQUAL sr));
    ("^=",(fun (sr,s)-> CARETEQUAL sr));
    ("|=",(fun (sr,s)-> VBAREQUAL sr));
    ("&=",(fun (sr,s)-> AMPEREQUAL sr));
    ("~=",(fun (sr,s)-> TILDEEQUAL sr));
    (":=",(fun (sr,s)-> COLONEQUAL sr));
    ("->",(fun (sr,s)-> RIGHTARROW sr));
    ("=>",(fun (sr,s)-> EQRIGHTARROW sr));
    ("<-",(fun (sr,s)-> LEFTARROW sr));
    ("[|",(fun (sr,s)-> LSQBAR sr));
    ("|]",(fun (sr,s)-> RSQBAR sr));
    ("&&",(fun (sr,s)-> AMPERAMPER sr));
    ("||",(fun (sr,s)-> VBARVBAR sr));
    ("\\&",(fun (sr,s)-> SLOSHAMPER sr));
    ("\\|",(fun (sr,s)-> SLOSHVBAR sr));
    ("\\^",(fun (sr,s)-> SLOSHCIRCUMFLEX sr));
    ("#!",(fun (sr,s)-> HASHBANG sr));
    ("<<=",(fun (sr,s)-> LEFTSHIFTEQUAL sr));
    (">>=",(fun (sr,s)-> RIGHTSHIFTEQUAL sr));
    ("<->",(fun (sr,s)-> LEFTRIGHTARROW sr));
    ("&==",(fun (sr,s)-> ANDEQEQUAL sr));
    ("&!=",(fun (sr,s)-> ANDNOTEQUAL sr));
    ("&<=",(fun (sr,s)-> ANDLESSEQUAL sr));
    ("&>=",(fun (sr,s)-> ANDGREATEREQUAL sr));
    ("...",(fun (sr,s)-> DOTDOTDOT sr));
    ("-->",(fun (sr,s)-> LONGRIGHTARROW sr));
    ("=>#",(fun (sr,s)-> PARSE_ACTION sr));
    ("#!/",(fun (sr,s)-> HASHBANGSLASH sr));
  ]

let mk_std_tokens () =
  let tk = Array.make 4 [] in
  iter  (fun (s,f) ->
    let n = String.length s in
    assert (n >0 && n <= 3);
    tk.(n) <- (s,f) :: tk.(n)
  )
  special_tokens
  ;
  tk

exception Duplicate_macro of string

class comment_control =
  object (self)
    val mutable nesting_level = 0
    val mutable text = ""

    method set_text s = text <- s; nesting_level <- 1
    method append s = text <- text ^ s
    method get_comment = text

    method incr = nesting_level <- nesting_level + 1
    method decr = nesting_level <- nesting_level - 1
    method get_nesting_level = nesting_level
  end

exception Found_file of string

type condition_t = [
 | `Processing
 | `Skip_to_endif
 | `Skip_to_else
 | `Subscan
]

type location = {
    mutable buf_pos : int;
    mutable last_buf_pos : int;
    mutable line_no : int;
    mutable original_line_no : int;
}

class file_control
  (filename' : string)
  (basedir': string)
  (incdirs': string list)
=
  object(self)
    val mutable loc : location = { buf_pos = 0; last_buf_pos = 0; line_no = 1; original_line_no = 1; }
    method get_loc = loc
    method set_loc loc' = loc <- loc'

    (* this is the physical filename *)
    val original_filename = filename'
    val incdirs = incdirs'
    val basedir = basedir'

    (* this is the generator file name, can be set with #line directive *)
    val mutable filename = filename'
    val mutable condition:condition_t list = [`Processing]
    val macros : (string,string list * Flx_token.token list) Hashtbl.t = Hashtbl.create 97

    method incr_lex_counters lexbuf =
      loc.line_no <- loc.line_no + 1;
      loc.original_line_no <- loc.original_line_no + 1;
      loc.last_buf_pos <- loc.buf_pos;
      loc.buf_pos <- Lexing.lexeme_end lexbuf

    method set_buf_pos x = loc.buf_pos <- x
    method get_buf_pos = loc.buf_pos
    method get_srcref lexbuf =
      filename,
      loc.line_no,
      Lexing.lexeme_start lexbuf - loc.buf_pos + 1,
      Lexing.lexeme_end lexbuf - loc.buf_pos

    method get_physical_srcref lexbuf =
      original_filename,
      loc.original_line_no,
      Lexing.lexeme_start lexbuf - loc.buf_pos + 1,
      Lexing.lexeme_end lexbuf - loc.buf_pos

    method incr n =
      loc.line_no <- loc.line_no + n;
      loc.original_line_no <- loc.original_line_no + n

    method set_line n lexbuf =
      loc.line_no <- n;
      loc.last_buf_pos <- loc.buf_pos;
      loc.buf_pos <- Lexing.lexeme_end lexbuf;
      (* this is a hack .. *)
      loc.original_line_no <- loc.original_line_no + 1

    method set_filename f = filename <- f
    method get_relative f =
      let fn = Filename.concat basedir f in
      if not (Sys.file_exists fn) then
        failwith ("Relative include file \""^f^ "\" not found in "^basedir)
      else fn

    method get_absolute f =
      try
        List.iter
        (fun d ->
          let f = Filename.concat d f in
          if Sys.file_exists f
          then raise (Found_file f)
        )
        incdirs
        ;
        failwith ("Library File <" ^ f ^ "> not found in path")
      with Found_file s -> s

    method store_macro name params body =
      Hashtbl.add macros name (params,body)

    method undef_macro name = Hashtbl.remove macros name

    method get_macro name =
      try Some (Hashtbl.find macros name)
      with Not_found -> None

    method get_macros = macros

    method get_incdirs = incdirs
    method get_condition = List.hd condition
    method push_condition c =  condition <- (c :: condition)
    method pop_condition = condition <- List.tl condition
    method set_condition c = condition <- (c :: List.tl condition)
    method condition_stack_length = List.length condition
  end

class lexer_state filename basedir incdirs cache_dir expand_expr' =
  object (self)
    method get_cache_dir : string option = cache_dir
    val expand_expr: string -> expr_t -> expr_t = expand_expr'

    val mutable include_files: string list = []

    val comment_ctrl = new comment_control
    val file_ctrl = new file_control filename basedir incdirs
    val mutable at_line_start = true

    val mutable keywords:
      (string * (srcref * string -> Flx_token.token)) list array
      = [| [] |]

    val mutable symbols:
      (string * (srcref * string -> Flx_token.token)) list array
      = mk_std_tokens ()

    val nonterminals:
      (string, (token list * ast_term_t) list) Hashtbl.t
      = Hashtbl.create 97

    method get_expand_expr = expand_expr
    method get_include_files = include_files
    method add_include_file f = include_files <- f :: include_files

    method get_symbols = symbols
    method get_nonterminals = nonterminals

    method is_at_line_start = at_line_start

    method inbody = at_line_start <- false
    method get_srcref lexbuf = file_ctrl#get_srcref lexbuf
    method get_physical_srcref lexbuf = file_ctrl#get_physical_srcref lexbuf
    method string_of_srcref lexbuf =
      match self#get_srcref lexbuf with
      (filename, lineno, scol,ecol) ->
      "File \"" ^ filename ^ "\"" ^
      ", Line " ^ string_of_int lineno ^
      ", Columns " ^ string_of_int scol ^
      "-" ^ string_of_int ecol

    (* comments *)
    method comment_level = comment_ctrl#get_nesting_level
    method incr_comment = comment_ctrl#incr
    method decr_comment = comment_ctrl#decr

    method set_comment text = comment_ctrl#set_text text
    method append_comment text = comment_ctrl#append text
    method get_comment = comment_ctrl#get_comment

    (* line counting *)
    method newline lexbuf =
      at_line_start <- true;
      file_ctrl#incr_lex_counters lexbuf

    (* string decoders *)
    method decode decoder (s : string) : string =
      let lfcount s =
        let n = ref 0 in
        for i = 0 to (String.length s) - 1 do
          if s.[i] = '\n' then incr n
        done;
        !n
      in
        file_ctrl#incr (lfcount s);
        decoder s

    method set_line n lexbuf =
      file_ctrl#set_line n lexbuf;
      at_line_start <- true

    method set_filename f = file_ctrl#set_filename f

    method get_loc = file_ctrl#get_loc
    method set_loc loc' = file_ctrl#set_loc loc'
    method get_incdirs = file_ctrl#get_incdirs
    method get_relative f = file_ctrl#get_relative f
    method get_absolute f = file_ctrl#get_absolute f

    method get_condition = file_ctrl#get_condition
    method push_condition c = file_ctrl#push_condition c
    method pop_condition = file_ctrl#pop_condition
    method set_condition c = file_ctrl#set_condition c
    method condition_stack_length = file_ctrl#condition_stack_length

    method store_macro name parms body = file_ctrl#store_macro name parms body
    method undef_macro name = file_ctrl#undef_macro name
    method get_macro name = file_ctrl#get_macro name
    method get_macros = file_ctrl#get_macros

    method add_macros (s:lexer_state) =
      let h = self#get_macros in
      Hashtbl.iter
      (fun k v ->
        if Hashtbl.mem h k
        then raise (Duplicate_macro k)
        else Hashtbl.add h k v
      )
      s#get_macros
      ;

     (* append new keywords *)
     let new_keywords = s#get_keywords in
     let n = Array.length new_keywords in
     if n > Array.length keywords then begin
       let old_keywords = keywords in
       keywords <- Array.make n [];
       Array.blit old_keywords 0 keywords 0 (Array.length old_keywords)
     end;
     for i = 0 to Array.length new_keywords - 1 do
       keywords.(i) <- new_keywords.(i) @ keywords.(i)
     done
     ;

     (* append new symbols *)
     let new_symbols = s#get_symbols in
     let n = Array.length new_symbols in
     if n > Array.length symbols then begin
       let old_symbols = symbols in
       symbols <- Array.make n [];
       Array.blit old_symbols 0 symbols 0 (Array.length old_symbols)
     end;
     for i = 0 to Array.length new_symbols - 1 do
       symbols.(i) <- new_symbols.(i) @ symbols.(i)
     done
     ;

     Hashtbl.iter
     (fun k ls ->
       let old = try Hashtbl.find nonterminals k with Not_found -> [] in
       Hashtbl.replace nonterminals k (ls @ old)
     )
     s#get_nonterminals

    method get_keywords = keywords

    method adjust_keyword_array n =
      let m = Array.length keywords in
      if m <= n then begin
        let a = Array.make (n+1) [] in
        Array.blit keywords 0 a 0 m;
        keywords <- a
      end

    method adjust_symbol_array n =
      let m = Array.length symbols in
      if m <= n then begin
        let a = Array.make (n+1) [] in
        Array.blit symbols 0 a 0 m;
        symbols <- a
      end

    method add_keyword (s:string) =
      let n = String.length s in
      self#adjust_keyword_array n;
      let elt = s,(fun (sr,_) -> USER_KEYWORD (sr,s)) in
      keywords.(n) <- elt :: keywords.(n)

    method add_nonterminal (s:string) (sr:range_srcref) (toks: Flx_token.token list) (term:ast_term_t) =
      let productions = try Hashtbl.find nonterminals s with Not_found -> [] in
      Hashtbl.replace nonterminals s ((toks,term)::productions)

    method tokenise_symbols lexbuf (s:string) : token list =
      (* temporary hack *)
      let sr = self#get_srcref lexbuf in
      let rec tk tks s =
        let m = String.length s in
        let rec aux n =
          if n = 0 then (* cannot match even first char *)
           tk (ERRORTOKEN (sr,String.sub s 0 1)::tks) (String.sub s 1 (m-1))
          else
          let f =
            try Some (assoc (String.sub s 0 n) symbols.(n))
            with Not_found -> None
          in
          match f with
          | None -> aux (n-1)
          | Some f ->
            (* next token *)
            tk (f (sr,String.sub s 0 n) :: tks) (String.sub s n (m-n))
        in
        let n = String.length s in
        if n = 0 then rev tks
        else aux (min n (Array.length symbols - 1))
      in
        tk [] s
end
