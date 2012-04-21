(* Lexer for Scheme.  *)

open Ocs_types
open Ocs_error

type token =
    Leof
  | Lopenv			(* #( *)
  | Lunqsplice			(* ,@ *)
  | Lident of string
  | Lstring of string
  | Lnumber of sval
  | Lbool of sval
  | Lchar of sval
  | Ltoken of char

type lexer = {
  l_port : Ocs_port.port;
  l_buf : Buffer.t;
  l_name : string;
  mutable l_line : int
}

let make_lexer port name =
  { l_port = port;
    l_buf = Buffer.create 512;
    l_name = name;
    l_line = 0 }
;;

let get_loc lex =
  (lex.l_name, lex.l_line)
;;

let lex_error lex err =
  if String.length lex.l_name = 0 then
    Error err
  else
    ErrorL (get_loc lex, err)
;;

let num_w_base lex s =
  let base =
    match s.[0] with
      'B' | 'b' -> 2
    | 'D' | 'd' -> 10
    | 'O' | 'o' -> 8
    | 'X' | 'x' -> 16
    | _ -> raise (lex_error lex "invalid character literal")
  and n = String.length s
  in
    let rec scn v i =
      if i >= n then v
      else
	match s.[i] with
	  '0' .. '9' as c when (int_of_char c) - (int_of_char '0') < base ->
	    scn (v * base + (int_of_char c) - (int_of_char '0')) (i + 1)
	| 'a' .. 'f' as c when base = 16 ->
	    scn (v * base + (int_of_char c) - (int_of_char 'a') + 10) (i + 1)
	| 'A' .. 'F' as c when base = 16 ->
	    scn (v * base + (int_of_char c) - (int_of_char 'A') + 10) (i + 1)
	| _ -> v	(* Ignore trailing junk *)
    in
      scn 0 1
;;

let string_num_esc lex base n =
  let rec scn v i =
    if i >= n then char_of_int v
    else
      match Ocs_port.getc lex.l_port with
        Some ('0' .. '9' as c) when
	  (int_of_char c) - (int_of_char '0') < base ->
	    scn (v * base + (int_of_char c) - (int_of_char '0')) (i + 1)
      | Some ('a' .. 'f' as c) when base = 16 ->
	    scn (v * base + (int_of_char c) - (int_of_char 'a') + 10) (i + 1)
      | Some ('A' .. 'F' as c) when base = 16 ->
	    scn (v * base + (int_of_char c) - (int_of_char 'A') + 10) (i + 1)
      | Some c ->
	  Ocs_port.ungetc lex.l_port c;
	  char_of_int v
      | None -> raise (lex_error lex "unexpected eof in string literal")
  in
    scn 0 0
;;

let read_char lex =
  begin
    match Ocs_port.getc lex.l_port with
      Some c -> Buffer.add_char lex.l_buf c
    | None -> raise (lex_error lex "unexpected eof")
  end;
  let rec loop () =
    match Ocs_port.getc lex.l_port with
      Some (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c) ->
	Buffer.add_char lex.l_buf c; loop ()
    | Some c -> Ocs_port.ungetc lex.l_port c
    | None -> ()
  in
    loop ();
    let s = Buffer.contents lex.l_buf in
      if String.length s = 1 then
	Lchar (Schar s.[0])
      else
	match Ocs_char.name_to_char s with
	  Some c -> Lchar (Schar c)
	| None -> Lchar (Schar (char_of_int (num_w_base lex s)))
;;

let rec read_string lex =
  match Ocs_port.getc lex.l_port with
    Some '\"' -> Lstring (Buffer.contents lex.l_buf)
  | Some '\\' ->
      begin
	match Ocs_port.getc lex.l_port with
	  Some ('N' | 'n') ->
	    Buffer.add_char lex.l_buf '\n';
	    read_string lex
	| Some ('R' | 'r') ->
	    Buffer.add_char lex.l_buf '\r';
	    read_string lex
	| Some ('T' | 't') ->
	    Buffer.add_char lex.l_buf '\t';
	    read_string lex
	| Some ('B' | 'b') ->
	    Buffer.add_char lex.l_buf (string_num_esc lex 2 8);
	    read_string lex
	| Some ('D' | 'd') ->
	    Buffer.add_char lex.l_buf (string_num_esc lex 10 3);
	    read_string lex
	| Some ('O' | 'o') ->
	    Buffer.add_char lex.l_buf (string_num_esc lex 8 3);
	    read_string lex
	| Some ('X' | 'x') ->
	    Buffer.add_char lex.l_buf (string_num_esc lex 16 2);
	    read_string lex
	| Some ('0' .. '9' as c) ->
	    Ocs_port.ungetc lex.l_port c;
	    Buffer.add_char lex.l_buf (string_num_esc lex 10 3);
	    read_string lex
	| Some c ->
	    Buffer.add_char lex.l_buf c;
	    read_string lex
	| None ->
	    raise (lex_error lex "unexpected eof in string literal")
      end
  | Some '\n' ->
      lex.l_line <- lex.l_line + 1;
      Buffer.add_char lex.l_buf '\n';
      read_string lex
  | Some c ->
      Buffer.add_char lex.l_buf c;
      read_string lex
  | None -> raise (lex_error lex "unexpected eof in string literal")
;;

let rec read_ident lex =
  match Ocs_port.getc lex.l_port with
    Some (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '!' | '$' | '%' | '&' |
           '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' |
	   '+' | '-' | '.' | '@') as c) ->
      Buffer.add_char lex.l_buf c;
      read_ident lex
  | Some c ->
      Ocs_port.ungetc lex.l_port c;
      Lident (Buffer.contents lex.l_buf)
  | None ->
      Lident (Buffer.contents lex.l_buf)
;;

let parse_number lex =
  try
    Lnumber (Ocs_numstr.string_to_num (Buffer.contents lex.l_buf) 0)
  with
    Error err -> raise (lex_error lex err)
;;

(* When reading numbers, accept almost any characters that look
   like they may be part of a number.  Some extremely obfuscated
   inputs may be misinterpreted.  *)
let rec read_number lex =
  match Ocs_port.getc lex.l_port with
    Some (('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' | '+' | '.' |
	   '#' | '/' | '@') as c) ->
      Buffer.add_char lex.l_buf c;
      read_number lex
  | Some c ->
      Ocs_port.ungetc lex.l_port c;
      parse_number lex
  | None ->
      parse_number lex
;;

let rec tok lex =
  match Ocs_port.getc lex.l_port with
    Some c ->
      begin
	match c with
	  '\n' -> lex.l_line <- lex.l_line + 1; tok lex
	| ' ' | '\t' | '\r' | '\012' -> tok lex
	| ';' ->
	    begin
	      let rec loop () =
		match Ocs_port.getc lex.l_port with
		  Some '\n' -> lex.l_line <- lex.l_line + 1; tok lex
		| Some _ -> loop ()
		| None -> Leof
	      in
		loop ()
	    end
	| ',' ->
	    begin
	      match Ocs_port.getc lex.l_port with
		Some '@' -> Lunqsplice
	      | Some c -> Ocs_port.ungetc lex.l_port c; Ltoken ','
	      | None -> Ltoken ','
	    end
	| '#' ->
	    begin
	      match Ocs_port.getc lex.l_port with
		Some ('f' | 'F') -> Lbool Sfalse
	      | Some ('t' | 'T') -> Lbool Strue
	      | Some (('B' | 'b' | 'D' | 'd' | 'O' | 'o' | 'X' | 'x' |
	               'E' | 'e' | 'I' | 'i') as c) ->
		  Buffer.add_char lex.l_buf '#';
		  Buffer.add_char lex.l_buf c;
		  read_number lex
	      | Some '\\' -> read_char lex
	      | Some '(' -> Lopenv
	      | Some c -> Ocs_port.ungetc lex.l_port c; Ltoken '#'
	      | None -> Ltoken '#'
	    end
	| '\"' -> read_string lex
	| '+' | '-' ->
	    begin
	      match Ocs_port.getc lex.l_port with
		Some (('0' .. '9' | 'i' | 'I' | '.') as x) ->
		  Buffer.add_char lex.l_buf c;
		  Buffer.add_char lex.l_buf x;
		  read_number lex
	      | Some x ->
		  Ocs_port.ungetc lex.l_port x;
		  Lident (String.make 1 c)
	      | None -> Lident (String.make 1 c)
	    end
	| '.' ->
	    begin
	      match Ocs_port.getc lex.l_port with
		Some '.' ->
		  Buffer.add_string lex.l_buf "..";
		  read_ident lex
	      | Some ('0' .. '9' as c) ->
		  Buffer.add_char lex.l_buf '.';
		  Buffer.add_char lex.l_buf c;
		  read_number lex
	      | Some c ->
		  Ocs_port.ungetc lex.l_port c;
		  Ltoken '.'
	      | None -> Ltoken '.'
	    end
	| '0' .. '9' ->
	    Buffer.add_char lex.l_buf c;
	    read_number lex
	| 'a' .. 'z' | 'A' .. 'Z' | '!' | '$' | '%' | '&' | '*' | '/'
	  | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' ->
	    begin
	      Buffer.add_char lex.l_buf c;
	      read_ident lex
	    end
	| _ -> Ltoken c
      end
  | None -> Leof
;;

let get_tok lex =
  Buffer.clear lex.l_buf;
  tok lex
;;

