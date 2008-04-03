(* Reader of Scheme expressions.  *)

open Ocs_types
open Ocs_error
open Ocs_lex
open Ocs_sym
open Ocs_misc

let read_error lex err =
  let (file, name) = get_loc lex in
    if String.length file = 0 then
      Error err
    else
      ErrorL ((file, name), err)
;;

let show_token =
  function
    '"' -> "'\"'"
  | '\032' .. '\126' as c -> Printf.sprintf "\"%c\"" c
  | c -> Printf.sprintf "ascii %d" (int_of_char c)
;;

let rec read_item lex =
  function
    Leof -> Seof
  | Lident s -> get_symbol (String.lowercase s)
  | Lstring s -> Sstring s
  | Lnumber s | Lbool s | Lchar s -> s
  | Ltoken '(' -> read_list lex ')'
  | Ltoken '[' -> read_list lex ']'
  | Lopenv -> read_vector lex
  | Ltoken '\'' -> read_quoted lex sym_quote
  | Ltoken '`' -> read_quoted lex sym_quasiquote
  | Ltoken ',' -> read_quoted lex sym_unquote
  | Lunqsplice -> read_quoted lex sym_unquote_splicing
  | Ltoken c -> raise (read_error lex ("unexpected " ^ show_token c))

and read_list lex term =
  let rec loop r =
    match get_tok lex with
      Leof -> raise (read_error lex "unexpected eof in list")
    | Ltoken c when c = term -> make_slist Snull r
    | Ltoken '.' ->
	begin
	  let tl =
	    match get_tok lex with
	      Leof -> raise (read_error lex "unexpected eof in dotted tail")
	    | x -> read_item lex x
	  in
	    match get_tok lex with
	      Ltoken c when c = term -> make_slist tl r
	    | _ -> raise (read_error lex
			    "expected close paren after dotted tail")
	end
    | x -> loop ((read_item lex x)::r)
  in
    loop []

and read_vector lex =
  let rec loop r =
    match get_tok lex with
      Leof -> raise (read_error lex "unexpected eof in vector")
    | Ltoken ')' -> r
    | x -> loop ((read_item lex x)::r)
  in
    Svector (Array.of_list (List.rev (loop [])))

and read_quoted lex sym =
  match get_tok lex with
    Leof -> raise (read_error lex "unexpected eof")
  | x ->
      let x = read_item lex x in
	Spair { car = sym; cdr = Spair { car = x; cdr = Snull }}
;;

let read_expr lex =
  read_item lex (get_tok lex)
;;

let read_from_port p =
  read_expr (make_lexer p "")
;;

let read_from_string s =
  read_from_port (Ocs_port.string_input_port s)
;;
