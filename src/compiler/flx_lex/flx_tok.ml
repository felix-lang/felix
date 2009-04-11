open Flx_ast
open Flx_exceptions
open List
open Flx_srcref
open Flx_token

let dyphack (ls : ( 'a * string) list) : 'a =
  match ls with
  | [x,_] -> x
  | _ -> failwith "Dypgen parser failed"

let print_pre_token t =
  let emit t = print_string (Flx_prelex.string_of_token t) in
    begin match t with
    | COMMENT_NEWLINE s ->
      print_endline ("//" ^ s);

    | NEWLINE ->
      print_endline ""

    | ENDMARKER -> print_endline "<<EOF>>"
    | _ -> emit t
    end;
    flush stdout

let print_pre_tokens ts =
  if (length ts) = 0
  then print_string "<Empty pretoken list>";
  print_string "   1: ";
  iter print_pre_token ts

let print_tokens ts =
  let lineno = ref 0 in
  let indent = ref 0 in
  let emit t =
    print_string ((Flx_prelex.string_of_token t) ^ " ")
  and emit_eol t =
    print_endline t;
    let s' = "    " ^ (string_of_int !lineno) in
    let n = String.length s' in
    print_string ((String.sub s' (n-4) 4) ^ ": ");
    for i=0 to !indent -1 do print_string "  " done
  in
  let print_token t =
    begin match t with
    | NEWLINE  ->
      emit_eol ("//")
    | LBRACE _ ->
      incr indent;
      emit_eol "  {"
    | RBRACE _ ->
      decr indent;
      emit_eol "}"
    | ENDMARKER -> emit_eol "#<<EOF>>"
    | _ -> emit t
    end;
    flush stdout
  in
    iter print_token ts
;;

class tokeniser t =
  object(self)
    val mutable tokens = []
    val mutable tokens_copy = []
    val mutable current_token_index = 0
    initializer tokens  <- t; tokens_copy <- t

    method token_peek (dummy :Lexing.lexbuf) =
      hd tokens

    method token_src (dummy :Lexing.lexbuf) =
      if List.length tokens = 0 then begin
        print_endline "Tokeniser: Run out of tokens!";
        ENDMARKER
      end else
      let tmp = hd tokens in
      tokens <- tl tokens;
      current_token_index <- current_token_index + 1;
      tmp

    method put_back (x:token) =
      tokens <- x :: tokens;
      current_token_index <- current_token_index - 1

    method get_loc =
      let token = nth tokens_copy current_token_index in
      slift (Flx_prelex.src_of_token token)

    method report_syntax_error =
      print_endline "";
      print_endline "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
      let n = length tokens_copy in
      let first = max 0 (current_token_index - 20)
      and last = min (n-1) (current_token_index + 20)
      and slist = ref [] in
      for i = first to current_token_index-1 do
        slist := concat [!slist; [nth tokens_copy i]]
      done;
      print_tokens !slist;
      print_endline "";

      let j =
        begin
          if length tokens_copy = current_token_index
          then begin
            print_string "Unexpected End Of File";
            current_token_index - 1
          end else begin
            print_string "Syntax Error before token ";
            print_string (string_of_int current_token_index);
            current_token_index
          end
        end
      in
      let token = nth tokens_copy j in
      let sr = ref (Flx_prelex.src_of_token token) in
      let file,line,scol,ecol = !sr in
      if line <> 0 or j = 0 then
        print_endline
        (
          " in " ^ file ^
          ", line " ^ string_of_int line ^
          " col " ^ string_of_int scol
        )
      else begin
        let token = nth tokens_copy (j-1) in
        sr := Flx_prelex.src_of_token token;
        let file,line,scol,ecol = !sr in
        print_endline
        (
          " in " ^ file ^
          ", after line " ^ string_of_int line ^
          " col " ^ string_of_int scol
        )
      end;

      slist := [];
      for i = current_token_index to last do
        slist := concat [!slist; [nth tokens_copy i]]
      done;
      print_tokens !slist;
      print_endline "";
      print_endline "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
      flush stdout;
      (*
      clierr (slift (!sr)) "Syntax Error";
      ()
      *)
  end;;

type 'a parser_t =
  (Lexing.lexbuf  -> token) ->
  Lexing.lexbuf ->
  'a

let parse_tokens (parser:'a parser_t) (tokens: token list) =
  let toker = (new tokeniser tokens) in
  try
    parser (toker#token_src) (Lexing.from_string "dummy" )
  with
  | Flx_exceptions.ClientError _
  | Flx_exceptions.ClientError2 _
  | Flx_exceptions.ClientErrorn _ as x ->
    (*
    print_endline ("got client error from parse..");
    *)
    toker#report_syntax_error;
    raise x

  | Flx_exceptions.ParseError _ as x ->
    (*
    print_endline ("got ParseError from parse..");
    *)
    toker#report_syntax_error;
    raise x

  | Flx_exceptions.RDP_match_fail _ as x ->
    (*
    print_endline ("got RDP_match_fail from parse..");
    *)
    toker#report_syntax_error;
    raise x

  | exn ->
    print_endline "Got unknown error from parse..";
    print_endline (Printexc.to_string exn);
    toker#report_syntax_error;
    raise (Flx_exceptions.ParseError "Parsing Tokens")
