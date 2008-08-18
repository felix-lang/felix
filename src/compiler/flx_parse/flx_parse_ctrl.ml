open Flx_ast
open Flx_token
open Flx_exceptions
open Flx_parse

let dyphack (ls : ( 'a * Dyp.priority) list) : 'a =
  match ls with
  | [x,_] -> x
  | _ -> failwith "Dypgen parser failed"

let parse_file
  (filename : string)
  (basedir :string)
  (include_dirs : string list)
  cache_dir
  expand_expr
  auto_imports
=
  let pre_tokens  =
    Flx_pretok.pre_tokens_of_filename
      filename
      basedir
      include_dirs
      cache_dir
      expand_expr
      auto_imports
  in
  let tokens  = Flx_lex1.translate pre_tokens in
  let hash_include_files, tokens = match tokens with
    | HASH_INCLUDE_FILES fs :: t -> fs,t
    | _ -> assert false
  in
    begin
      let toker = (new Flx_tok.tokeniser tokens) in
      let parse_tree =
      try dyphack (
        Flx_parse.compilation_unit
        (toker#token_src)
        (Lexing.from_string "dummy" )
        )
      with
      | Failure s ->
        begin
          toker#report_syntax_error;
          print_endline s;
          raise (Flx_exceptions.ParseError ("Failure \""^s^"\" Parsing File"))
        end
      | Flx_exceptions.ClientError _
      | Flx_exceptions.ClientError2 _
      | Flx_exceptions.ClientErrorn _ as x  ->
        begin
          toker#report_syntax_error;
          raise x
        end

      | Flx_exceptions.RDP_match_fail (sr1,sr2,s) as x  ->
        begin
          toker#report_syntax_error;
          clierr2 sr1 sr2 ("User Syntax Parse Error " ^ s)
        end

      | Dyp.Bad_constructor (nt,ctor1, ctor2) ->
          toker#report_syntax_error;
          let s = "Bad constructor '" ^ ctor1 ^ "' and '" ^ ctor2 ^ "' for nonterminal '" ^ nt ^"'" in
          print_endline s;
          failwith s

      | x ->
        begin
          toker#report_syntax_error;
          print_endline (Printexc.to_string x);
          raise (Flx_exceptions.ParseError "Unknown exception Parsing File")
        end
      in hash_include_files, parse_tree
    end

let parse_string (data : string) (filename : string) expand_expr =
  let pre_tokens  =
    Flx_pretok.pre_tokens_of_string data filename expand_expr
  in
  let tokens  = Flx_lex1.translate pre_tokens in
    begin
      let toker = (new Flx_tok.tokeniser tokens) in
      try
        dyphack (
        Flx_parse.compilation_unit
        (toker#token_src)
        (Lexing.from_string "dummy" )
        )
      with _ -> begin
        toker#report_syntax_error;
        raise (Flx_exceptions.ParseError "Parsing String")
      end
    end
