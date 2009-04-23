open Flx_token
open Flx_lex
open Flx_prelex
open Flx_preproc
open List

let pre_tokens_of_lexbuf buf state =
  let rec get lst =
    let t = Flx_lex.pre_flx_lex state buf in
    match t with
    | [ENDMARKER] -> lst
    | _ -> get (List.rev_append t lst)
   in
   let tks = ENDMARKER :: get [] in
    (*
    print_endline
    (
      "#included files are " ^
      String.concat ", " state#get_include_files
    )
    ;
    *)
  let toks = List.rev tks in
  let includes = state#get_include_files in
  HASH_INCLUDE_FILES includes :: toks

(* This routine appears to be called ONLY by the parser. It is never
   called for any #includes. So it is triggered only for each
   whole file name given to flxg, or, by a syntactic include directive
   which recursively parses.
*)

let rec pre_tokens_of_filename filename dirname incdirs cache_dir expand_expr auto_imports =
  (*
  print_endline ("tokenising " ^ filename);
  *)
  let state = new Flx_lexstate.lexer_state filename dirname incdirs cache_dir expand_expr in
  let tokss =
    (map
      (fun fn->
        (*
        print_endline (" .. Autoimporting " ^ fn);
        *)
        include_directive "import" state ("auto_import",0,0,0,0)
        fn pre_flx_lex
      )
      auto_imports
    )
  in
  let split_hs toks = match toks with
  | HASH_INCLUDE_FILES fs :: toks -> fs, toks
  | _ -> assert false
  in

  (*
  print_endline ("Actually tokenising " ^ filename);
  *)
  let infile = open_in filename in
  let src = Lexing.from_channel infile in
  let toks = pre_tokens_of_lexbuf src state in close_in infile;
  let fs,toks = split_hs toks in
  let tokss = tokss @ [toks] in
  let toks = HASH_INCLUDE_FILES fs :: concat tokss in
  (*
  print_endline ("DONE tokenising " ^ filename);
  print_endline ("Token stream is: ");
  Flx_tok.print_tokens toks;
  *)
  toks

let pre_tokens_of_string s filename expand_expr =
  let state = new Flx_lexstate.lexer_state filename "" [] None expand_expr in
  pre_tokens_of_lexbuf (Lexing.from_string s) state
