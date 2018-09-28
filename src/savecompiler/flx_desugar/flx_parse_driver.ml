open Flx_token
open Ocs_types
open Dyp
open Flx_srcref
open Lexing
open Flx_parse

let show_error lexbuf =
  let b = (Dyp.std_lexbuf lexbuf) in
  let file = b.lex_start_p.pos_fname in
  let start_line = b.lex_start_p.pos_lnum in
  let start_col = b.lex_start_p.pos_cnum - b.lex_start_p.pos_bol + 1 in
  let end_line = b.lex_curr_p.pos_lnum in
  let end_col = b.lex_curr_p.pos_cnum - b.lex_start_p.pos_bol in
  let sr = Flx_srcref.make (file, start_line, start_col, end_line, end_col) in
  print_endline (Flx_srcref.long_string_of_src sr)

let parse_lexbuf_with_parser aparser old_local_data lexbuf : local_data_t =
  (* This is a horrible hack, because the global environment is set to
     the current lexbuf at the start of a parse, preventing recursive
     parsing since it isn't re-entrant
  *)
  begin
    let adjust_line s =
      match s with
      | Sstring s -> 
        Flx_parse_srcref.adjust_lineno lexbuf (Bytes.to_string s); Sunspec
      | _ -> raise (Ocs_error.Error ("adjust-linecount: not a string"))
    in
    let env = Flx_parse_data.global_data.Flx_token.env in
    Ocs_env.set_pf1 env adjust_line "adjust-linecount"
  end;

  let local_data =
    try aparser old_local_data lexbuf
    with
      | Failure s ->
        begin
          if Printexc.backtrace_status () then begin
            print_endline (Printexc.get_backtrace ());
          end;
          print_endline ("Parse fail " ^ s);
          show_error lexbuf;
          raise (Flx_exceptions.ParseError ("Failure \"" ^ s ^ "\" Parsing File"))
        end
(*
      | Flx_exceptions.ClientError (sr, s)
      | Flx_exceptions.ClientError2 (sr, _, s)
      | Flx_exceptions.ClientErrorn (sr::_, s) as x  ->
        begin
          if Printexc.backtrace_status () then begin
            print_endline (Printexc.get_backtrace ());
          end;
          print_endline "Parse fail";
          print_endline (Flx_srcref.long_string_of_src sr);
          raise x
        end
*)
      | Flx_ocs_init.Scheme_error x ->
          begin match x with 
          | Sstring (s) -> failwith (Bytes.to_string s)
          | _ -> failwith "SCHEME_ERROR"
          end

      | Dyp.Syntax_error as x ->
          show_error lexbuf;
          raise x

      | Dyp.Bad_constructor (nt, ctor1, ctor2) ->
          let s = "Bad constructor '" ^ ctor1 ^ "' and '" ^ ctor2 ^ "' for nonterminal '" ^ nt ^ "'" in
          print_endline s;
          failwith s

      | x ->
        begin
          print_string "Fatal error: exception ";
          print_endline (Printexc.to_string x);
          if Printexc.backtrace_status () then begin
            print_endline (Printexc.get_backtrace ());
          end;
          raise (Flx_exceptions.ParseError "Unknown exception Parsing File")
        end
  in
  local_data

let make_parser_state () : Flx_token.local_data_t = Flx_parse_data.local_data

let parser_data { rev_stmts_as_scheme = r } = r

(* ---------------------------------------------------------------------------------------- *)
let match_hash_include line =
  let line = line ^ "\n" in (* add terminator *)
  if String.length line > 12 then
    if String.sub line 0 9 = "#include " then begin
      let i = ref 9 in
      while line.[!i] = ' ' do incr i done; (* skip white *)
      if line.[!i] <> '"' then "" else begin (* require dquote *)
        incr i; (* skip dquote *)
        let j = ref (!i) in
        while line.[!j] <> '"' && line.[!j] <> '\n' do incr j done; (* scan to end of quote *)
        if line.[!j] <> '"' then "" else (* require dquote *)
        let s = String.sub line (!i) (!j - !i) in
        s
      end
    end else ""
  else ""

let get_hash_include include_dirs line =
  let include_file = match_hash_include line in
  if include_file = "" then "" else
  try
    let include_file = Flx_filesys.find_file ~include_dirs include_file in
    let include_file = Flx_filesys.mkabs include_file in
    include_file
  with Flx_filesys.Missing_path _ -> ""
  

let rec load_file include_dirs hash_includes buffer name =
  let lineno = ref 0 in
  let ch = 
    try open_in_bin name 
    with _ ->  print_endline ("Can't open file '" ^ name ^ "'"); assert false
  in
  let parent_dir = Filename.dirname name in 
  try
    while true do 
      let line = input_line ch in
      let line = 
         if !lineno = 0 && String.length line > 2 && String.sub line 0 2 = "#!" then
         "//" ^ String.sub line 2 (String.length line - 2)
         else line
      in
      incr lineno;
(*
      let include_file = get_hash_include (parent_dir :: include_dirs) line in
      if include_file <> "" then begin
print_endline ("#include file '" ^ include_file ^ "'");
        hash_includes := Flx_parse_helper.uniq_add include_file (!hash_includes);
        Buffer.add_string buffer ("#line 1 \""^include_file^"\"\n");
        load_file include_dirs hash_includes buffer include_file;
        Buffer.add_string buffer ("#line "^string_of_int (!lineno+1)^" \""^name^"\"\n")
      end
      else 
*)
        Buffer.add_string buffer (line ^ "\n")
    done
  with End_of_file ->
    close_in ch

(* FIXME: WARNING DANGEROUS HACKERY MUTATING STRING *)
let feed_buffer buffer = 
  let start = ref 0 in 
  let len = Buffer.length buffer in
  (*
  fun (s':string) n -> 
    (* Currently Dypgen provides a string to be filled in, so we have to
     * unsafely alias it as bytes to allow the blit to work. This code
     * will change when and if Dypgen is fixed to use bytes
     *)
    let s = Bytes.unsafe_of_string s' in 
*)
  fun (s:bytes) n ->
    if n < (len - !start) then begin
      Buffer.blit buffer (!start) s 0 n;
      start := (!start) + n;
      n
    end else begin
      let m = len - !start in
      Buffer.blit buffer (!start) s 0 m;
      start := len;
      m
    end

let create_file_lexbuf ~include_dirs name = 
  let name = Flx_filesys.find_file ~include_dirs name in
  let buffer = Buffer.create 10000 in
  let hash_includes = ref [] in
  Buffer.add_char buffer '\n';
  load_file include_dirs hash_includes buffer name;
  let parser_pilot = pp () in
  let lexbuf = Dyp.from_function parser_pilot (feed_buffer buffer) in
  Dyp.set_fname lexbuf name;
  begin (* fudge line count *)
    let olexbuf = (Dyp.std_lexbuf lexbuf) in 
    let lcp = olexbuf.lex_curr_p in
    olexbuf.lex_curr_p <- { lcp with
    pos_lnum = lcp.pos_lnum - 1;
  }
  end
  ;
  lexbuf, !hash_includes 

(* ---------------------------------------------------------------------------------------- *)
let parse_compilation_unit local_data lexbuf : local_data_t = 
  let global_data = Flx_parse_data.global_data in
  Flx_parse.dyphack (Flx_parse.compilation_unit ~local_data ~global_data lexbuf)

let parse_syntax_unit local_data lexbuf : local_data_t = 
  let global_data = Flx_parse_data.global_data in
  Flx_parse.dyphack (Flx_parse.syntax_unit ~local_data ~global_data lexbuf)

(* ---------------------------------------------------------------------------------------- *)
let parse_lexbuf_with_compilation_unit old_local_data lexbuf : local_data_t =
  parse_lexbuf_with_parser parse_compilation_unit old_local_data lexbuf

let parse_lexbuf_with_syntax_unit old_local_data lexbuf : local_data_t  =
  parse_lexbuf_with_parser parse_syntax_unit old_local_data lexbuf

(* ---------------------------------------------------------------------------------------- *)
(* USER ENTRY POINTS *)
let parse_file_with_compilation_unit ?(include_dirs=[]) parser_state name =
  let lexbuf,hash_includes = create_file_lexbuf include_dirs name in
  hash_includes,parse_lexbuf_with_compilation_unit parser_state lexbuf

let parse_file_with_syntax_unit ?(include_dirs=[]) parser_state name =
  let lexbuf,hash_includes = create_file_lexbuf include_dirs name in
  hash_includes,parse_lexbuf_with_syntax_unit parser_state lexbuf


