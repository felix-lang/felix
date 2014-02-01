open Flx_token
open Ocs_types
open Dyp
open Flx_srcref
open Flx_parse_helper
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
        Flx_parse_srcref.adjust_lineno lexbuf s; Sunspec
      | _ -> raise (Ocs_error.Error ("adjust-linecount: not a string"))
    in
    let env = Flx_parse_helper.global_data.Flx_token.env in
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

let make_parser_state () : Flx_token.local_data_t = Flx_parse_helper.local_data

let parser_data { rev_stmts_as_scheme = r } = r

(* ---------------------------------------------------------------------------------------- *)
let parse_compilation_unit old_local_data lexbuf : local_data_t = 
  Flx_parse.dyphack (Flx_parse.compilation_unit ~local_data:old_local_data ~global_data:Flx_parse_helper.global_data lexbuf)

let parse_lexbuf old_local_data lexbuf : local_data_t =
  parse_lexbuf_with_parser parse_compilation_unit old_local_data lexbuf

let parse_channel ?(name="<channel>") parser_state channel =
  let parser_pilot = pp () in
  let lexbuf = Dyp.from_channel parser_pilot channel in
  Dyp.set_fname lexbuf name;
  parse_lexbuf parser_state lexbuf

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
  

let rec load_file include_dirs buffer name =
  let lineno = ref 0 in
  let ch = 
    try open_in_bin name 
    with _ ->  print_endline ("Can't open file '" ^ name ^ "'"); assert false
  in
  let parent_dir = Filename.dirname name in 
  try
    while true do 
      let line = input_line ch in
      incr lineno;
      let include_file = get_hash_include (parent_dir :: include_dirs) line in
      if include_file <> "" then begin
print_endline ("#include file '" ^ include_file ^ "'");
        Buffer.add_string buffer ("#line 1 \""^include_file^"\"\n");
        load_file include_dirs buffer include_file;
        Buffer.add_string buffer ("#line "^string_of_int (!lineno+1)^" \""^name^"\"\n")
      end
      else 
        Buffer.add_string buffer (line ^ "\n")
    done
  with End_of_file ->
    close_in ch

let feed_buffer buffer = 
  let start = ref 0 in 
  let len = Buffer.length buffer in
  fun s n -> 
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

let parse_file ?(include_dirs=[]) parser_state name =
  let name = Flx_filesys.find_file ~include_dirs name in
  let buffer = Buffer.create 10000 in
  Buffer.add_char buffer '\n';
  load_file include_dirs buffer name;
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
  parse_lexbuf parser_state lexbuf


(*
let parse_file ?(include_dirs=[]) parser_state name =
  let name = Flx_filesys.find_file ~include_dirs name in
  let inf name =
    let ch = open_in_bin name in
    let isopen = ref true in
    let isstart = ref true in
    let reader s n =
      if !isopen then 
      begin
        if !isstart then 
        begin
          s.[0] <- '\n';
          isstart := false;
          1
        end
        else 
        begin
          let count = input ch s 0 n in
          if count = 0 then 
          begin
            isopen := false;
            close_in ch;
          end
          ;
          count
        end
      end
      else 0
    in 
    reader
  in 
  let parser_pilot = pp () in
  let lexbuf = Dyp.from_function parser_pilot (inf name) in
  Dyp.set_fname lexbuf name;
  begin (* fudge line count *)
    let olexbuf = (Dyp.std_lexbuf lexbuf) in 
    let lcp = olexbuf.lex_curr_p in
    olexbuf.lex_curr_p <- { lcp with
    pos_lnum = lcp.pos_lnum - 1;
  }
  end
  ;
  parse_lexbuf parser_state lexbuf
*)

let parse_string ?(name="<string>") parser_state str =
  let parser_pilot = pp() in
  let lexbuf = Dyp.from_string parser_pilot str in
  Dyp.set_fname lexbuf name;
  parse_lexbuf parser_state lexbuf

let parse_function ?(name="<function>") parser_state f =
  let parser_pilot = pp () in
  let lexbuf = Dyp.from_function parser_pilot f in
  parse_lexbuf parser_state lexbuf


(* ---------------------------------------------------------------------------------------- *)

let parse_syntax_unit old_local_data lexbuf : local_data_t = 
  Flx_parse.dyphack (Flx_parse.syntax_unit ~local_data:old_local_data ~global_data:Flx_parse_helper.global_data lexbuf)

let parse_syntax_lexbuf old_local_data lexbuf : local_data_t  =
  parse_lexbuf_with_parser parse_syntax_unit old_local_data lexbuf

let parse_syntax_channel ?(name="<channel>") parser_state channel =
  let parser_pilot = pp () in
  let lexbuf = Dyp.from_channel parser_pilot channel in
  Dyp.set_fname lexbuf name;
  parse_syntax_lexbuf parser_state lexbuf

let parse_syntax_file ?(include_dirs=[]) parser_state name =
  let name = Flx_filesys.find_file ~include_dirs name in
  Flx_filesys.with_file_in name (parse_syntax_channel ~name parser_state)


