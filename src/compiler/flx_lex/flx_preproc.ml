open Flx_util
open Flx_token
open Flx_string
open Big_int
open Flx_exceptions
open Flx_lexstate
open List

let dyphack (ls : ( 'a * string) list) : 'a =
  match ls with
  | [x,_] -> x
  | _ -> failwith "Dypgen parser failed"

let dyphack_parser f tk lb = dyphack (f tk lb)

let substr = String.sub
let len = String.length

let is_in_string s ch =
  try
    ignore(String.index s ch);
    true
  with Not_found ->
    false

let is_white = is_in_string " \t"
let is_digit = is_in_string "0123456789"

let strip_us s =
  let n = String.length s in
  let x = Buffer.create n in
  for i=0 to n - 1 do
    match s.[i] with
    | '_' -> ()
    | c -> Buffer.add_char x c
  done;
  Buffer.contents x


let pre_tokens_of_lexbuf lexer buf state =
  let rec get lst =
    let ts = lexer state buf in
    match ts with
    | [ENDMARKER] -> lst
    | _ -> get (rev_append ts lst)
  in
    rev (get [])

let pre_tokens_of_string lexer s filename expand_expr =
  let state = new lexer_state filename "" [] None expand_expr in
  pre_tokens_of_lexbuf lexer (Lexing.from_string s) state

let line_directive state sr s lexbuf =
  let i = ref 0 in
  let a =
    let a = ref 0 in
    while is_digit s.[!i] do
      a := !a * 10 + dec_char2int s.[!i];
      incr i
    done;
    !a
  in
  if !i = 0
  then clierr sr "digits required after #line"
  else begin
    while is_white s.[!i] do incr i done;
    if s.[!i] <> '\n'
    then begin
      if s.[!i]<>'"'
      then clierr sr "double quote required after line number in #line"
      else begin
        incr i;
        let j = !i in
        while s.[!i]<>'"' && s.[!i]<>'\n' do incr i done;

        if s.[!i]='\n'
        then clierr sr "double quote required after filename in #line directive"
        else begin
          let filename = String.sub s j (!i-j) in
          state#set_filename filename;
          state#set_line a lexbuf
        end
      end
    end else begin
      (* print_endline ("SETTING LINE " ^ string_of_int a); *)
      state#set_line a lexbuf
    end
  end;
  [NEWLINE]

let find_include_file state s sr =
  if s.[0]<>'"' && s.[0]<>'<'
  then clierr sr "'\"' or '<' required after #include"
  ;
  let rquote = if s.[0]='"' then '"' else '>' in
  let i = ref 1 in
  let j = !i in
  while s.[!i]<>rquote && s.[!i]<>'\n' do incr i done
  ;

  if s.[!i]='\n'
  then clierr sr "double quote required after filename in #include directive"
  ;
  let filename = String.sub s j (!i-j) in
  let filename=
    if rquote = '"'
    then state#get_relative filename
    else state#get_absolute filename
  in
    (*
      print_endline (
      "//Resolved in path: \"" ^ filename ^ "\""
    );
    *)
    filename

(* throw if the file isn't found or is bad *)
let load_syntax filename outfile =
   let syn_mt = Flx_filesys.filetime filename in
   let cache_mt = Flx_filesys.filetime outfile in
   (*
   print_endline ("Syntax file time = " ^ string_of_float syn_mt);
   print_endline ("Cache file time = " ^ string_of_float cache_mt);
   *)
   if syn_mt > cache_mt then raise Not_found;
   let this_version = !Flx_version.version_data in
   let x =  open_in_bin outfile in
   let that_version = Marshal.from_channel x in
   if this_version <> that_version then begin
     close_in x;
     raise Not_found
   end;

   assert(this_version = that_version);
   assert(this_version.Flx_version.build_time_float < cache_mt);

   let local_data = Marshal.from_channel x in
   close_in x;
   (*
   print_endline ("// Load Syntax file " ^ outfile ^
     " flxg compile time = " ^
     string_of_float this_version.Flx_version.build_time_float ^
     " = " ^
     this_version.Flx_version.build_time
   );
   *)
   [LOAD_SYNTAX (local_data)]

let include_directive kind state sr s pre_flx_lex =
  (*
  print_endline ("#" ^ kind ^ " " ^ s);
  *)
  let filename = find_include_file state s sr in
  if mem filename state#get_include_files then begin
    (*
    print_endline ("WARNING: duplicate #import/include of " ^ filename);
    print_endline ("In " ^ Flx_srcref.long_string_of_src sr);
    print_endline "SKIPPED";
    *)
    []
  end else begin
  state#add_include_file filename;
  let pre_tokens_of_filename filename =
    let incdirs = state#get_incdirs in
    let basedir = Filename.dirname filename in
    let state' = new lexer_state filename basedir incdirs state#get_cache_dir state#get_expand_expr in
    let infile = open_in filename in
    let src = Lexing.from_channel infile in
    let toks = pre_tokens_of_lexbuf pre_flx_lex src state' in
      close_in infile;
      if kind = "import" then begin
        state#add_macros state'
      end;
      iter state#add_include_file state'#get_include_files;
      toks
  in
  match kind with
  | "import"
  | "include" ->
    pre_tokens_of_filename filename
  | "syntax" ->
    begin
      let outfile = filename ^ ".syncache" in
      try load_syntax filename outfile with _ ->
      match state#get_cache_dir with
      | None -> pre_tokens_of_filename filename @ [SAVE_SYNTAX outfile]
      | Some d ->
        let outfile = Filename.concat d (Filename.basename outfile) in
        try load_syntax filename outfile with _ ->
        pre_tokens_of_filename filename @ [SAVE_SYNTAX outfile]
    end
  | _ -> assert false
  end

let count_newlines s =
  let n = ref 0 in
  let len = ref 0 in
  let last_len = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\n' then begin incr n; last_len := !len; len := 0; end
    else incr len
  done;
  !n,!last_len

let handle_preprocessor state lexbuf s pre_flx_lex start_location start_position =
  let linecount,last_line_len = count_newlines s in
  let file,line1,col1,_ = state#get_srcref lexbuf in
  let file',line1',_,_ = state#get_physical_srcref lexbuf in

  let next_line = line1+linecount in
  let next_line' = line1'+linecount in
  let sr = file,line1,col1,next_line-1,last_line_len+1 in
  let sr' = file',line1',col1,next_line'-1,last_line_len+1 in
  let saved_buf_pos = Lexing.lexeme_end lexbuf in
  (*
  print_endline ("PREPROCESSING: " ^ Flx_srcref.long_string_of_src sr);
  print_endline ("Trailing buf pos = " ^ si saved_buf_pos);
  *)
  let ident,s' =

    (* .. note the string WILL end with a newline .. *)

    (* skip spaces *)
    let i = ref 0 in
    while is_white s.[!i] && (s.[!i] <> '\n') do incr i done;

    (* scan non-spaces, stop at #, white, or newline *)
    let n = ref 0 in
    while
      not (is_white s.[!i + !n]) &&
      not (s.[!i + !n]='\n') &&
      not (s.[!i + !n]='#')
    do incr n done;

    (* grab the preprocessor directive name *)
    let ident = String.sub s !i !n in

    (* scan for next non-white *)
    let j = ref (!i + !n) in
    while is_white s.[!j] && (s.[!j] <> '\n') do incr j done;

    (* scan back from end of text for last non-white *)
    n := String.length s - 1;
    while !n > !j && is_white(s.[!n-1]) do decr n done;

    (* grab the text from after the directive name to the end *)
    let ssl = !n - !j in
    let rest = String.sub s !j ssl in
    ident,rest
  in

  (*
  print_endline ("PREPRO i=" ^ ident^", t='"^s'^"',\ns='"^s^"'");
  *)
  match ident with

  (* THESE COMMANDS ARE WEIRD HANGOVERS FROM C WHICH
     CANNOT HANDLE NORMAL TOKENISATION
  *)
  (* print a warning *)
  | "error" ->
      print_endline ("#error " ^ s');
      clierr2 sr sr' ("#error " ^ s')

  | "warn" ->
    let result =
        let desc = Flx_srcref.short_string_of_src sr in
          print_endline desc
        ;
        if sr <> sr' then begin
          let desc = Flx_srcref.short_string_of_src sr' in
          print_endline ("Physical File:\n" ^ desc)
        end
        ;
        print_endline ("#warn " ^ s');
        print_endline "";
        [NEWLINE]
    in
      for i = 1 to linecount do state#newline lexbuf done;
      result

  | "line" ->
    line_directive state sr s' lexbuf

  | "syntax" ->
    let result =
        include_directive ident state sr s' pre_flx_lex
    in
     for i = 1 to linecount do state#newline lexbuf done;
     result

  (* THESE ONES USE ORDINARY TOKEN STREAM *)
  | _ ->
  let result =
    let src = Lexing.from_string s in
    (*
    print_endline ("Start buf pos = " ^ si (start_position.Lexing.pos_cnum));
    print_endline ("Start loc = " ^ si (start_location.buf_pos));
    *)

    (* hack the location to the start of the line *)
    let b = start_location.buf_pos - start_position.Lexing.pos_cnum in
    (*
    print_endline ("Hacking column position to " ^ si b);
    *)
    state#set_loc {
      buf_pos = b;
      last_buf_pos = b;
      line_no = line1;
      original_line_no = line1';
    };

    let toks = pre_tokens_of_lexbuf pre_flx_lex src state in

    (* use the special preprocessor token filter *)
    let toks = Flx_lex1.translate_preprocessor toks in

    (*
    iter (fun tok ->
      let sr = Flx_srcref.slift (Flx_prelex.src_of_token tok) in
      print_endline (Flx_srcref.long_string_of_src sr)
    )
    toks;
    *)

    match toks with
    | [] -> [] (* DUMMY *)
    | h :: toks ->
    let h = Flx_prelex.string_of_token h in
    if h <> ident then
      failwith (
        "WOOPS, mismatch on directive name: ident=" ^
        ident ^ ", head token = " ^
        h
      )
    ;

      print_endline (state#string_of_srcref lexbuf);
      print_endline
      (
        "LEXICAL ERROR: IGNORING UNKNOWN PREPROCESSOR DIRECTIVE \"" ^
        ident ^ "\""
      );
      [NEWLINE]
  in

  (* restore the location to the start of the next line *)
  state#set_loc {
    buf_pos = saved_buf_pos;
    last_buf_pos = saved_buf_pos;
    line_no = next_line;
    original_line_no = next_line'
  };
  result
