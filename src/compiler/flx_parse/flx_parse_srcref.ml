open Dyp
open Lexing

let getsr dyp =
  let s = dyp.symbol_start_pos() and e = dyp.symbol_end_pos() in
(*
if s <> e then begin
let si x = string_of_int x in
  if s.pos_fname <> e.pos_fname then
    print_endline "MULTIFILE REDUCTION"
  else if s.pos_lnum <> e.pos_lnum then
    print_endline "MULTILINE REDUCTION"
  else if s.pos_cnum <> e.pos_cnum then
    print_endline ("MULTICHAR REDUCTION, length " ^ si (e.pos_cnum - s.pos_cnum))
  ;
  if e.pos_cnum < s.pos_cnum then
    print_endline "NEGATIVE LENGTH REDUCTION??"
  ;
  print_endline ("Getsr: filename start " ^ s.pos_fname ^ ", end " ^ e.pos_fname);
  print_endline ("Getsr: lnum start " ^ si s.pos_lnum ^ ", end " ^ si e.pos_lnum);
  print_endline ("Getsr: cnum start " ^ si s.pos_cnum ^ ", end " ^ si e.pos_cnum);
  print_endline ("Getsr: bol start " ^ si s.pos_bol ^ ", end " ^ si e.pos_bol);
  print_endline ("Getsr: column start " ^ si (s.pos_cnum - s.pos_bol) ^ ", end " ^ si (e.pos_cnum - e.pos_bol));
  print_endline "";
end;
*)
  if s.pos_fname <> e.pos_fname then begin
    (* print_endline "MULTIFILE REDUCTION"; *)
    Flx_srcref.make (
      e.pos_fname,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1)
  end else if s.pos_lnum < e.pos_lnum then begin
    (* print_endline "MULTILINE REDUCTION"; *)
    Flx_srcref.make (
      e.pos_fname,
      s.pos_lnum,
      s.pos_cnum - s.pos_bol + 1,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1)
  end else if s.pos_lnum > e.pos_lnum then begin
    print_endline "REVERSE MULTILINE REDUCTION";
    Flx_srcref.make (
      e.pos_fname,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1)
  end else if e.pos_cnum < s.pos_cnum then begin
      print_endline "NEGATIVE LENGTH REDUCTION??";
    Flx_srcref.make (
      e.pos_fname,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1)
  end else if e.pos_cnum = s.pos_cnum then begin
    (*  print_endline "ZERO LENGTH REDUCTION??"; *)
    Flx_srcref.make (
      e.pos_fname,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1)
  end else if e.pos_cnum > s.pos_cnum then begin
     Flx_srcref.make (
      e.pos_fname,
      s.pos_lnum,
      s.pos_cnum - s.pos_bol + 1,
      e.pos_lnum,
      e.pos_cnum - e.pos_bol + 1) (* recent change by symmetry not thinking *)
  end else assert false

let incr_lineno lexbuf= 
  Dyp.set_newline lexbuf

let set_lineno lexbuf n =
  let lexbuf = (Dyp.std_lexbuf lexbuf) in 
  let lcp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { lcp with
    pos_lnum = n;
    pos_bol = lcp.pos_cnum;
  }

let lfcount s =
  let n = ref 0 in
  for i = 0 to (String.length s) - 1 do
    if s.[i] = '\n' then incr n
  done;
  !n

let adjust_lineno lexbuf s =
  let lexbuf = (Dyp.std_lexbuf lexbuf) in 
  let start_pos = lexbuf.lex_start_p in
  let fname = start_pos.pos_fname in
  let lnum = ref start_pos.pos_lnum in
  let cnum = ref start_pos.pos_cnum in
  let bol = ref start_pos.pos_bol in
  for i = 0 to String.length s - 1 do
    incr cnum;
    if s.[i] = '\n' then begin
     incr lnum;
     bol := !cnum;
    end
  done;
  let old_end = lexbuf.lex_curr_p in
  assert (old_end.pos_cnum = !cnum);
  let end_pos =  
    { 
      pos_fname = fname; 
      pos_lnum = !lnum;
      pos_bol = !bol;
      pos_cnum = !cnum;
    }
  in
  lexbuf.lex_curr_p <- end_pos



