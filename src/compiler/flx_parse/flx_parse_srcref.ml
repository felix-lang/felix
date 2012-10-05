open Dyp
open Lexing

let getsr dyp =
  let s = dyp.symbol_start_pos() and e = dyp.symbol_end_pos() in
  Flx_srcref.make (
    s.pos_fname,
    s.pos_lnum,
    s.pos_cnum - s.pos_bol + 1,
    e.pos_lnum,
    e.pos_cnum - e.pos_bol)

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


