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

let incr_lineno lexbuf n = 
  let n = ref n in
  while !n <> 0 do Dyp.set_newline lexbuf; decr n done

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


