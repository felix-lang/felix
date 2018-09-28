let () = print_endline "
Simple calculator example

Priorities: pi<pt<pp

Grammar :
main: expr \"\\n\" { $1 }
expr:
  | ['0'-'9']    { int_of_string $1 } pi
  | \"-\" expr(=pi)        { -$2 }      pi
  | \"(\" expr \")\"         { $2 }       pi
  | expr(<=pp) (\"+\" {`PLUS} | \"-\" {`MINUS}) expr(<pp)
      { match $2 with
          | `PLUS -> $1 + $3
          | `MINUS -> $1 - $3 } pp
  | expr(<=pt) (\"*\" {`TIMES} | \"/\" {`DIV}) expr(<pt)
      { match $2 with
          | `TIMES -> $1 * $3
          | `DIV -> $1 / $3 } pt
"

let lexbuf = Dyp.from_channel (Calc_parser.pp ()) stdin

let _ =
  try
    while true do
      (Dyp.flush_input lexbuf;
      try
        let pf = Calc_parser.main lexbuf in
        Printf.printf "= %d\n\n" (fst (List.hd pf))
      with
        Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Failure _ -> exit 0
