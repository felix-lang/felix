let _ = print_endline "
Simple calculator example

Priorities: pi<pt<pp

Grammar:

main: expr \"\\n\" { $1 }
expr:
  | ['0'-'9']+      { int_of_string $1 } pi
  | \"-\" expr(=pi)            { -$2 }     pi
  | \"(\" expr \")\"             { $2 }      pi
  | expr(<=pp) \"+\" expr(<pp) { $1 + $3 } pp
  | expr(<=pp) \"-\" expr(<pp) { $1 - $3 } pp
  | expr(<=pt) \"*\" expr(<pt) { $1 * $3 } pt
  | expr(<=pt) \"/\" expr(<pt) { $1 / $3 } pt
"

let _ =
  try
    while true do
      ((*Dyp.flush_input lexbuf;*)
      try
        (*let pf = Calc_parser.main lexbuf in*)
        let pf =
          Calc_parser.main
          (Dyp.from_channel (Calc_parser.pp ()) stdin)
        in
        Printf.printf "= %d\n\n" (fst (List.hd pf))
      with
        Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Failure _ -> exit 0
