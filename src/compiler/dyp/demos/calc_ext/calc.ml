let () = print_endline "
Basic extensible calculator example

Initial priorities: pi<p+

Initial grammar :
S -> E
E(pi) -> int
E(p+) -> E(<=p+) + E(<p+)
A     -> &
E(pi) -> A E

When the parser reduces with the rule A -> &,
the grammar is extended with the following rule:
  E(p*) -> E(<=p*) * E(<p*)
And the relation is extended to: pi<p*<p+
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
