let () = print_endline "
Counting the number of reductions with data

Priorities: pi<p*<p+

Grammar:
S -> E
E(pi) -> int
E(p+) -> E(<=p+) + E(<p+)
E(p*) -> E(<=p*) * E(<p*)
"

let lexbuf = Dyp.from_channel (Global_data_parser.pp ()) stdin

let _ =
  try
    while true do
      (Dyp.flush_input lexbuf;
      try
        let pf = Global_data_parser.main lexbuf in
        Printf.printf "= %d\n\n" (fst (List.hd pf))
      with
        Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Failure _ -> exit 0
