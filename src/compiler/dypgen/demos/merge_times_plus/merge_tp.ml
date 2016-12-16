open Parse_tree

let () = print_endline "
Example of disambiguation with a merge function
Prints a parse tree

Grammar :
S -> E
E -> int
E -> E + E
E -> E * E

example : 1+2*3 yields
(1+(2*3))
"

let lexbuf = Dyp.from_channel (Merge_tp_parser.pp ()) stdin

let _ =
  try
    while true do
      (Dyp.flush_input lexbuf;
      try
        let pf = Merge_tp_parser.main lexbuf in
        let _ = List.iter print_tree (List.map (fun (x,_) -> x) pf) in
        print_newline ()
      with
        Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Failure _ -> exit 0
