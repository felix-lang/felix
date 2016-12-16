open Parse_tree

let () = print_endline "
Example of use of local_data to build a simple symbol table
and to disambiguate.

Grammar :
S -> E
E -> int
E -> id
E -> ( E )
E -> E + E
E -> let B in E
B -> id = E

example :

`let x = 1 in x+1'
yields :
  (let x = 1 in (x+1))
  ((let x = 1 in x)+1)

`let x = 1 in 1+x'
only yields :
  (let x = 1 in (1+x))

'Q' to quit
"

 let lexbuf = Dyp.from_channel (Local_data_parser.pp ()) stdin 
(*let lexbuf = Dyp.from_string (Local_data_parser.pp ()) "let x = 1 in x+1\n"
let pf = Local_data_parser.main lexbuf*)

let _ =
  try
    while true do
      (Dyp.flush_input lexbuf;
      try
        let pf = Local_data_parser.main lexbuf in
        let _ = List.iter print_tree (List.map (fun (x,_) -> x) pf) in
        print_newline ()
      with
        Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
    done
  with Failure _ -> exit 0
