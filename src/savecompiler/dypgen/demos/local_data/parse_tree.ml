type tree = Int of int | Ident of string | Plus of (tree * tree) | Let of (tree * tree) | Binding of (string * tree)

let print_tree t =
  let rec aux t = match t with
    | Int x -> print_int x
    | Ident x -> print_string x
    | Plus (t1,t2) -> (
        print_string "(";
        aux t1;
        print_string "+";
        aux t2;
        print_string ")")
    | Let (t1,t2) -> (
        print_string "(let ";
        aux t1;
        print_string " in ";
        aux t2;
        print_string ")")
    | Binding (t1,t2) -> (
        print_string (t1^" = ");
        aux t2)
  in
  aux t;
  print_newline ()
