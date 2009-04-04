type tree = Int of int | Plus of (tree * tree) | Times of (tree * tree)

let print_tree t =
  let rec aux t = match t with
    | Int x -> print_int x
    | Plus (t1,t2) -> (
        print_string "(";
        aux t1;
        print_string "+";
        aux t2;
        print_string ")")
    | Times (t1,t2) -> (
        print_string "(";
        aux t1;
        print_string "*";
        aux t2;
        print_string ")")
  in
  aux t;
  print_newline ()
