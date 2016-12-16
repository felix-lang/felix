type tree = Int of int | Node of (tree * tree * tree)

let rec print_tree t = match t with
  | Int x -> print_int x
  | Node (t1,t2,t3) -> (
      print_string "(";
      print_tree t1;
      print_string "+";
      print_tree t2;
      print_string "*";
      print_tree t3;
      print_string ")")

let print_forest forest =
  let aux t = print_tree t; print_newline () in
  List.iter aux forest;
  print_newline ()
