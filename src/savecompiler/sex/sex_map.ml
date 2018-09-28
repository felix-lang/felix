open Sex_types
open List

let rec sex_map f (x:sexp_t): sexp_t =
  match x with
  | Lst xs -> f (Lst (map (sex_map f) xs))
  | x -> f x

let rec subst fresh env x =
  match x with
  | Lst [Id "lambda"; Id name; y] ->
    let z = Id ("_" ^ string_of_int (fresh())) in
    Lst [Id "lambda"; z; subst fresh ((name,z)::env) y]

  | Id y as z -> (try assoc y env with Not_found -> z)
  | Lst xs -> Lst (map (subst fresh env) xs)
  | x -> x

let rec eval fresh env x =
  (*
  print_string "Eval "; Sex_print.sex_print x;
  *)
  match x with
  | Lst [Id "apply"; f; arg] ->
    (*
    print_endline "Application = "; Sex_print.sex_print term;
    *)
    let f = subst fresh env f in
    let arg = subst fresh env arg in
    (*
    print_string "f = "; Sex_print.sex_print f;
    print_string "a = "; Sex_print.sex_print arg;
    *)
    begin match f with
    | Lst [Id "lambda"; Id name; body] ->
      let body = subst fresh ((name,arg)::env) body in
      (*
      print_string "body(after sub) = "; Sex_print.sex_print body;
      *)
      eval fresh env body

    | _ -> Lst [Id "apply"; f; arg]
    end

  | Lst [Id "letin"; Id name; y; body] ->
    let y = subst fresh env y in
    eval fresh ((name,y)::env) body

  | Lst [Id "defin"; Id name; y; body] ->
    eval fresh ((name,y)::env) body

  | Lst [Id "ifelse"; c; t; f] ->
    let c = eval fresh env c in
    let r = if c = Int "0" then f else t in
    (*
    print_string "After cond="; Sex_print.sex_print r;
    *)
    eval fresh env r

  | Lst (Sym "+" :: t) as x ->
    begin try Int (string_of_int (fold_left
      (fun acc x -> match eval fresh env x with
        | Int y -> acc + int_of_string y
        | _ -> raise Not_found
      )
      0 t
      ))
    with Not_found -> x
    end

  | Lst (Sym "-" :: h :: t1 :: tt) as x ->
    let h = eval fresh env h in
    begin match h with
    | Int h ->
    begin try Int (string_of_int (fold_left
      (fun acc x -> match eval fresh env x with
        | Int y -> acc - int_of_string y
        | _ -> raise Not_found
      )
      (int_of_string h) (t1::tt)
      ))
    with Not_found -> x
    end
    | _ -> x
    end

  | Lst [Sym "-" ; Int s] ->
    Int (string_of_int (- (int_of_string s)))

  | Lst (Sym "*" :: h :: t) as x ->
    let h = eval fresh env h in
    begin match h with
    | Int h ->
    begin try Int (string_of_int (fold_left
      (fun acc x -> match eval fresh env x with
        | Int y -> acc * int_of_string y
        | _ -> raise Not_found
      )
      (int_of_string h) t
      ))
    with Not_found -> x
    end
    | _ -> x
    end
  | Id y as z -> (try assoc y env with Not_found -> z)
  | Lst xs -> Lst (map (eval fresh env) xs)
  | x -> x

