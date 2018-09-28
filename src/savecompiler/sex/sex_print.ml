open Sex_types
open List

let emit p () =
  let maxcol = 40 in
  let col = ref 0 in
  let newline () = p "\n"; col := 0 in
  fun s ->
    let n = String.length s in
    if !col <> 0 && !col + n > maxcol then newline();
    if !col <> 0 then p " ";
    p s;
    col := !col + n + 1

let sex_out p sex =
  let emit = emit p () in
  let rec pr sex = match sex with
  | Int s
  | Sym s
  | Id s -> emit s
  | Str s -> emit ("\"" ^ s ^ "\"")
  | Lst sex -> emit "("; iter pr sex; emit ")"
  in
  pr sex;
  p "\n"

let sex_print sex = sex_out print_string sex

let string_of_sex s =
  let b = Buffer.create 200 in
  sex_out (Buffer.add_string b) s;
  Buffer.contents b

