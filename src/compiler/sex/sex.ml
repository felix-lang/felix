open Sex_types
open List

let main() =
  let mk_fresh () =
    let counter = ref 0 in
    fun x -> let y = !counter in incr counter; y
  in
  let fresh = mk_fresh () in
  let filename = Sys.argv.(1) in
  print_endline ("Processing " ^ filename);
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let sexes = Sex_parse.sex_parse (Sex_lex.sex_lex 1) lexbuf in
  close_in file;

  let sex = match sexes with
  | (sex,_) :: _ -> sex
  | _ -> assert false (* ambiguous parse not expected! *)
  in
  Sex_print.sex_print sex;
  print_endline "Done"
  ;
  let sex = Sex_map.eval fresh [] sex in
  print_endline "Evaluated:";
  Sex_print.sex_print sex
  ;

  let filename = "test2.sex" in
  print_endline ("Processing " ^ filename);
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let sexes = Sex_parse.sex_parse (Sex_lex.sex_lex 1) lexbuf in
  close_in file;
  let sex = match sexes with
  | (sex,_) :: _ -> sex
  | _ -> assert false (* ambiguous parse not expected! *)
  in
  Sex_print.sex_print sex;
  (*
  let sr = "test2.sex",0,0,0,0 in
  let flxe : Flx_ast.expr_t = Sex2flx.xexpr_t sr 1 [] sex in
  print_endline ("e=" ^ Flx_print.string_of_expr flxe)
  *)
;;

main()
;;

