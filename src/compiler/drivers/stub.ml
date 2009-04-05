open Flx_types
;;
let pe x = print_endline x
;;
pe "STUB TEST PROGRAM"
;;

open Flx_ctypes
open Flx_cexpr
open Flx_ctype
open List
open Flx_mtypes2

let dyphack (ls : ( 'a * Dyp.priority) list) : 'a =
  match ls with
  | [x,_] -> x
  | _ -> failwith "Dypgen parser failed"

let raw_options = Flx_getopt.parse_options Sys.argv;;
let compiler_options = Flx_flxopt.get_felix_options raw_options;;
let syms = Flx_flxopt.make_syms compiler_options;;

let input_file_name = Sys.argv.(1)
;;

let () =
  let pre_tokens =  Flx_pretok.pre_tokens_of_filename
    input_file_name
    (Filename.dirname input_file_name)
    []
    syms.compiler_options.cache_dir
    Flx_macro.expand_expression
    compiler_options.auto_imports
  in
  Flx_tok.print_pre_tokens pre_tokens;
  let tokens  = Flx_lex1.translate pre_tokens in

  (* strip EOF *)
  let lexbuf = (Lexing.from_string "dummy" ) in
  let toker = (new Flx_tok.tokeniser tokens) in
  while toker#token_peek lexbuf != Flx_token.ENDMARKER do
    let x =
        try
          dyphack (
          Flx_preparse.expression
          (toker#token_src)
          lexbuf
          )
        with _ -> begin
          toker#report_syntax_error;
          raise (Flx_exceptions.ParseError "Parsing Expr")
        end
    in
    print_endline (Flx_print.string_of_expr x)
    ;
  done;
  print_endline "Parsed it"
;;

 (*
let e1 =
  `Ce_infix ("*",
    `Ce_infix ("+",`Ce_atom "x",`Ce_atom "y"),
    `Ce_infix ("*",`Ce_atom "x",`Ce_atom "y")
  )
;;
let e2 =
  `Ce_infix ("-",
    `Ce_infix ("+",`Ce_atom "x",`Ce_atom "y"),
    `Ce_infix ("-",`Ce_atom "x",`Ce_atom "y")
  )
;;
let e3 =
  `Ce_infix ("-",
    `Ce_infix ("*",`Ce_atom "x",`Ce_atom "y"),
    `Ce_infix ("/",`Ce_atom "x",`Ce_atom "y")
  )
;;

let e4 =
  `Ce_infix ("-",
    `Ce_infix ("-",`Ce_atom "x",`Ce_atom "y"),
    `Ce_atom "z"
  )
;;
let e5 =
  `Ce_infix ("-",
    `Ce_atom "x",
    `Ce_infix ("-",`Ce_atom "y",`Ce_atom "z")
  )
;;
pe (string_of_cexpr e1) ;;
pe (string_of_cexpr e2) ;;
pe (string_of_cexpr e3) ;;
pe (string_of_cexpr e4) ;;
pe (string_of_cexpr e5) ;;

let pt t = pe ("  " ^ (string_of_ctype t))
let pd n t = pe ("  " ^ (string_of_cdecl_type n t))
let pv n t = pe ("  "^ (string_of_cdecl_type n (`Cdt_value t)))

let int_t =  `Ct_base "int"
let long_t = `Ct_base "long"
let pi_t = `Ct_ptr int_t
let ai_t = `Ct_varray int_t
let pai_t = `Ct_ptr ai_t
let api_t = `Ct_varray pi_t
let a6a4i_t = `Ct_array (6,`Ct_array(4,int_t))
;;

pv "pi" pi_t;;
pv "ai" ai_t;;
pv "pai" pai_t;;
pv "api" api_t;;
pv "a6a4i" a6a4i_t;;


pt int_t;;
pt long_t;;
pt pi_t;;

pe "//int * const * volatile *";;
pt (`Ct_vptr (`Ct_cptr pi_t));;

pd "cri" (`Cdt_cref int_t);;
pv "a" (`Ct_array (9,int_t));;

let f = `Ct_fun(int_t,[long_t]);;
pe "//int f(long)";;
pv "f" f;;

pe "//int (*pf)(long)";;
pv "pf" (`Ct_ptr f);;

pe "//int *g(long)";;
let g = `Ct_fun(`Ct_ptr int_t,[long_t]);;
pv "g" g;;

pe "//f is a function of int returning a pointer to a function of long returning int";;
pe "//int (*ff(int))(long)";;
let ff = `Ct_fun(`Ct_ptr f,[int_t]);;
pv "ff" ff;;


(* reduce type check *)

let u = `BTYP_tuple [];;
let u2= `BTYP_tuple [u;u];;
let u4 = `BTYP_tuple [u2;u2];;

print_endline ("u4="^Flx_print.string_of_btypecode syms.Flx_mtypes2.dfns u4);;
let u4 = Flx_maps.reduce_type u4 in
print_endline ("u4="^Flx_print.string_of_btypecode syms.Flx_mtypes2.dfns u4);;


*)
