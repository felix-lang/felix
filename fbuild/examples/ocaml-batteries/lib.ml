TYPE_CONV_PATH "Lib"

type foo = A | B of int with sexp;;

print_endline (SExpr.string_of_sexp (sexp_of_foo A));;
