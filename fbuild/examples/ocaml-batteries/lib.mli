type foo = A | B of int

val sexp_of_foo: foo -> Sexplib.Sexp.t
