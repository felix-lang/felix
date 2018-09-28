{
open Calc_parser
exception Eof
}
rule token = parse
    [' ' '\t']        { token lexbuf }     (* skip blanks *)
  | ['\n' ]           { EOL }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'               { OP("+") }
  | '-'               { OP("-") }
  | '*'               { OP("*") }
  | '/'               { OP("/") }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | eof | 'q' | 'e'   { raise Eof }
