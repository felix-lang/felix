(* A simple lexer. *)

{
open Sex_token
open Sex_parse
let lexeme = Lexing.lexeme
let substr = String.sub
let len = String.length
}
(* special characters *)
let quote = '\''
let dquote = '"'
let slosh = '\\'
let linefeed = '\n'
let tab = '\t'
let space = ' '
let formfeed = '\012'
let vtab = '\011'
let carriage_return = '\013'
let underscore = '_'

(* character sets *)
let bindigit = ['0'-'1']
let octdigit = ['0'-'7']
let digit = ['0'-'9']
let hexdigit = digit | ['A'-'F'] | ['a'-'f']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper
let hichar = ['\128'-'\255']
let white = space | tab
let idletter = letter | underscore

(* nasty: form control characters *)
let form_control = linefeed | carriage_return | vtab | formfeed
let newline_prefix = linefeed | carriage_return
let newline = formfeed | linefeed  | carriage_return linefeed
let hash = '#'

let lb = "("
let rb = ")"

let symchar =
  '!' | '$' | '%' | '&' | '*' |
  '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' |
  '=' | '>' | '?' | '@' | '[' | ']' | '^' |
  '`' | '{' | '|' | '}' | '~' | '#' | '\\'

let ordinary = letter | digit | hichar | symchar | lb | rb

let printable = ordinary | quote | dquote

let alphanum = idletter | digit
let identifier = letter alphanum *
let integer = digit +
let sqstring = quote ordinary * quote
let dqstring = dquote ordinary * dquote
let string = sqstring | dqstring

let whitespace = (white | newline) +

rule sex_lex state = parse
| whitespace { sex_lex state lexbuf }
| identifier { ID (lexeme lexbuf) }
| string  { STR (lexeme lexbuf) }
| integer { INT (lexeme lexbuf) }
| lb { LB }
| rb { RB }
| symchar + { SYM (lexeme lexbuf) }
| eof { EOF }
| _ { failwith "lexing failed" }

{
}
