open Flx_ctypes
exception Unknown_prec of string

let iter = List.iter
let map = List.map
let find = Hashtbl.find
let strcat = String.concat
let add = Hashtbl.add

let precedence = [
  "atom";
  "primary";
  "postfix";
  "unary";
  "cast";
  "pm";
  "mult";
  "add";
  "shift";
  "rel";
  "eq";
  "band";
  "bxor";
  "bor";
  "and";
  "xor";
  "or";
  "cond";
  "assign";
  "comma";
  "expr";
]

let postfix_cops = [
  "++","postfix";
  "--","postfix";
]

let prefix_cops = [
  "~","primary";
  "+","unary";
  "-","unary";
  "!","unary";
  "&","unary";
  "*","unary";
  "++","unary";
  "--","unary";
  "sizeof","unary";
]

let infix_cops = [
  "+","add";
  "-","add";
  "*","mult";
  "/","mult";
  "%","mult";
  "<<","shift";
  ">>","shift";

  "&","band";
  "|","bor";
  "^","bxor";

  "&&","and";
  "||","or";

  "+=","assign";
  "-=","assign";
  "*=","assign";
  "/=","assign";
  "%=","assign";
  "<<=","assign";
  ">>=","assign";
  "&=","assign";
  "|=","assign";
  "^=","assign";

  "<","rel";
  ">","rel";
  ">=","rel";
  "<=","rel";
  "==","eq";
  "!=","eq";

  ".","postfix";
  "->","postfix";
  ".*","pm";
  "->*","pm";
  ",","comma";
]
;;

let remaps = [
  "$1++",("$1:postfix ++ ","postfix");
  "$1--",("$1:postfix -- ","postfix");

  "~$1",("~$1:unary","unary");
  "+$1",("+ $1:unary","unary");
  "-$1",("- $1:unary","unary");
  "!$1",("!$1:unary","unary");
  "&$1",("& $1:unary","unary");
  "*$1",("*$1:unary","unary");
  "++$1",("++ $1:unary","unary");
  "--$1",("-- $1:unary","unary");
  "$1+$2",("$1:add + $2:mult","add");
  "$1-$2",("$1:add - $2:mult","add");
  "$1*$2",("$1:mult * $2:pm","mult");
  "$1/$2",("$1:mult / $2:pm","mult");
  "$1%$2",("$1:mult % $2:pm","mult");

  "$1<<$2",("$1:shift << $2:band","shift");
  "$1>>$2",("$1:shift >> $2:band","shift");
  "$1&$2",("$1:band & $2:bor","band");
  "$1|$2",("$1:bor | $2:bxor","bor");
  "$1^$2",("$1:bxor ^ $2:and","bxor");
  "$1&&$2",("$1:and && $2:or","and");
  "$1||$2",("$1:or || $2:cond","or");

  "$1+=$2",("$1:cond += $2:assign","assign");
  "$1-=$2",("$1:cond -= $2:assign","assign");
  "$1*=$2",("$1:cond *= $2:assign","assign");
  "$1/=$2",("$1:cond /= $2:assign","assign");
  "$1%=$2",("$1:cond %= $2:assign","assign");
  "$1<<=$2",("$1:cond <<= $2:assign","assign");
  "$1>>=$2",("$1:cond >>= $2:assign","assign");
  "$1&=$2",("$1:cond &= $2:assign","assign");
  "$1|=$2",("$1:cond |= $2:assign","assign");
  "$1^=$2",("$1:cond ^= $2:assign","assign");

  "$1<$2",("($1:rel < $2:shift)","rel");
  "$1>$2",("($1:rel > $2:shift)","rel");
  "$1>=$2",("($1:rel >= $2:shift)","rel");
  "$1<=$2",("($1:rel <= $2:shift)","rel");
  "$1==$2",("($1:eq == $2:rel)","eq");
  "$1!=$2",("($1:eq != $2:rel)","eq");

  "$1($2)",("$1:postfix($2:assign)","postfix");
  "$1[$2]",("$1:postfix[$2:expr]","postfix");
  "$1->$2",("$1:postfix->$2:atom","postfix");
  "$1.*$2",("$1:pm.*$2:cast","pm");
  "$1->*$2",("$1:pm->*$2:cast","pm");
  "$1:comma,$2:comma",("$1,$2","comma");
]
;;

let prec = Hashtbl.create 17
let infix = Hashtbl.create 31
let prefix = Hashtbl.create 17
let postfix = Hashtbl.create 17
let prec_remap = Hashtbl.create 31
let seq = ref 0
;;
let find_prec p =
  try Hashtbl.find prec p
  with Not_found ->
    raise (Unknown_prec p)
;;

iter (fun x -> add prec x !seq; incr seq; incr seq) precedence;
iter (fun (n,p) -> add infix n (find_prec p)) infix_cops;
iter (fun (n,p) -> add prefix n (find_prec p)) prefix_cops;
iter (fun (n,p) -> add postfix n (find_prec p)) postfix_cops;
iter (fun (k,v) -> add prec_remap k v) remaps
;;

let pr cop =
 match cop with
  | `Ce_atom _ -> 0
  | `Ce_postfix (s,_) -> find postfix s
  | `Ce_prefix (s,_) -> find prefix s
  | `Ce_infix (s,_,_) -> find infix s

  | `Ce_call _
  | `Ce_array _ -> find_prec "postfix"

  | `Ce_new _ -> find_prec "unary"
  | `Ce_cast _ -> find_prec "cast"
  | `Ce_cond _ -> find_prec "cond"
  | `Ce_expr (p,_) -> find_prec p

let commaprec = find_prec "comma"
let rec comma es = "(" ^ strcat ", " (map (cep commaprec) es) ^ ")"
and comma_opt = function | [] -> "" | ps -> comma ps

(* we need brackets if the binding looseness is higher
than or equal to the context.

But due associativity, (x+y)+z = x+y+z, and we make that
happen by making the context of the LHS subexpression
slightly higher.
*)
and cep cp e =
  let ep = pr e in
  let rce e = cep ep e and lce e = cep (ep+1) e in
  let need_brackets = ep > cp in
  (if need_brackets then "(" else "")
  ^
  begin match e with
    | `Ce_atom s -> s
    | `Ce_postfix (s,e) -> rce e ^ s
    | `Ce_prefix (s,e) -> s ^ rce e
    | `Ce_infix (s,e1,e2) -> lce e1 ^ s ^ rce e2

    | `Ce_call (f,es) -> rce f ^ comma es
    | `Ce_array (f,e) -> rce f ^ "[" ^ lce e ^ "]"
    | `Ce_new (ps,cls,args) ->
        "new" ^ comma_opt ps ^ " " ^ cls ^ " " ^ comma_opt args
    | `Ce_cast (cast,e) -> "(" ^ cast ^ ")" ^ rce e
    | `Ce_cond (e,e1,e2) -> lce e ^ "?" ^ rce e1 ^ ":" ^ rce e2
    | `Ce_expr (_, s) -> s
  end
  ^
  (if need_brackets then ")" else "")

let ce_atom s = `Ce_atom s
let ce_postfix o e = `Ce_postfix (o,e)
let ce_prefix o e = `Ce_prefix (o,e)
let ce_infix o a b = `Ce_infix (o,a,b)
let ce_call a b = `Ce_call (a,b)
let ce_array a b = `Ce_array (a,b)
let ce_new p c a = `Ce_new (p,c,a)
let ce_cast s e = `Ce_cast (s,e)
let ce_cond c a b = `Ce_cond (c,a,b)
let ce_expr p s = `Ce_expr (p,s)
let ce_top s = ce_expr "expr" s
let ce_dot e s = ce_infix "." e (ce_atom s)
let ce_arrow e s = ce_infix "->" e (ce_atom s)

let string_of_cexpr e = cep 1000 e
let sc p e = cep (find_prec p) e
let ce p s = ce_expr p s

let genprec ct prec =
  try Hashtbl.find prec_remap ct
  with Not_found -> ct,prec
