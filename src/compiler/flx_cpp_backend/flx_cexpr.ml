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
  "$1($2)",("$1:postfix($2:assign)","postfix");
  "$1[$2]",("$1:postfix[$2:expr]","postfix");
  "$1->$2",("$1:postfix->$2:atom","postfix");

  "$1.*$2",("$1:pm.*$2:cast","pm");
  "$1->*$2",("$1:pm->*$2:cast","pm");

  "~$1",("~$1:unary","unary");
  "+$1",("+ $1:unary","unary");
  "-$1",("- $1:unary","unary");
  "!$1",("!$1:unary","unary");
  "&$1",("& $1:unary","unary");
  "*$1",("*$1:unary","unary");
  "++$1",("++ $1:unary","unary");
  "--$1",("-- $1:unary","unary");

  "$1*$2",("$1:mult * $2:pm","mult");
  "$1/$2",("$1:mult / $2:pm","mult");
  "$1%$2",("$1:mult % $2:pm","mult");

  "$1+$2",("$1:add + $2:mult","add");
  "$1-$2",("$1:add - $2:mult","add");

  "$1<<$2",("$1:shift << $2:add","shift");
  "$1>>$2",("$1:shift >> $2:add","shift");

  "$1<$2",("($1:rel < $2:shift)","rel");
  "$1>$2",("($1:rel > $2:shift)","rel");
  "$1>=$2",("($1:rel >= $2:shift)","rel");
  "$1<=$2",("($1:rel <= $2:shift)","rel");

  "$1==$2",("($1:eq == $2:rel)","eq");
  "$1!=$2",("($1:eq != $2:rel)","eq");

  "$1&$2",("$1:band & $2:eq","band");
  "$1|$2",("$1:bor | $2:band","bor");
  "$1^$2",("$1:bxor ^ $2:bor","bxor");

  "$1&&$2",("$1:and && $2:bxor","and");
  "$1||$2",("$1:or || $2:and","or");


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
  "$1:comma,$2:comma",("$1,$2","comma");

  (* common library stuff: a hack but safe, prolly should fix in library*)
  "&::std::cout",("&::std::cout","unary");
  "&::std::cerr",("&::std::cerr","unary");
  "$1.size()",("$1:postfix.size()","postfix");
  "$1.data[$2]",("$1:postfix.data[$2:expr]","postfix");

  "::flx::rtl::strutil::str<int>($1)",("::flx::rtl::strutil::str<int>($1:assign)","postfix");
  "::flx::rtl::strutil::str<#1>($1)",("::flx::rtl::strutil::str<#1>($1:assign)","postfix");
  "static_cast<#0>($1)",("static_cast<#0>($1:assign)","postfix");
  "reinterpret<?1>($1)",("reinterpret<?1>($1:assign)","postfix");
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

  | `Ce_add (a,b)
  | `Ce_sub (a,b) -> find_prec "add"
  | `Ce_div (a,b)
  | `Ce_mul (a,b)
  | `Ce_rmd (a,b) -> find_prec "mult"

  | `Ce_neg a -> find_prec "unary"
  | `Ce_int i -> 0


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
  let need_brackets = ep >= cp in
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
    | `Ce_cond (e,e1,e2) -> lce e ^ " ? " ^ rce e1 ^ " : " ^ rce e2
    | `Ce_expr (_, s) -> s

    | `Ce_int i -> string_of_int i
    | `Ce_add (a,b) -> lce a ^ "+" ^ rce b
    | `Ce_sub (a,b) -> lce a ^ "-" ^ rce b
    | `Ce_mul (a,b) -> lce a ^ "*" ^ rce b
    | `Ce_div (a,b) -> lce a ^ "/" ^ rce b
    | `Ce_rmd (a,b) -> lce a ^ "%" ^ rce b
    | `Ce_neg a  -> "-" ^ rce a

  end
  ^
  (if need_brackets then ")" else "")

let alnum = "ABCDEFGHIJKLMNOPQRSTUVWXYabcdefghijklmnopqrstuvwxyz0123456789_:"

let is_id x =
  try 
   for i = 0 to String.length x - 1 do 
     if not (String.contains alnum (x.[i]))
     then raise Not_found  else ()
   done
   ; 
   true 
  with  Not_found -> false


let ce_atom s = `Ce_atom s
let ce_postfix o e = `Ce_postfix (o,e)
let ce_prefix o e = `Ce_prefix (o,e)
let ce_infix o a b = `Ce_infix (o,a,b)
let ce_call a b = `Ce_call (a,b)
let ce_array a b = `Ce_array (a,b)
let ce_new p c a = `Ce_new (p,c,a)
let ce_cast s e = if is_id s then `Ce_call (`Ce_atom s, [e]) else `Ce_cast (s,e)
let ce_cond c a b = `Ce_cond (c,a,b)
let ce_expr p s = `Ce_expr (p,s)
let ce_top s = ce_expr "expr" s
let ce_dot e s = ce_infix "." e (ce_atom s)
let ce_arrow e s = ce_infix "->" e (ce_atom s)

let ce_add a b = `Ce_add (a,b)
let ce_sub a b = `Ce_sub (a,b)
let ce_mul a b = `Ce_mul (a,b)
let ce_div a b = `Ce_div (a,b)
let ce_rmd a b = `Ce_rmd (a,b)
let ce_neg a  = `Ce_neg a
let ce_int i = `Ce_int i

let sc p e = cep (find_prec p) e
let ce p s = ce_expr p s

(* name(...) form *)
let check_apl ct =
  let n = String.length ct in
  let bcount = ref 0 in
  let mode = ref "id" in
  for i = 0 to n - 2 do
     if !mode = "id" then
       if ct.[i] = '(' then 
         mode := "scan"
       else 
         if String.contains alnum ct.[i] then () 
         else raise Not_found
     else begin
       if ct.[i]='(' then incr bcount
       else if ct.[i]=')' then if !bcount > 0 then decr bcount else raise Not_found
       else ()
    end
  done;
  if !mode="scan" && !bcount=0 && ct.[n-1] = ')' 
  then begin 
    ct, "postfix" 
  end
  else raise Not_found

let genprec ct prec =
  assert (ct <> "");
  if is_id ct then ct,"atom" else
  let n = String.length ct in
  if ct.[n-1] = ';' then begin ct, prec end else
  try 
    let ct,prec = Hashtbl.find prec_remap ct in
    ct,prec

  with Not_found -> 
  try 
    let ct,prec = check_apl ct in
    (* print_endline ("Fun cal found prec " ^ prec ^ " for " ^ ct); *)
    ct,prec
  with Not_found -> 
    (* print_endline ("Fun cal FAILED, using prec " ^ prec ^ " for " ^ ct); *)
    ct,prec

(* WARNING: These reductions only work for code that obeys C semantics, i.e. it will
   NOT work for all possible overloads of C++ operations: it should be OK provided
   the C++ code follows the C semantic mode. Eg x + 0 -> x is always ok in C.
   Similarly !(x = y) -> x != y is always ok in C.
   In C++ anything could happen.

   Our excuse here is that these combinators can ONLY be introduce by Felix
   compiler OR Felix code combining bound operations. The compiler always generates
   code for C.

   Felix programmers combining stuff bound to C++ code should ensure that non-conforming
   operations are OPAQUE. Generally, this will be true because the combinations
   are all done with string substitution, not these combinators.

   We need to be a bit careful the Felix compiler doesn't analyse C binding strings
   too hard and try to remodel them with combinators. It already DOES analyse them
   in order to establish precedences. See above!
*)
let rec reduce (e:cexpr_t) : cexpr_t =
  let r e = reduce e in
  match e with (* this would be better if it were bottom up rather than top down *)
  (* flip comparisons *)
  | `Ce_prefix ("!", (`Ce_infix ("==", e1, e2))) -> r (`Ce_infix ("!=", e1, e2))
  | `Ce_prefix ("!", (`Ce_infix ("!=", e1, e2))) -> r (`Ce_infix ("==", e1, e2))

  | `Ce_prefix (s,e) -> `Ce_prefix (s,r e)

  | `Ce_infix (s,e1,e2) -> `Ce_infix (s,r e1, r e2)

  | `Ce_postfix (s,e) -> `Ce_postfix (s, r e)
  | `Ce_call (e1,es) -> `Ce_call (r e1, List.map r es)
  | `Ce_array (e1,e2) -> `Ce_array (r e1, r e2)
  | `Ce_cond (c,e1,e2) -> `Ce_cond (r c, r e1, r e2)
  | `Ce_cast(s,e) -> `Ce_cast (s, r e)

  | _ -> e

  
let string_of_cexpr e = cep 1000 (reduce e)

 

