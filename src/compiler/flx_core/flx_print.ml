open Flx_util
open Flx_ast
open Flx_types
open Flx_typing
open List

let rec string_of_string s = Flx_string.c_quote_of_string s

let string_of_char c =
  if c = -1 then "<<EOF>>" else
  if c < 32 || c > 126
  then "\\x" ^ Flx_string.hex2 c
  else String.make 1 (Char.chr c)


let suffix_of_type s = match s with
  | "tiny" -> "t"
  | "short" -> "s"
  | "int" -> ""
  | "long" -> "l"
  | "vlong" -> "v"
  | "utiny" -> "tu"
  | "ushort" -> "su"
  | "uint" -> ""
  | "ulong" -> "lu"
  | "uvlong" -> "vu"
  | "int8" -> "i8"
  | "int16" -> "i16"
  | "int32" -> "i32"
  | "int64" -> "i64"
  | "uint8" -> "u8"
  | "uint16" -> "u16"
  | "uint32" -> "u32"
  | "uint64" -> "u64"
  | "float" -> "f"
  | "double" -> ""
  | "ldouble" -> "l"
  | _ -> failwith ("[suffix_of_type] Unexpected Type " ^ s)

let string_of_literal e = match e with
  | `AST_int (s,i) -> (Big_int.string_of_big_int i)^suffix_of_type s
  | `AST_float (t,v) -> v ^ suffix_of_type t
  | `AST_string s -> string_of_string s
  | `AST_cstring s -> "c"^string_of_string s
  | `AST_wstring s -> "w"^string_of_string s
  | `AST_ustring s -> "u"^string_of_string s

let rec string_of_qualified_name (n:qualified_name_t) =
  let se e = string_of_expr e in
  match n with
  | `AST_the (sr,q) -> "the " ^ string_of_qualified_name q
  | `AST_index (sr,name,idx) -> name ^ "<" ^ si idx ^ ">"
  | `AST_void _ -> "void"
  | `AST_name (_,name,ts) -> name ^
    (
      if List.length ts = 0 then ""
      else "[" ^ catmap ", " string_of_typecode ts ^ "]"
    )
  | `AST_case_tag (_,v) -> "case " ^ si v
  | `AST_typed_case (_,v,t) ->
    "(case " ^ si v ^
    " of " ^ string_of_typecode t ^ ")"

  | `AST_lookup (_,(e,name, ts)) -> "("^se e ^")::" ^ name ^
    (if length ts = 0 then "" else
    "[" ^ catmap ", " string_of_typecode ts ^ "]"
    )
  | `AST_callback (_,name) -> "callback " ^string_of_qualified_name name

and string_of_suffixed_name (n:suffixed_name_t) =
  match n with
  | #qualified_name_t as n -> string_of_qualified_name n
  | `AST_suffix (_,(name,suf)) ->
    string_of_qualified_name name ^ " of (" ^ string_of_typecode suf ^ ")"

and string_of_re re =
  match re with
  | `REGEXP_seq (r1,r2) -> string_of_re r1 ^ " " ^ string_of_re r2
  | `REGEXP_alt (r1,r2) -> string_of_re r1 ^ " | " ^ string_of_re r2
  | `REGEXP_aster r -> "(" ^ string_of_re r ^ ")*"
  | `REGEXP_name s -> string_of_qualified_name s
  | `REGEXP_string s ->
    let ss=Buffer.create (String.length s) in
    Buffer.add_char ss '"';
    for i = 0 to String.length s - 1 do
      Buffer.add_string ss (string_of_char (Char.code s.[i]))
    done;
    Buffer.add_char ss '"';
    Buffer.contents ss


  | `REGEXP_epsilon -> "epsilon"
  | `REGEXP_sentinel -> "sentinel"
  | `REGEXP_code e -> "<CODE " ^ string_of_expr e ^ ">"
  | `REGEXP_group (n,r) -> "(" ^ string_of_re r ^ " as " ^ n ^ ")"

and string_of_expr (e:expr_t) =
  let st t = string_of_typecode t in
  let se e = string_of_expr e in
  let sme e = string_of_expr e in
  let sqn e = string_of_qualified_name e in
  match e with
  | #suffixed_name_t as n -> string_of_suffixed_name n
  | `AST_patvar (sr,s) -> "?"^s
  | `AST_patany sr -> "ANY"
  | `AST_vsprintf (sr,s) -> "f"^string_of_string s
  | `AST_ellipsis _ -> "..."
  (*
  | `AST_noexpand (sr,e) -> "noexpand(" ^ string_of_expr e ^ ")"
  *)
  (* because 'noexpand' is too ugly .. *)
  | `AST_noexpand (sr,e) -> string_of_expr e

  | `AST_letin (sr,(pat,e1, e2)) ->
    "let " ^ string_of_letpat pat ^ " = " ^ se e1 ^ " in " ^ se e2
  | `AST_coercion (_,(e,t)) ->
    "(" ^ sme e ^ ":" ^
    string_of_typecode t ^ ")"

  | `AST_expr (_,s,t) ->
    "code ["^string_of_typecode t^"]" ^
    "'" ^ s ^ "'"

  | `AST_cond (_,(e,b1,b2)) ->
    "if " ^ se e ^
    " then " ^ se b1 ^
    " else " ^ se b2 ^
    " endif"

  | `AST_typeof (_,e) -> "typeof("^se e^")"
  | `AST_as (_,(e1, name)) -> "(" ^ se e1 ^ ") as " ^ name
  | `AST_get_n (_,(n,e)) -> "get (" ^ si n ^ ", " ^se e^")"
  | `AST_get_named_variable (_,(n,e)) -> "get (" ^ n ^ ", " ^se e^")"
  | `AST_map (_,f,e) -> "map (" ^ se f ^ ") (" ^ se e ^ ")"
  | `AST_deref (_,e) -> "*(" ^ se e ^ ")"
  | `AST_ref (_,e) -> "&" ^ "(" ^ se e ^ ")"
  | `AST_likely (_,e) -> "likely" ^ "(" ^ se e ^ ")"
  | `AST_unlikely (_,e) -> "unlikely" ^ "(" ^ se e ^ ")"
  | `AST_new (_,e) -> "new " ^ "(" ^ se e ^ ")"
  | `AST_literal (_,e) -> string_of_literal e
  | `AST_apply  (_,(fn, arg)) -> "(" ^
    sme fn ^ " " ^
    sme arg ^
    ")"

  | `AST_product (_,ts) ->
     cat "*" (map se ts)

  | `AST_sum (_,ts) ->
     cat "+" (map se ts)

  | `AST_setunion (_,ts) ->
     cat "||" (map se ts)

  | `AST_setintersection (_,ts) ->
     cat "&&" (map se ts)

  | `AST_intersect (_,ts) ->
     cat "&" (map se ts)

  | `AST_isin (_,(a,b)) ->
     sme a ^ " isin " ^ sme b

  | `AST_orlist (_,ts) ->
     cat " or " (map se ts)

  | `AST_andlist (_,ts) ->
     cat " and " (map se ts)

  | `AST_arrow (_,(a,b)) ->
    "(" ^ se a ^ " -> " ^ se b ^ ")"

  | `AST_longarrow (_,(a,b)) ->
    "(" ^ se a ^ " --> " ^ se b ^ ")"

  | `AST_superscript (_,(a,b)) ->
    "(" ^ se a ^ " ^ " ^ se b ^ ")"

  | `AST_tuple (_,t) -> "(" ^ catmap ", " sme t ^ ")"

  | `AST_record (_,ts) -> "struct {" ^
      catmap "; " (fun (s,e) -> s ^ "="^ sme e ^";") ts ^
    "}"

  | `AST_record_type (_,ts) -> "struct {" ^
      catmap "; " (fun (s,t) -> s ^ ":"^ string_of_typecode t ^";") ts ^
    "}"

  | `AST_variant (_,(s,e)) -> "case " ^ s ^ " of (" ^ se e ^ ")"

  | `AST_variant_type (_,ts) -> "union {" ^
      catmap "; " (fun (s,t) -> s ^ " of "^ string_of_typecode t ^";") ts ^
    "}"

  | `AST_arrayof (_,t) -> "[|" ^ catmap ", " sme t ^ "|]"
  (*
  | `AST_dot (_,(e,n,ts)) ->
    "get_" ^ n ^
    (match ts with | [] -> "" | _ -> "[" ^ catmap "," string_of_typecode ts^ "]")^
    "(" ^ se e ^ ")"
  *)

  | `AST_dot (_,(e1,e2)) ->
    "(" ^ se e1 ^ "." ^ se e2 ^  ")"

  | `AST_lambda (_,(vs,paramss,ret, sts)) ->
    "(fun " ^ print_vs vs ^
    catmap " "
    (fun ps -> "(" ^ string_of_parameters ps ^ ")") paramss
    ^
    (match ret with
    | `TYP_none -> ""
    | _ -> ": " ^string_of_typecode ret) ^
    " = " ^
    string_of_compound 0 sts ^ ")"

  | `AST_ctor_arg (_,(cn,e)) ->
    "ctor_arg " ^ sqn cn ^ "(" ^
    se e ^ ")"

  | `AST_case_arg (_,(n,e)) ->
    "case_arg " ^ si n ^ "(" ^
    se e ^ ")"

  | `AST_case_index (_,e) ->
    "caseno (" ^ se e ^ ")"

  | `AST_match_ctor (_,(cn,e)) ->
    "match_ctor " ^ sqn cn ^ "(" ^
    se e ^ ")"

  | `AST_match_case (_,(v,e)) ->
    "match_case " ^ si v ^ "(" ^
    se e ^ ")"

  | `AST_match (_,(e, ps)) ->
    "match " ^ se e ^ " with\n" ^
    catmap "\n"
    (fun (p,e')->
      " | " ^
      string_of_pattern p ^
      " => " ^
      string_of_expr e'
    )
    ps
    ^
    " endmatch"

(*
  | `AST_type_match (_,(e, ps)) ->
    "typematch " ^ string_of_typecode e ^ " with " ^
    catmap "\n"
    (fun (p,e')->
      " | " ^
      string_of_tpattern p ^
      " => " ^
      string_of_typecode e'
    )
    ps
    ^
    " endmatch"
*)

  | `AST_type_match (_,(e, ps)) ->
    "typematch " ^ string_of_typecode e ^ " with " ^
    catmap ""
    (fun (p,e')->
      "\n  | " ^
      string_of_typecode p ^
      " => " ^
      string_of_typecode e'
    )
    ps
    ^
    "\n endmatch"

  | `AST_macro_ctor (_,(s,e)) ->
    "macro ctor " ^ s ^ string_of_expr e

  | `AST_macro_statements (_,ss) ->
    "macro statements begin\n" ^
    catmap "\n" (string_of_statement 1) ss ^ "\nend"

  | `AST_user_expr (_,name,term) ->
    let body = string_of_ast_term 0 term in
    "User expr " ^ name ^ "(" ^ body ^ ")"

(* precedences for type operators ..
   0 -- atomic
   0.5 -- indexing t[i]
   1 -- pointer
   2 -- application
   3 -- ^
   4 -- *
   5 -- +
   6 -- isin
   7 .. and
   8 .. or
   9 -- ->
   10 -- =>
   11    as, all
*)


and st prec tc : string =
  let iprec,txt =
    match tc with
    | #suffixed_name_t as t -> 0,string_of_suffixed_name t
    | `AST_patvar (sr,s) -> 0,"?"^s
    | `AST_patany sr -> 0,"ANY"
    | `TYP_none -> 0,"<none>"
    | `TYP_ellipsis-> 0,"..."

    | `TYP_type_match (e,ps) -> 0,
      "typematch " ^ string_of_typecode e ^ " with " ^
      catmap ""
      (fun (p,t) ->
      "\n  | " ^ string_of_typecode p ^ " => " ^ string_of_typecode t
      )
      ps
      ^
      "\nendmatch"

    | `TYP_var i -> 0,"<var " ^ si i ^ ">"
    | `TYP_unitsum k ->
      0,
      begin match k with
      | 0 -> "void"
      | 1 -> "unit"
      | 2 -> "bool"
      | _ -> si k
      end

    | `TYP_tuple ls ->
      begin match ls with
      | [] -> 0,"unit"
      | _ -> 4, cat " * " (map (st 4) ls)
      end

    | `TYP_record ls ->
      begin match ls with
      | [] -> 0,"unit"
      | _ -> 0, "struct {" ^ catmap "" (fun (s,t)->s^":"^st 0 t ^"; ") ls ^ "}"
      end

    | `TYP_variant ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ -> 0, "union {" ^ catmap "" (fun (s,t)->s^" of "^st 0 t ^"; ") ls ^ "}"
      end

    | `TYP_sum ls ->
      begin match ls with
      | [] -> 0,"void"
      | [`TYP_tuple[];`TYP_tuple[]] -> 0,"bool"
      | _ -> 5,cat " + " (map (st 5) ls)
      end

    | `TYP_typeset ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ -> 0,"{" ^ cat ", " (map (st 0) ls) ^  "}"
      end

    | `TYP_intersect ls ->
      let ls = filter (fun t -> t <> `TYP_tuple []) ls in
      begin match ls with
      | [] -> 0,"unit"
      | _ -> 9,cat " & " (map (st 9) ls)
      end

    | `TYP_setintersection ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ -> 9,cat " && " (map (st 9) ls)
      end

    | `TYP_setunion ls ->
      begin match ls with
      | [] -> 0,"unit"
      | _ -> 9,cat " || " (map (st 9) ls)
      end

    | `TYP_function (args, result) ->
      9,st 9 args ^ " -> " ^ st 9 result

    | `TYP_cfunction (args, result) ->
      9,st 9 args ^ " --> " ^ st 9 result

    | `TYP_array (vt,it) -> 3, st 1 vt ^ "^" ^ st 3 it

    | `TYP_pointer t -> 1,"&" ^ st 1 t
(*    | `TYP_lvalue t -> 0,"lvalue[" ^ st 1 t ^"]" *)

    | `TYP_typeof e -> 0,"typeof(" ^ string_of_expr e ^ ")"
    | `TYP_as (t,s) -> 11,st 11 t ^ " as " ^ s

    | `TYP_proj (i,t) -> 2,"proj_"^si i^" "^ st 2 t
    | `TYP_dual t -> 2,"~"^ st 2 t
    | `TYP_dom t -> 2,"dom "^ st 2 t
    | `TYP_cod t -> 2,"cod "^st 2 t
    | `TYP_case_arg (i,t) -> 2,"case_arg_"^si i^" "^st 2 t

    | `TYP_isin (t1,t2) -> 6,st 2 t1 ^ " isin " ^ st 6 t2

    | `TYP_apply (t1,t2) -> 2,st 2 t1 ^ " " ^ st 2 t2
    | `TYP_type -> 0,"TYPE"
    | `TYP_type_tuple ls ->
      4, cat ", " (map (st 4) ls)

    | `TYP_typefun (args,ret,body) ->
       10,
       (
         "fun(" ^ cat ", "
         (
           map
           (fun (n,t)-> n ^ ": " ^ st 10 t)
           args
         ) ^
         "): " ^ st 0 ret ^ "=" ^ st 10 body
       )
  in
    if iprec >= prec
    then "(" ^ txt ^ ")"
    else txt

and string_of_typecode tc = st 99 tc

and qualified_name_of_index_with_vs dfns index =
  match Hashtbl.find dfns index with
  | { id=id; vs=vs; parent=parent } ->
    match parent with
    | Some index' ->
      qualified_name_of_index_with_vs dfns index' ^
      id ^
      print_ivs vs ^
      "::"
    | None -> ""
      (* If this entity has no parent, its the root module,
        and we don't bother to print its name as a prefix
      *)

and qualified_name_of_index' dfns index =
  match Hashtbl.find dfns index with
  | { id=id; parent=parent } ->
    begin match parent with
    | Some index' -> qualified_name_of_index_with_vs dfns index'
    | None -> ""
    end ^
    id

and qualified_name_of_index dfns index =
  try qualified_name_of_index' dfns index ^ "<"^si index ^">"
  with Not_found -> "index_"^ si index

and get_name_parent dfns bbdfns index =
  try
    match Hashtbl.find dfns index with
    { id=id; vs=vs; parent=parent} -> id, parent
  with Not_found ->
  try
    match Hashtbl.find bbdfns index with
    id,parent,_,_ -> id,parent
  with Not_found -> "index_" ^ string_of_int index,None


and qualified_name_of_bindex dfns bbdfns index =
  let name,parent = get_name_parent dfns bbdfns index in
  match parent with
  | Some index' ->
    qualified_name_of_bindex dfns bbdfns index' ^ "::" ^ name
  | None -> name

and bound_name_of_bindex dfns bbdfns index =
  let name,parent = get_name_parent dfns bbdfns index in
  name ^ "<" ^ si index ^ ">"

(* fixppoint labeller .. very sloppy, ignores precedence .. *)
and get_label i =
  if i = 0 then ""
  else
    let ch = Char.chr (i mod 26 + Char.code('a')-1) in
    get_label (i/26) ^ String.make 1 ch

and print_fixpoints depth fixlist =
  match fixlist with
  | (d,lab) :: t when d = depth ->
    let txt,lst = print_fixpoints depth t in
    " as " ^ lab ^ " " ^ txt, lst
  | _ -> "", fixlist

and sb dfns depth fixlist counter prec tc =
  let sbt prec t = sb dfns (depth+1) fixlist counter prec t in
  let iprec, term =
    match tc with
    | `BTYP_type_match (t,ps) ->
      0,
      (
        "typematch " ^
        sbt 99 t ^
        " with" ^
        catmap ""
        (fun ({pattern=p},t) ->
          "\n  | " ^ sbt 99 p ^ " => " ^ sbt 99 t
        )
        ps
        ^
        "\nendmatch"
      )

    | `BTYP_fix i ->
       0,
       (
         try assoc (depth+i) !fixlist
         with Not_found ->
           incr counter; (* 'a is 1 anyhow .. *)
           let lab = "fix" ^ si i ^ "_"^get_label !counter in
           fixlist := (depth+i,lab) :: !fixlist;
           lab
       )

    | `BTYP_var (i,mt) -> 0,"<T" ^ si i ^
      (match mt with `BTYP_type i ->"" | _ -> ":"^sbt 0 mt)^
      ">"

    | `BTYP_inst (i,ts) ->
      0,qualified_name_of_index dfns i ^
      (if List.length ts = 0 then "" else
      "[" ^cat ", " (map (sbt 9) ts) ^ "]"
      )

    | `BTYP_tuple ls ->
      begin match ls with
      | [] -> 0,"unit"
      | [x] -> failwith ("UNEXPECTED TUPLE OF ONE ARGUMENT " ^ sbt 9 x)
      | _ -> 4,cat " * " (map (sbt 4) ls)
      end

    | `BTYP_record ls ->
      begin match ls with
      | [] -> 0,"record_unit"
      | _ -> 0,"struct {"^catmap "" (fun (s,t)->s^":"^sbt 0 t^";") ls ^"}"
      end

    | `BTYP_variant ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ -> 0,"union {"^catmap "" (fun (s,t)->s^" of "^sbt 0 t^";") ls ^"}"
      end

    | `BTYP_unitsum k ->
      begin match k with
      | 0 -> 0,"/*unitsum*/void"
      | 2 -> 0,"bool"
      | _ -> 0,si k
      end

    | `BTYP_sum ls ->
      begin match ls with
      | [] -> 9,"UNEXPECTED EMPTY SUM = void"
      | [`BTYP_tuple[]; `BTYP_tuple[]] -> 0,"unexpected bool"
      | [x] -> (* failwith *) (9,"UNEXPECTED SUM OF ONE ARGUMENT " ^ sbt 9 x)
      | _ ->
        if (all_units ls)
        then
          0,si (length ls)
        else
          5,cat " + " (map (sbt 5) ls)
      end

    | `BTYP_typeset ls ->
      begin match ls with
      | [] -> 9,"UNEXPECTED EMPTY TYPESET = void"
      | _ ->
          0,"{" ^ cat "," (map (sbt 0) ls) ^ "}"
      end

    | `BTYP_intersect ls ->
      begin match ls with
      | [] -> 9,"/*intersect*/void"
      | _ ->
          4,cat " and " (map (sbt 5) ls)
      end

    | `BTYP_typesetintersection ls ->
      begin match ls with
      | [] -> 9,"/*typesetintersect*/void"
      | _ ->
          4,cat " && " (map (sbt 5) ls)
      end

    | `BTYP_typesetunion ls ->
      begin match ls with
      | [] -> 9,"/*typesetunion*/unit"
      | _ ->
          4,cat " || " (map (sbt 5) ls)
      end

    | `BTYP_function (args, result) ->
      6,(sbt 6 args) ^ " -> " ^ (sbt 6 result)

    | `BTYP_cfunction (args, result) ->
      6,(sbt 6 args) ^ " --> " ^ (sbt 6 result)

    | `BTYP_array (t1,t2) ->
      begin match t2 with
      | `BTYP_unitsum k -> 3, sbt 3 t1 ^"^"^si k
      | _ -> 3, sbt 3 t1 ^"^"^sbt 3 t2
      end

    | `BTYP_pointer t -> 1,"&" ^ sbt 1 t
    | `BTYP_void -> 0,"void"

    | `BTYP_apply (t1,t2) -> 2,sbt 2 t1 ^ " " ^ sbt 2 t2
    | `BTYP_type i -> 0,"TYPE " ^ si i
    | `BTYP_type_tuple ls ->
      begin match ls with
      | [] -> 0,"UNEXPECTED TYPE TUPLE NO ARGS"
      | _ -> 4, cat ", " (map (sbt 4) ls)
      end


    | `BTYP_typefun (args,ret,body) ->
       8,
       (
         "fun (" ^ cat ", "
         (
           map
           (fun (i,t)-> "T"^si i ^ ": " ^ sbt 8 t)
           args
         ) ^
         "): " ^ sbt 0 ret ^ "=" ^ sbt 8 body
       )
  in
    let txt,lst = print_fixpoints depth !fixlist in
    fixlist := lst;
    if txt = "" then
      if iprec >= prec then "(" ^ term ^ ")"
      else term
    else
    "(" ^ term ^ txt ^ ")"

and string_of_btypecode (dfns:symbol_table_t) tc =
  let fixlist = ref [] in
  let term = sb dfns 0 fixlist (ref 0) 99 tc in
  let bad = ref "" in
  while List.length !fixlist > 0 do
    match !fixlist with
    | (d,v)::t ->
      bad := !bad ^ " [Free Fixpoint " ^ si d ^ " " ^ v ^"]";
      fixlist := t
    | [] -> assert false
  done;
  term ^ !bad

and sbt a b = string_of_btypecode a b

and string_of_basic_parameters (ps: simple_parameter_t list) =
  cat
    ", "
    (map (fun (x,y)-> x ^ ": "^(string_of_typecode y)) ps)

and string_of_param_kind = function
  | `PVal -> "val"
  | `PVar -> "var"
  | `PRef -> "ref"
  | `PFun -> "fun"

and string_of_parameters (ps:params_t) =
  let ps, traint = ps in
  cat
    ", "
    (map
      (fun (k,x,y,d)->
        string_of_param_kind k^ " " ^
        x ^ ": "^(string_of_typecode y) ^
        (match d with None -> "" | Some e -> "="^ string_of_expr e)
      )
      ps
     )
  ^
  (match traint with
  | Some x -> " where " ^ string_of_expr x
  | None -> ""
  )

(*
and string_of_iparameters dfns ps =
  let ps,traint = ps in
  cat
    ", "
    (map (fun (x,(i,y))-> x ^ "["^si i^"]: "^(string_of_typecode y)) ps)
  ^
  (match traint with
  | Some x ->  " where " ^ sbe dfns bbdfns x
  | None -> ""
  )
*)

and string_of_basic_bparameters dfns ps : string =
  catmap ","
  (fun {pid=x; pkind=kind; pindex=i; ptyp=y}->
    string_of_param_kind kind ^ " " ^ x ^ "<"^si i^">: "^(string_of_btypecode dfns y)
  )
  ps

and string_of_bparameters dfns bbdfns ps : string =
  let ps, traint = ps in
  string_of_basic_bparameters dfns ps
  ^
  (match traint with
  | Some x -> " where " ^ sbe dfns bbdfns x
  | None -> ""
  )

and string_of_arguments ass =
  catmap ", " string_of_expr ass


and string_of_component level (name, typ) =
   spaces level ^ name ^ ": " ^ (string_of_typecode typ)

and string_of_float_pat = function
  | Float_plus (t,v) -> v ^ t
  | Float_minus (t,v) -> "-" ^ v ^ t
  | Float_inf -> "inf"
  | Float_minus_inf -> "-inf"

and string_of_tpattern p =
  let sp p = string_of_tpattern p in
  match p with
  | `TPAT_function (p1,p2) -> sp p1 ^ " -> " ^ sp p2
  | `TPAT_sum ps -> catmap " + " sp ps
  | `TPAT_tuple ps -> catmap " * " sp ps
  | `TPAT_pointer p -> "&" ^ sp p
  | `TPAT_void -> "0"
  | `TPAT_var s -> "?" ^ s
  | `TPAT_name (s,ps) ->
    s ^
    (
      match ps with
      | [] -> ""
      | ps -> "[" ^ catmap "," sp ps ^ "]"
    )

  | `TPAT_as (p,s) -> sp p ^ " as " ^ s
  | `TPAT_any -> "_"
  | `TPAT_unitsum j -> si j
  | `TPAT_type_tuple ps -> catmap ", " sp ps

and string_of_pattern p =
  let se e = string_of_expr e in
  match p with
  | PAT_coercion (_,p,t) -> "(" ^ string_of_pattern p ^ ":" ^ string_of_typecode t ^ ")"
  | PAT_none _ -> "<none>"
  | PAT_nan _ -> "NaN"
  | PAT_int (_,t,i) -> Big_int.string_of_big_int i ^ suffix_of_type t
  | PAT_int_range (_,t1,i1,t2,i2) ->
    Big_int.string_of_big_int i1 ^ suffix_of_type t1 ^
    " .. " ^
    Big_int.string_of_big_int i2 ^ suffix_of_type t2

  | PAT_string (_,s) -> string_of_string s
  | PAT_string_range (_,s1, s2) ->
    string_of_string s1 ^ " .. " ^ string_of_string s2
  | PAT_float_range (_,x1, x2) ->
    string_of_float_pat x1 ^ " .. " ^ string_of_float_pat x2
  | PAT_name (_,s) -> s
  | PAT_tuple (_,ps) -> "(" ^ catmap ", "  string_of_pattern ps ^ ")"
  | PAT_any _ -> "any"
  | PAT_const_ctor (_,s) -> "|" ^ string_of_qualified_name s
  | PAT_nonconst_ctor (_,s,p)-> "|" ^ string_of_qualified_name s ^ " " ^ string_of_pattern p
  | PAT_as (_,p,n) ->
    begin match p with
    | PAT_any _ -> n
    | _ ->
      "(" ^ string_of_pattern p ^ " as " ^ n ^ ")"
    end
  | PAT_when (_,p,e) -> "(" ^ string_of_pattern p ^ " when " ^ se e ^ ")"
  | PAT_record (_,ps) ->
     "struct { " ^ catmap "; " (fun (s,p) -> s ^ "="^string_of_pattern p) ps ^"; }"

and string_of_letpat p =
  match p with
  | PAT_name (_,s) -> s
  | PAT_tuple (_,ps) -> "(" ^ catmap ", "  string_of_letpat ps ^ ")"
  | PAT_any _ -> "_"
  | PAT_const_ctor (_,s) -> "|" ^ string_of_qualified_name s
  | PAT_nonconst_ctor (_,s,p)-> "|" ^ string_of_qualified_name s ^ " " ^ string_of_letpat p
  | PAT_as (_,p,n) -> "(" ^ string_of_pattern p ^ " as " ^ n ^ ")"
  | PAT_record (_,ps) ->
     "struct { " ^ catmap "; " (fun (s,p) -> s ^ "="^string_of_pattern p) ps ^"; }"

  | _ -> failwith "unexpected pattern kind in let/in pattern"

and string_of_compound level ss =
  spaces level ^ "{\n" ^
  catmap "\n" (string_of_statement (level+1)) ss ^ "\n" ^
  spaces level ^ "}"

and short_string_of_compound level ss =
  match ss with
  | [] -> "{}"
  | _ -> "\n"^ string_of_compound level ss

and string_of_asm_compound level ss =
  spaces level ^ "{\n" ^
  catmap "\n" (string_of_asm (level+1)) ss ^ "\n" ^
  spaces level ^ "}"

and short_string_of_asm_compound level ss =
  match ss with
  | [] -> "{}"
  | _ -> "\n"^ string_of_asm_compound level ss

and special_string_of_typecode ty =  (* used for constructors *)
  match ty with
  | `TYP_tuple [] -> ""
  | _ -> " of " ^ string_of_typecode ty

and special_string_of_btypecode dfns ty =  (* used for constructors *)
  match ty with
  | `BTYP_tuple [] -> ""
  | _ -> " of " ^ string_of_btypecode dfns ty

and string_of_macro_parameter_type = function
  | Expr -> "fun"
  | Ident -> "ident"
  | Stmt -> "proc"

and print_ixs = function
  | [] -> ""
  | ixs -> "[" ^ cat ", " ixs ^ "]"

(*
and string_of_maybe_tpattern = function
  | `TPAT_any -> ""
  | t -> ": " ^ string_of_tpattern t
*)

and string_of_maybe_tpattern = function
  | `AST_patany _ -> ""
  | t -> ": " ^ string_of_typecode t

and print_tconstraint = function
  | `TYP_tuple [] -> ""
  | `TYP_intersect [`TYP_tuple []] -> ""
  | t -> let x = string_of_typecode t in
    if x <> "unit" then " where " ^ x else ""

and print_tclass_req qn = string_of_qualified_name qn

and print_tclass_reqs = function
  | [] -> ""
  | t -> " with " ^ catmap "," print_tclass_req t

and print_tcon {raw_type_constraint=tcon; raw_typeclass_reqs=rtcr} =
  print_tconstraint tcon ^ print_tclass_reqs rtcr

and print_ivs (vs,({raw_type_constraint=tcon; raw_typeclass_reqs=rtcr} as con)) =
  match vs,tcon,rtcr with
  | [],`TYP_tuple [],[] -> ""
  | _ ->
    "[" ^ cat ", " (map (fun (name,ix,tpat) -> name ^ string_of_maybe_tpattern tpat) vs) ^
    print_tcon con ^
    "]"

and print_ivs_with_index (vs,({raw_type_constraint=tcon; raw_typeclass_reqs=rtcr} as con)) =
  match vs,tcon,rtcr with
  | [],`TYP_tuple [],[] -> ""
  | _ ->
    "[" ^ cat ", " (map (fun (name,ix,tpat) -> name ^ "<"^si ix^">"^string_of_maybe_tpattern tpat) vs) ^
    print_tcon con ^
    "]"

and print_vs (vs,({raw_type_constraint=tcon; raw_typeclass_reqs=rtcr} as con)) =
  match vs,tcon,rtcr with
  | [],`TYP_tuple [],[] -> ""
  | _ ->
    "[" ^
    cat ", "
    (map (fun (name,tpat) -> name ^ string_of_maybe_tpattern tpat) vs) ^
    print_tcon con ^
    "]"

and print_bvs = function
  | [] -> ""
  | vs ->
    "[" ^
    cat ", "
    (
      map
      (fun (s,i)-> s^"<"^si i^">" )
      vs
    ) ^
    "]"

and print_bvs_cons dfns vs cons = match vs,cons with
 | [],`BTYP_tuple [] -> ""
 | vs,cons ->
   "[" ^ catmap "," (fun (s,i)->s^"<"^si i^">") vs ^
   (if cons = `BTYP_tuple[] then ""
   else " where " ^ sbt dfns cons) ^
   "]"

and print_inst dfns = function
  | [] -> ""
  | ts ->
    "[" ^
    cat ", "
    (
      map (string_of_btypecode dfns) ts
    ) ^
    "]"

and sl x = string_of_lvalue x
and string_of_lvalue (x,t) =
  begin match x with
  | `Val (sr,x) -> "val " ^ x
  | `Var (sr,x) -> "var " ^ x
  | `Name (sr,x) -> x
  | `Skip (sr) -> "_"
  | `List ls -> "(" ^ catmap ", " sl ls ^ ")"
  | `Expr (sr,e) -> string_of_expr e
  end ^
  begin match t with
  | Some t -> ":" ^ string_of_typecode t
  | None -> ""
  end

and string_of_property = function
| `Recursive -> "recursive"
| `Inline -> "inline"
| `Generated s -> "generated " ^ s
| `NoInline -> "noinline"
| `Inlining_started -> "inlining_started"
| `Inlining_complete -> "inlining_complete"
| `Explicit_closure -> "explicit_closure_expression"
| `Stackable -> "stackable"
| `Unstackable -> "unstackable"
| `Heap_closure -> "heap_closure"
| `Stack_closure -> "stack_closure"
| `Pure -> "pure"
| `Uses_global_var-> "uses_global_var"
| `Requires_ptf -> "requires_thread_frame"
| `Not_requires_ptf -> "does_not_require_thread_frame"
| `Uses_gc -> "uses_gc"
| `Ctor -> "ctor"
| `Generator -> "generator"
| `Yields -> "yields"
| `Virtual -> "virtual"
| `Cfun -> "cfun"
| `Lvalue -> "lvalue"

and string_of_properties ps =
  match ps with
  | [] -> ""
  | ps -> catmap " " string_of_property ps ^ " "

and string_of_code_spec = function
  | `StrTemplate s -> "\"" ^ s ^  "\""
  | `Str s -> "c\"" ^ s ^  "\""
  | `Virtual -> "virtual"
  | `Identity -> "identity"

and string_of_long_code_spec c =
  let triple_quote = "\"\"\"" in
  match c with
  | `StrTemplate s -> triple_quote ^ s ^ triple_quote
  | `Str s -> "c" ^ triple_quote ^ s ^ triple_quote
  | `Virtual -> "virtual"
  | `Identity -> "identity"

and string_of_raw_req = function
  | `Named_req s -> string_of_qualified_name s
  | `Body_req c -> "body " ^ string_of_code_spec c
  | `Header_req c -> "header " ^ string_of_code_spec c
  | `Property_req s -> "property \"" ^ s ^ "\""
  | `Package_req c -> "package " ^ string_of_code_spec c

(* fairly lame excess brackets here *)
and string_of_raw_req_expr = function
  | `RREQ_atom r -> string_of_raw_req r
  | `RREQ_and (a,b) -> "(" ^ string_of_raw_req_expr a ^ ") and (" ^ string_of_raw_req_expr b ^")"
  | `RREQ_or (a,b) -> "(" ^ string_of_raw_req_expr a ^ ") or (" ^ string_of_raw_req_expr b ^")"
  | `RREQ_true -> "(true)"
  | `RREQ_false -> "(false)"

(* fairly lame excess brackets here *)
and string_of_named_req_expr = function
  | `NREQ_atom r -> string_of_qualified_name r
  | `NREQ_and (a,b) -> "(" ^ string_of_named_req_expr a ^ ") and (" ^ string_of_named_req_expr b ^")"
  | `NREQ_or (a,b) -> "(" ^ string_of_named_req_expr a ^ ") or (" ^ string_of_named_req_expr b ^")"
  | `NREQ_true -> "(true)"
  | `NREQ_false -> "(false)"

and string_of_raw_reqs x = match x with
  | `RREQ_true -> "" (* required nothing *)
  | x -> " requires " ^ string_of_raw_req_expr x

and string_of_named_reqs x = match x with
  | `NREQ_true -> "" (* requires nothing *)
  | x -> " requires " ^ string_of_named_req_expr x

and string_of_base_qual = function
| `Incomplete -> "incomplete"
| `Pod -> "pod"
| `GC_pointer -> "GC_pointer"

and string_of_qual = function
| #base_type_qual_t as x -> string_of_base_qual x
| `Raw_needs_shape t -> "needs_shape(" ^ string_of_typecode t ^ ")"

and string_of_bqual dfns = function
| #base_type_qual_t as x -> string_of_base_qual x
| `Bound_needs_shape t -> "needs_shape(" ^ string_of_btypecode dfns t ^ ")"

and string_of_quals qs = catmap " " string_of_qual qs
and string_of_bquals dfns qs = catmap " " (string_of_bqual dfns) qs

and string_of_ast_term level (term:ast_term_t) =
  let sast level x = string_of_ast_term level x in
  match term with
  | `Statement_term s -> string_of_statement (level+1) s
  | `Statements_term ss -> catmap "\n" (string_of_statement (level+1)) ss
  | `Expression_term e -> string_of_expr e
  | `Identifier_term s -> s
  | `Keyword_term s -> s
  | `Apply_term (t,ts) -> "apply("^ sast 0 t ^ ",(" ^ catmap ", " (sast 0) ts ^ "))"

and string_of_class_component level mem =
  let kind, name, mix,vs,ty,cc = match mem with
  | `MemberVar (name,typ,cc) -> "var",name,None,dfltvs,typ,cc
  | `MemberVal (name,typ,cc) -> "val",name,None,dfltvs,typ,cc
  | `MemberFun (name,mix,vs,typ,cc) -> "fun",name,mix,vs,typ,cc
  | `MemberProc (name,mix,vs,typ,cc) -> "proc",name,mix,vs,typ,cc
  | `MemberCtor (name,mix,typ,cc) -> "ctor",name,mix,dfltvs,typ,cc
  in
    (spaces (level+1)) ^
    kind ^ " " ^ name ^ print_vs vs ^ ": " ^ string_of_typecode ty ^
    (match cc with None -> "" | Some cc -> string_of_code_spec cc) ^
    ";"

and string_of_ikind = function
  | `Header -> "header "
  | `Body -> "body "
  | `Package -> "package "

and string_of_axiom_method a = match a with
  | `Predicate e -> string_of_expr e
  | `Equation (l,r) -> string_of_expr l ^ " = " ^ string_of_expr r

and string_of_baxiom_method dfns bbdfns a = match a with
  | `BPredicate e -> string_of_expr e
  | `BEquation (l,r) -> sbe dfns bbdfns l ^ " = " ^ sbe dfns bbdfns r

and string_of_statement level s =
  let se e = string_of_expr e in
  let sqn n = string_of_qualified_name n in
  match s with
  | `AST_seq (_,sts) -> catmap "" (string_of_statement level) sts
  (*
  | `AST_public (_,s,st) ->
    "\n" ^
    spaces level ^ "public '" ^ s ^ "'\n" ^
    string_of_statement (level+1) st
  *)

  | `AST_private (_,st) ->
    spaces level ^ "private " ^
    string_of_statement 0 st

  | `AST_export_fun (_,flx_name,cpp_name) ->
    spaces level ^
    "export fun " ^
    string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | `AST_export_python_fun (_,flx_name,cpp_name) ->
    spaces level ^
    "export python fun " ^
    string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | `AST_export_type (_,flx_type,cpp_name) ->
    spaces level ^
    "export type (" ^
    string_of_typecode flx_type ^
    ") as \"" ^ cpp_name ^ "\";"

  | `AST_label (_,s) -> s ^ ":"
  | `AST_goto (_,s) -> spaces level ^ "goto " ^ s ^ ";"

  | `AST_assert (_,e) -> spaces level ^ "assert " ^ se e ^ ";"

  | `AST_init (_,v,e) ->
    spaces level ^ v ^ " := " ^ se e ^ ";"

  | `AST_comment (_,s) -> spaces level ^ "// " ^ s

  | `AST_open (_,vs,n) ->
    spaces level ^ "open" ^ print_vs vs ^ " " ^ sqn n ^ ";"

  | `AST_inject_module (_,n) ->
    spaces level ^ "inherit " ^ sqn n ^ ";"

  | `AST_include (_,s) ->
    spaces level ^ "include " ^ s ^ ";"

  | `AST_use (_,n,qn) ->
    spaces level ^ "use " ^ n ^ " = " ^ sqn qn ^ ";"

  | `AST_type_alias (_,t1,vs,t2) ->
    spaces level ^ "typedef " ^ t1 ^ print_vs vs ^
    " = " ^
    string_of_typecode t2 ^ ";"

  | `AST_inherit (_,name,vs,qn) ->
    spaces level ^ "inherit " ^ name ^ print_vs vs ^
    " = " ^
    string_of_qualified_name qn ^ ";"

  | `AST_inherit_fun (_,name,vs,qn) ->
    spaces level ^ "inherit fun " ^ name ^ print_vs vs ^
    " = " ^
    string_of_qualified_name qn ^ ";"

  | `AST_untyped_module (_,name, vs,sts)  ->
    spaces level ^ "module " ^ name ^ print_vs vs ^
    " = " ^
    "\n" ^
    string_of_compound level sts

  | `AST_struct (_,name, vs, cs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ name ^ ": " ^ string_of_typecode ty ^ ";"
    in
    spaces level ^ "struct " ^ name ^ print_vs vs ^ " = " ^
    spaces level ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    spaces level ^ "}"

  | `AST_cstruct (_,name, vs, cs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ name ^ ": " ^ string_of_typecode ty ^ ";"
    in
    spaces level ^ "cstruct " ^ name ^ print_vs vs ^ " = " ^
    spaces level ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    spaces level ^ "}"

  | `AST_typeclass (_,name, vs, sts) ->
    spaces level ^ "typeclass " ^ name ^ print_vs vs ^ " = " ^
    string_of_compound level sts

  | `AST_instance (_,vs,name, sts) ->
    spaces level ^ "instance " ^ print_vs vs ^ " " ^
    string_of_qualified_name name ^ " = " ^
    string_of_compound level sts

  | `AST_union (_,name, vs,cs) ->
    let string_of_union_component (name,cval, vs,ty) =
      (spaces (level+1)) ^ "|" ^ name ^
      (match cval with None -> "" | Some i -> "="^ si i) ^
      special_string_of_typecode ty
    in
    spaces level ^ "union " ^ name ^ print_vs vs ^ " = " ^
    spaces level ^ "{\n" ^
    catmap ";\n" string_of_union_component cs ^ "\n" ^
    spaces level ^ "}"

  | `AST_ctypes (_,names, quals, reqs) -> spaces level ^
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "ctypes " ^ catmap "," snd names ^
    string_of_raw_reqs reqs ^
    ";"

  | `AST_abs_decl (_,t,vs, quals, ct, reqs) -> spaces level ^
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "type " ^ t ^ print_vs vs ^
    " = " ^ string_of_code_spec ct ^
    string_of_raw_reqs reqs ^
    ";"

  | `AST_newtype (_,t,vs, nt) -> spaces level ^
    "type " ^ t ^ print_vs vs ^
    " = new " ^ string_of_typecode nt ^
    ";"

  | `AST_callback_decl (_,name,args,result, reqs) -> spaces level ^
    "callback " ^ name ^ ": " ^
    (string_of_typecode (`TYP_tuple args)) ^ " -> " ^
    (string_of_typecode result) ^
    string_of_raw_reqs reqs ^
    ";"

  | `AST_fun_decl (_,name,vs,args, result, code, reqs,prec) ->
    spaces level ^
    "fun " ^ name ^ print_vs vs ^
    ": " ^
    (string_of_typecode (`TYP_tuple args)) ^ " -> " ^
    (string_of_typecode result) ^
    " = " ^ string_of_code_spec code ^
    (if prec = "" then "" else ":"^prec^" ")^
    string_of_raw_reqs reqs ^
    ";"

  | `AST_const_decl (_,name,vs,typ, code, reqs) ->
    spaces level ^
     "const " ^ name ^
     ": " ^ string_of_typecode typ ^
     " = "^string_of_code_spec code^
     string_of_raw_reqs reqs ^
     ";"

  | `AST_insert (_,n,vs,s, ikind, reqs) ->
    spaces level ^ string_of_ikind ikind ^
    n^print_vs vs^
    "\n" ^ string_of_code_spec s ^ " " ^
     string_of_raw_reqs reqs ^
    ";\n"

  | `AST_code (_,s) ->
    "code \n" ^ string_of_long_code_spec s ^ ";\n"

  | `AST_noreturn_code (_,s) ->
    "noreturn_code \n" ^ string_of_long_code_spec s ^ ";\n"

  | `AST_reduce (_,name, vs, ps, rsrc, rdst) ->
    spaces level ^
    "reduce " ^ name ^ print_vs vs ^
    "("^string_of_basic_parameters ps^"): "^
    string_of_expr rsrc ^ " => " ^ string_of_expr rdst ^
    ";\n"

  | `AST_axiom (_,name, vs, ps, a) ->
    spaces level ^
    "axiom " ^ name ^ print_vs vs ^
    "("^string_of_parameters ps^"): "^
    string_of_axiom_method a ^
    ";\n"

  | `AST_lemma (_,name, vs, ps, a) ->
    spaces level ^
    "lemma " ^ name ^ print_vs vs ^
    "("^string_of_parameters ps^"): "^
    string_of_axiom_method a ^
    ";\n"

  | `AST_function (_,name, vs, ps, (res,post), props, ss) ->
    spaces level ^
    string_of_properties props ^
    "fun " ^ name ^ print_vs vs ^
    "("^string_of_parameters ps^"): "^string_of_typecode res^
    (match post with
    | None -> ""
    | Some x -> " when " ^ string_of_expr x
    )^
    begin match ss with
    | [`AST_fun_return (_,e)] -> " => " ^ se e ^ ";\n"
    | _ -> "\n" ^ string_of_compound level ss
    end

  | `AST_curry (_,name, vs, pss, (res,traint) , kind, ss) ->
    spaces level ^
    (match kind with
    | `Function -> "fun "
    | `CFunction -> "cfun "
    | `InlineFunction -> "inline fun "
    | `NoInlineFunction -> "noinline fun "
    | `Virtual -> "virtual fun "
    | `Ctor -> "ctor "
    | `Generator -> "generator "
    )
    ^
    name ^ print_vs vs ^
    catmap " "
    (fun ps ->
      "("^string_of_parameters ps^")"
    )
    pss
    ^
    ": "^string_of_typecode res^
    (match traint with
    | None -> ""
    | Some x -> " when " ^ string_of_expr x
    )^
    begin match ss with
    | [`AST_fun_return (_,e)] -> " => " ^ se e ^ ";\n"
    | _ -> "\n" ^ string_of_compound level ss
    end

  | `AST_macro_val (_,names, e) ->
    spaces level ^
    "macro val " ^ String.concat ", " names ^ " = " ^
    se e ^
    ";"

  | `AST_macro_vals (_,name, es) ->
    spaces level ^
    "macro val " ^ name ^ " = " ^
    catmap ", " se es ^
    ";"

  | `AST_macro_var (_,names, e) ->
    spaces level ^
    "macro var " ^ String.concat ", " names ^ " = " ^
    se e ^
    ";"

  | `AST_macro_assign (_,names, e) ->
    spaces level ^
    "macro " ^ String.concat ", " names ^ " = " ^
    se e ^
    ";\n"

  | `AST_macro_name (_,lname, rname) ->
    spaces level ^
    "macro ident " ^ lname ^ " = " ^
    (match rname with | "" -> "new" | _ -> rname) ^
    ";"

  | `AST_macro_names (_,lname, rnames) ->
    spaces level ^
    "macro ident " ^ lname ^ " = " ^
    cat ", " rnames ^
    ";"


  | `AST_expr_macro (_,name, ps, e) ->
    let sps =
      map
      (fun (p,t) -> p ^ ":" ^ string_of_macro_parameter_type t)
      ps
    in
    spaces level ^
    "macro fun " ^ name ^
    "("^ cat ", " sps ^") = " ^
    se e ^
    ";"

  | `AST_stmt_macro (_,name, ps, ss) ->
    let sps =
      map
      (fun (p,t) -> p ^ ":" ^ string_of_macro_parameter_type t)
      ps
    in
    spaces level ^
    "macro proc " ^ name ^
    "("^ cat ", " sps ^") " ^
    short_string_of_compound level ss

  | `AST_macro_block (_,ss) ->
    spaces level ^
    "macro " ^
    short_string_of_compound level ss ^
    "}"

  | `AST_macro_forget (_,names) ->
    spaces level ^
    "macro forget" ^
    (
      match names with
      | [] -> ""
      | _ -> " "
    ) ^
    cat ", " names ^
    ";"

  | `AST_macro_label (_,id) ->
    "macro " ^ id ^ ":>\n"

  | `AST_macro_goto (_,id) ->
    "macro goto " ^ id ^ ";\n"

  | `AST_macro_ifgoto (_,e,id) ->
    "macro if "^se e^" goto " ^ id ^ ";\n"

  | `AST_macro_proc_return (_) ->
    "macro return;\n"

  | `AST_val_decl (_,name, vs,ty, value) ->
    spaces level ^
    "val " ^ name ^
    (
      match ty with
      | Some t -> ": " ^ string_of_typecode t
      | None -> ""
    )
    ^
    (
      match value with
      | Some e -> " = " ^ (se e)
      | None -> ""
    )
    ^ ";"

  | `AST_ref_decl (_,name, vs,ty, value) ->
    spaces level ^
    "ref " ^ name ^
    (
      match ty with
      | Some t -> ": " ^ string_of_typecode t
      | None -> ""
    )
    ^
    (
      match value with
      | Some e -> " = " ^ (se e)
      | None -> ""
    )
    ^ ";"


  | `AST_lazy_decl (_,name, vs,ty, value) ->
    spaces level ^
    "fun " ^ name ^
    (
      match ty with
      | Some t -> ": " ^ string_of_typecode t
      | None -> ""
    )
    ^
    (
      match value with
      | Some e -> " = " ^ (se e)
      | None -> ""
    )
    ^ ";"

  | `AST_var_decl (_,name, vs,ty, value) ->
    spaces level ^
    "var " ^ name ^
    (
      match ty with
      | Some t -> ": " ^ string_of_typecode t
      | None -> ""
    )
    ^
    (
      match value with
      | Some e -> " = " ^ (se e)
      | None -> ""
    )
    ^ ";"

  | `AST_macro_ifor (_,v,ids,sts) ->
    spaces level
    ^ "macro for ident " ^ v ^ " in " ^ cat "," ids ^ " do\n" ^
    catmap "\n" (string_of_statement (level +2)) sts ^
    spaces level ^ "done;"

  | `AST_macro_vfor (_,v,e,sts) ->
    let se e = string_of_expr e in
    spaces level
    ^ "macro for val " ^ String.concat ", " v ^ " in " ^ se e ^ " do\n" ^
    catmap "\n" (string_of_statement (level +2)) sts ^
    spaces level ^ "done;"

  | `AST_call (_,pr, args) ->
    spaces level
    ^ "call " ^ se pr ^ " " ^ se args ^ ";"

  | `AST_assign (_,name,l,r) ->
    spaces level
    ^ "call " ^ name ^ "(" ^ sl l ^ "," ^se r^");"

  | `AST_cassign (_,l,r) ->
    spaces level ^
    se l ^ " = " ^ se r ^ ";"

  | `AST_jump (_,pr, args) ->
    spaces level
    ^ "jump " ^ se pr ^ " " ^ se args ^ ";"

  | `AST_loop (_,pr, args) ->
    spaces level
    ^ "call " ^ pr ^ " " ^ se args ^ ";"

  | `AST_nop (_,s) -> spaces level ^ "{/*"^s^"*/;}"

  | `AST_ifgoto (_,e,lab) ->
    spaces level ^
    "if("^string_of_expr e^")goto " ^ lab ^ ";"

  | `AST_ifreturn (_,e) ->
    spaces level ^
    "if("^string_of_expr e^")return;"

  | `AST_ifdo (_,e,ss1,ss2) ->
    spaces level ^
    "if("^string_of_expr e^")do\n" ^
    catmap "\n" (string_of_statement (level+1)) ss1 ^
    spaces level ^ "else\n" ^
    catmap "\n" (string_of_statement (level+1)) ss2 ^
    spaces level ^ "done;"

  | `AST_fun_return (_,e) ->
    spaces level ^ "return " ^ (se e) ^ ";"

  | `AST_yield (_,e) ->
    spaces level ^ "yield " ^ (se e) ^ ";"

  | `AST_proc_return _ ->
    spaces level ^ "return;"

  | `AST_halt (_,s) ->
    spaces level ^ "halt "^string_of_string s^";"

  | `AST_trace (_,v,s) ->
    spaces level ^ "trace "^v^ ", msg="^string_of_string s^";"

  | `AST_svc (_,name) ->
    spaces level ^ "read " ^ name ^ ";"

  | `AST_user_statement (_,name,term) ->
    let body = string_of_ast_term level term in
    spaces level ^ "User statement " ^ name ^ "\n" ^ body

  | `AST_scheme_string (_,s) ->
    spaces level ^ "Scheme string " ^ s ^ ";\n"

  | `AST_stmt_match (_,(e, ps)) ->
    spaces level ^ "match " ^ se e ^ " with\n" ^
    catmap "\n"
    (fun (p,sts)->
      " | " ^
      string_of_pattern p ^
      " => " ^
       catmap "\n" (string_of_statement (level+1)) sts 
    )
    ps
    ^
    "\n"^spaces level^"endmatch;"


and string_of_compilation_unit stats =
  catmap "\n" (string_of_statement 0) stats

and string_of_desugared stats =
  catmap "\n" (string_of_asm 0) stats

and string_of_iface level s =
  let spc = spaces level in
  match s with
  | IFACE_export_fun (flx_name,cpp_name) ->
    spc ^ "export fun " ^ string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | IFACE_export_python_fun (flx_name,cpp_name) ->
    spc ^ "export python fun " ^ string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | IFACE_export_type (flx_type,cpp_name) ->
    spc ^ "export type (" ^ string_of_typecode flx_type ^
    ") as \"" ^ cpp_name ^ "\";"

and string_of_symdef (entry:symbol_definition_t) name (vs:ivs_list_t) =
  let se e = string_of_expr e in
  let st t = string_of_typecode t in
  match entry with
  | SYMDEF_instance qn ->
    "instance " ^ print_ivs vs ^ " " ^
    string_of_qualified_name qn ^ ";\n"

  | SYMDEF_const_ctor (uidx,ut,idx,vs') ->
     st ut ^ "  const_ctor: " ^
     name ^ print_ivs vs ^
     ";"

  | SYMDEF_nonconst_ctor (uidx,ut,idx,vs',argt) ->
     st ut ^ "  nonconst_ctor: " ^
     name ^ print_ivs vs ^
     " of " ^ st argt ^
     ";"

  | SYMDEF_type_alias t ->
    "typedef " ^ name ^ print_ivs vs ^" = " ^ st t ^ ";"

  | SYMDEF_inherit qn ->
    "inherit " ^ name ^ print_ivs vs ^" = " ^ string_of_qualified_name qn ^ ";"

  | SYMDEF_inherit_fun qn ->
    "inherit fun " ^ name ^ print_ivs vs ^" = " ^ string_of_qualified_name qn ^ ";"

  | SYMDEF_abs (quals,code, reqs) ->
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "type " ^ name ^ print_ivs vs ^
    " = " ^ string_of_code_spec code ^
    string_of_named_reqs reqs ^
    ";"

  | SYMDEF_newtype (nt) ->
    "type " ^ name ^ print_ivs vs ^
    " = new " ^ st nt ^
    ";"

  | SYMDEF_var (t) ->
    "var " ^ name ^ print_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_val (t) ->
    "val " ^ name ^ print_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_ref (t) ->
    "ref " ^ name ^ print_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_lazy (t,e) ->
    "fun " ^ name ^ print_ivs vs ^
    ": "^ st t ^
    "= " ^ se e ^
    ";"

  | SYMDEF_parameter (k,t) ->
    "parameter " ^ string_of_param_kind k ^ " " ^
    name ^ print_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_typevar (t) ->
    "typevar " ^ name ^ print_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_const (props,t,ct, reqs) ->
    string_of_properties props ^
    "const " ^ name ^ print_ivs vs ^":"^
    st t ^ " = " ^string_of_code_spec ct^
    string_of_named_reqs reqs ^
    ";"

  | SYMDEF_union (cts) ->
    "union " ^ name ^ print_ivs vs ^ ";"

  | SYMDEF_struct (cts) ->
    "struct " ^ name ^ print_ivs vs ^ ";"

  | SYMDEF_cstruct (cts) ->
    "cstruct " ^ name ^ print_ivs vs ^ ";"

  | SYMDEF_typeclass ->
    "typeclass " ^ name ^ print_ivs vs ^ ";"

  | SYMDEF_fun (props, pts,res,cts, reqs,prec) ->
    string_of_properties props ^
    "fun " ^ name ^ print_ivs vs ^
    ": " ^ st
    (
      `TYP_function
      (
        (
          match pts with
          | [x] -> x
          | x -> `TYP_tuple x
        )
        ,
        res
      )
    ) ^
    (if prec = "" then "" else ":"^prec^" ")^
    string_of_named_reqs reqs ^
    ";"

  | SYMDEF_callback (props, pts,res,reqs) ->
    string_of_properties props ^
    "callback fun " ^ name ^ print_ivs vs ^
    ": " ^ st
    (
      `TYP_cfunction
      (
        (
          match pts with
          | [x] -> x
          | x -> `TYP_tuple x
        )
        ,
        res
      )
    ) ^
    string_of_named_reqs reqs ^
    ";"

  | SYMDEF_insert (s,ikind, reqs) ->
    (match ikind with
    | `Header -> "header "
    | `Body -> "body "
    | `Package -> "package "
    ) ^
    name ^ print_ivs vs ^
    " "^ string_of_code_spec s ^
     string_of_named_reqs reqs ^
    ";\n"

  | SYMDEF_reduce (ps,e1,e2) ->
    "reduce " ^ name ^ print_ivs vs ^ ";"

  | SYMDEF_axiom (ps,e1) ->
    "axiom " ^ name ^ print_ivs vs ^ ";"

  | SYMDEF_lemma (ps,e1) ->
    "lemma " ^ name ^ print_ivs vs ^ ";"

  | SYMDEF_function (ps,res,props, es) ->
    let ps,traint = ps in
    string_of_properties props ^
    "fun " ^ name ^ print_ivs vs ^
    ": " ^ st
    (
      `TYP_function
      (
        (
          match map (fun (x,y,z,d) -> z) ps with
          | [x] -> x
          | x -> `TYP_tuple x
        )
        ,
        res
      )
    ) ^
    ";"

  | SYMDEF_match_check (pat,(mvname,i))->
    "match_check " ^ name ^ " for " ^ string_of_pattern pat ^ ";"

  | SYMDEF_module ->
    "module " ^ name ^ ";"

and string_of_exe level s =
  let spc = spaces level
  and se e = string_of_expr e
  in
  match s with

  | `EXE_goto s -> spc ^ "goto " ^ s ^ ";"
  | `EXE_assert e -> spc ^ "assert " ^ se e ^ ";"

  | `EXE_ifgoto (e,s) -> spc ^
     "if(" ^ se e ^ ")goto " ^ s ^ ";"

  | `EXE_label s -> s ^ ":"

  | `EXE_comment s -> spc ^
    "// " ^ s

  | `EXE_call (p,a) -> spc ^
    "call " ^
    se p ^ " " ^
    se a ^ ";"

  | `EXE_jump (p,a) -> spc ^
    "jump " ^
    se p ^ " " ^
    se a ^ ";"

  | `EXE_loop (p,a) -> spc ^
    "loop " ^
    p ^ " " ^
    se a ^ ";"

  | `EXE_svc v -> spc ^
    "_svc " ^ v

  | `EXE_fun_return x -> spc ^
    "return " ^ se x ^ ";"

  | `EXE_yield x -> spc ^
    "yield " ^ se x ^ ";"

  | `EXE_proc_return -> spc ^
    "return;"

  | `EXE_halt s -> spc ^
    "halt "^string_of_string s^";"

  | `EXE_trace (v,s) -> spc ^
    "trace "^v^"="^string_of_string s^";"


  | `EXE_nop s -> spc ^
    "/*" ^ s ^ "*/"

  | `EXE_code s -> spc ^
    "code " ^ string_of_code_spec s

  | `EXE_noreturn_code s -> spc ^
    "noreturn_code " ^ string_of_code_spec s

  | `EXE_init (l,r) -> spc ^
    l ^ " := " ^ se r ^ ";"

  | `EXE_iinit ((l,i),r) -> spc ^
    l ^ "<"^si i^"> := " ^ se r ^ ";"

  | `EXE_assign (l,r) -> spc ^
    se l ^ " = " ^ se r ^ ";"

and sbe dfns bbdfns e = string_of_bound_expression dfns bbdfns e
and tsbe dfns bbdfns e = string_of_bound_expression_with_type dfns bbdfns e

and string_of_bound_expression_with_type dfns bbdfns ((e',t) as e) =
  string_of_bound_expression' dfns bbdfns (tsbe dfns bbdfns) e ^ ":" ^
  sbt dfns t

and string_of_bound_expression dfns bbdfns e =
  string_of_bound_expression' dfns bbdfns (sbe dfns bbdfns) e

and string_of_bound_expression' dfns bbdfns se e =
  (*
  let sid n = qualified_name_of_bindex dfns bbdfns n in
  let sid n = fst (get_name_parent dfns bbdfns n) in
  *)
  let sid n = bound_name_of_bindex dfns bbdfns n in
  match fst e with

  | BEXPR_get_n (n,e') -> "(" ^ se e' ^ ").mem_" ^ si n
  | BEXPR_get_named (i,e') -> "(" ^ se e' ^ ")." ^ sid i

  | BEXPR_deref e -> "*("^ se e ^ ")"
  | BEXPR_name (i,ts) -> sid i ^ print_inst dfns ts
  | BEXPR_closure (i,ts) -> sid i ^ print_inst dfns ts
  | BEXPR_ref (i,ts) -> "&" ^ sid i ^ print_inst dfns ts
  | BEXPR_new e -> "new " ^ se e
  | BEXPR_address e -> "&" ^ se e
  | BEXPR_not e -> "not " ^ se e
  | BEXPR_likely e -> "likely(" ^ se e ^")"
  | BEXPR_unlikely e -> "unlikely(" ^ se e ^")"

  | BEXPR_literal e -> string_of_literal e
  | BEXPR_apply  (fn, arg) -> "(" ^
    se fn ^ " " ^
    se arg ^
    ")"

  | BEXPR_apply_prim (i,ts, arg) -> "(" ^
    sid i ^ print_inst dfns ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_apply_direct  (i,ts, arg) -> "(" ^
    sid i ^ print_inst dfns ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_apply_struct (i,ts, arg) -> "(" ^
    sid i ^ print_inst dfns ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_apply_stack (i,ts, arg) -> "(" ^
    sid i ^ print_inst dfns ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_tuple t -> "(" ^ catmap ", " se t ^ ")"

  | BEXPR_record ts -> "struct { " ^
      catmap "" (fun (s,e)-> s^":"^ se e ^"; ") ts ^ "}"

  | BEXPR_variant (s,e) -> "case " ^ s ^ " of (" ^ se e ^ ")"

  | BEXPR_case (v,t) ->
    "case " ^ si v ^ " of " ^ string_of_btypecode dfns t

  | BEXPR_match_case (v,e) ->
    "(match case " ^ si v ^ ")(" ^ se e ^ ")"

  | BEXPR_case_arg (v,e) ->
    "(arg of case " ^ si v ^ " of " ^ se e ^ ")"

  | BEXPR_case_index e ->
    "caseno (" ^ se e ^ ")"

  | BEXPR_expr (s,t) ->
    "code ["^string_of_btypecode dfns t^"]" ^ "'" ^ s ^ "'"

  | BEXPR_range_check (e1,e2,e3) ->
    "range_check(" ^ se e1 ^"," ^ se e2 ^"," ^se e3 ^ ")"

  | BEXPR_coerce (e,t) -> se e ^ " : " ^ string_of_btypecode dfns t

and string_of_biface dfns bbdfns level s =
  let spc = spaces level in
  let se e = string_of_bound_expression dfns bbdfns e in
  let sid n = qualified_name_of_index dfns n in
  match s with
  | BIFACE_export_fun (_,index,cpp_name) ->
    spc ^ "export fun " ^ qualified_name_of_index dfns index ^
    " as \"" ^ cpp_name ^ "\";"

  | BIFACE_export_python_fun (_,index,cpp_name) ->
    spc ^ "export python fun " ^ qualified_name_of_index dfns index ^
    " as \"" ^ cpp_name ^ "\";"

  | BIFACE_export_type (_,btyp,cpp_name) ->
    spc ^ "export type (" ^ string_of_btypecode dfns btyp ^
    ") as \"" ^ cpp_name ^ "\";"

and sbx dfns bbdfns s =  string_of_bexe dfns bbdfns 0 s

and string_of_bexe dfns bbdfns level s =
  let spc = spaces level in
  let se e = string_of_bound_expression dfns bbdfns e in
  let sid n = bound_name_of_bindex dfns bbdfns n in
  match s with
  | BEXE_goto (_,s) -> spc ^ "goto " ^ s ^ ";"

  | BEXE_assert (_,e) -> spc ^ "assert " ^ se e ^ ";"
  | BEXE_assert2 (_,_,e1,e2) -> spc ^ "assert2 " ^
    (match e1 with Some e1 -> se e1 ^ " implies " | None -> "") ^
    se e2^";"

  | BEXE_axiom_check (_,e) -> spc ^ "axiom_check " ^ se e ^ ";"

  | BEXE_halt (_,s) -> spc ^ "halt " ^ s ^ ";"
  | BEXE_trace(_,v,s) -> spc ^ "trace " ^ s ^ ";"

  | BEXE_ifgoto (_,e,s) -> spc ^
     "if(" ^ se e ^ ")goto " ^ s ^ ";"

  | BEXE_label (_,s) -> s ^ ":"

  | BEXE_comment (_,s) -> spc ^
    "// " ^ s

  | BEXE_call (_,p,a) -> spc ^
    "call " ^
    se p ^ " " ^
    se a ^ ";"

  | BEXE_call_direct (_,i,ts,a) -> spc ^
    "directcall " ^
    sid i ^ print_inst dfns ts ^ " " ^
    se a ^ ";"

  | BEXE_jump_direct (_,i,ts,a) -> spc ^
    "direct tail call " ^
    sid i ^ print_inst dfns ts ^ " " ^
    se a ^ ";"

  | BEXE_call_stack (_,i,ts,a) -> spc ^
    "stackcall " ^
    sid i ^ print_inst dfns ts ^ " " ^
    se a ^ ";"

  | BEXE_call_prim (_,i,ts,a) -> spc ^
    "primcall " ^
    sid i ^ print_inst dfns ts ^ " " ^
    se a ^ ";"

  | BEXE_jump (_,p,a) -> spc ^
    "tail call " ^
    se p ^ " " ^
    se a ^ ";"

  | BEXE_loop (_,p,a) -> spc ^
    "loop<" ^
    si p ^ "> " ^
    se a ^ ";"

  | BEXE_svc (_,v) -> spc ^
    "_svc " ^ sid v

  | BEXE_fun_return (_,x) -> spc ^
    "return " ^ se x ^ ";"

  | BEXE_yield (_,x) -> spc ^
    "yield " ^ se x ^ ";"

  | BEXE_proc_return _ -> spc ^
    "return;"

  | BEXE_nop (_,s) -> spc ^
    "/*" ^ s ^ "*/"

  | BEXE_code (_,s) -> spc ^
    "code " ^ string_of_code_spec s

  | BEXE_nonreturn_code (_,s) -> spc ^
    "non_return_code " ^ string_of_code_spec s

  | BEXE_assign (_,l,r) -> spc ^
    se l ^ " = " ^ se r ^ ";"

  | BEXE_init (_,l,r) -> spc ^
    sid l ^ " := " ^ se r ^ ";"

  | BEXE_begin -> "{//begin"

  | BEXE_end -> "}//end"


and string_of_dcl level name seq vs (s:dcl_t) =
  let se e = string_of_expr e in
  let st t = string_of_typecode t in
  let sl = spaces level in
  let seq = match seq with Some i -> "<" ^ si i ^ ">" | None -> "" in
  match s with
  | DCL_type_alias (t2) ->
    sl ^ "typedef " ^ name^seq ^ print_vs vs ^
    " = " ^ st t2 ^ ";"

  | DCL_inherit qn ->
    sl ^ "inherit " ^ name^seq ^ print_vs vs ^
    " = " ^ string_of_qualified_name qn ^ ";"

  | DCL_inherit_fun qn ->
    sl ^ "inherit fun " ^ name^seq ^ print_vs vs ^
    " = " ^ string_of_qualified_name qn ^ ";"

  | DCL_module (asms) ->
    sl ^ "module " ^ name^seq ^ print_vs vs ^ " = " ^
    "\n" ^
    string_of_asm_compound level asms

  | DCL_instance (name,asms) ->
    sl ^ "instance " ^ print_vs vs ^ " " ^
    string_of_qualified_name name ^seq ^ " = " ^
    "\n" ^
    string_of_asm_compound level asms

  | DCL_struct (cs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ name^ ": " ^ st ty ^ ";"
    in
    sl ^ "struct " ^ name^seq ^ print_vs vs ^ " = " ^
    sl ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    sl ^ "}"

  | DCL_cstruct (cs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ name^ ": " ^ st ty ^ ";"
    in
    sl ^ "cstruct " ^ name^seq ^ print_vs vs ^ " = " ^
    sl ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    sl ^ "}"

  | DCL_typeclass (asms) ->
    sl ^ "type class " ^ name^seq ^ print_vs vs ^ " =\n" ^
    string_of_asm_compound level asms

  | DCL_union (cs) ->
    let string_of_union_component (name,v,vs,ty) =
      (spaces (level+1)) ^
      "|" ^name^
      (match v with | None -> "" | Some i -> "="^si i) ^
      special_string_of_typecode ty
    in
    sl ^ "union " ^ name^seq ^ print_vs vs ^
    " = " ^
    sl ^ "{\n" ^
    catmap ";\n" string_of_union_component cs ^ "\n" ^
    sl ^ "}"

  | DCL_newtype (nt)-> sl ^
    "type " ^ name^seq ^ print_vs vs ^
    " = new " ^ st nt ^ ";"

  | DCL_abs (quals, code, reqs) -> sl ^
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "type " ^ name^seq ^ print_vs vs ^
    " = " ^ string_of_code_spec code ^
    string_of_named_reqs reqs ^
    ";"

  | DCL_fun (props, args, result, code, reqs,prec) ->
    let argtype:typecode_t = type_of_argtypes args in
    let t:typecode_t = `TYP_function (argtype,result) in
    sl ^
    string_of_properties props ^
    "fun " ^ name^seq ^ print_vs vs ^
    ": " ^ st t ^
    " = " ^ string_of_code_spec code ^
    (if prec = "" then "" else ":"^prec^" ")^
    string_of_named_reqs reqs ^
    ";"

  | DCL_callback (props, args, result, reqs) ->
    let argtype:typecode_t = type_of_argtypes args in
    let t:typecode_t = `TYP_cfunction (argtype,result) in
    sl ^
    string_of_properties props ^
    "callback fun " ^ name^seq ^ print_vs vs ^
    ": " ^ st t ^
    string_of_named_reqs reqs ^
    ";"

  | DCL_insert (s,ikind, reqs) ->
    sl ^
    (match ikind with
    | `Header -> "header "
    | `Body -> "body "
    | `Package -> "package "
    ) ^
    name^seq ^  print_vs vs ^
    " = "^ string_of_code_spec s ^
    string_of_named_reqs reqs ^ ";"

  | DCL_const (props,typ, code, reqs) ->
    sl ^
    string_of_properties props ^
    "const " ^ name^seq ^print_vs vs ^
    ": " ^ st typ ^
    " = "^string_of_code_spec code^
    string_of_named_reqs reqs ^
    ";"

  | DCL_reduce (ps, e1,e2) ->
    sl ^
    "reduce " ^ name^seq ^ print_vs vs ^
    "("^ string_of_basic_parameters ps ^"): " ^
    string_of_expr e1 ^ " => " ^ string_of_expr e2 ^ ";"

  | DCL_axiom (ps, e1) ->
    sl ^
    "axiom " ^ name^seq ^ print_vs vs ^
    "("^ string_of_parameters ps ^"): " ^
    string_of_axiom_method e1 ^ ";"

  | DCL_lemma (ps, e1) ->
    sl ^
    "lemma " ^ name^seq ^ print_vs vs ^
    "("^ string_of_parameters ps ^"): " ^
    string_of_axiom_method e1 ^ ";"

  | DCL_function (ps, res, props, ss) ->
    sl ^
    string_of_properties props ^
    "fun " ^ name^seq ^ print_vs vs ^
    "("^ (string_of_parameters ps)^"): "^(st res)^"\n" ^
    string_of_asm_compound level ss


  | DCL_match_check (pat,(s,i)) ->
    sl ^
    "function " ^ name^seq ^ "() { " ^
    s ^ "<"^si i^"> matches " ^ string_of_pattern pat ^
    " }"

  | DCL_match_handler (pat,(varname, i), sts) ->
    sl ^
    "match_handler " ^ name^seq ^
    "(" ^ string_of_pattern pat ^ ")" ^
    string_of_asm_compound level sts

  | DCL_val (ty) ->
    sl ^
    "val " ^ name^seq ^ print_vs vs ^ ": " ^ st ty ^ ";"

  | DCL_ref (ty) ->
    sl ^
    "ref " ^ name^seq ^ print_vs vs ^ ": " ^ st ty ^ ";"

  | DCL_var (ty) ->
    sl ^
    "var " ^ name^seq ^ print_vs vs ^ ": " ^ st ty ^ ";"

  | DCL_lazy (ty,e) ->
    sl ^
    "fun " ^ name^seq ^ print_vs vs ^
    ": " ^ st ty ^
    "= " ^ se e ^
    ";"

and string_of_access = function
  | `Private -> "private "
  | `Public -> "public"

and string_of_asm level s =
  match s with
  | `Dcl (sr,name,seq,access,vs, d) ->
    (match access with
    | `Private -> "private "
    | `Public -> ""
    ) ^
    string_of_dcl level name seq vs d
  | `Exe (sr,s) -> string_of_exe level s
  | `Iface (sr,s) -> string_of_iface level s
  | `Dir s -> string_of_dir level s

and string_of_dir level s =
  let sqn n = string_of_qualified_name n in
  match s with
  | DIR_open (vs,qn) ->
    spaces level ^ "open" ^ print_ivs vs ^ sqn qn ^ ";"

  | DIR_use (n,qn) ->
    spaces level ^ "use " ^ n ^ " = " ^ sqn qn ^ ";"

  | DIR_inject_module (qn) ->
    spaces level ^ "inherit " ^ sqn qn ^ ";"

and string_of_breq dfns (i,ts) = "rq<"^si i^">" ^ print_inst dfns ts
and string_of_breqs dfns reqs = catmap ", " (string_of_breq dfns) reqs
and string_of_production p = catmap " " string_of_glr_entry p
and string_of_reduced_production p = catmap " " string_of_reduced_glr_entry p
and string_of_bproduction dfns p = catmap " " (string_of_bglr_entry dfns) p

and string_of_glr_term t = match t with
  | `GLR_name qn -> string_of_qualified_name qn
  | `GLR_opt t  -> "[" ^ string_of_glr_term t ^ "]"
  | `GLR_ast t -> "{" ^ string_of_glr_term t ^ "}"
  | `GLR_plus t -> "(" ^ string_of_glr_term t ^ ")+"
  | `GLR_alt ts -> catmap " | " string_of_glr_term ts
  | `GLR_seq ts -> catmap " " string_of_glr_term ts

and string_of_glr_entry (name,t) =
  (match name with
  | Some n -> n ^ ":"
  | None -> ""
  )^
  string_of_glr_term t

and string_of_reduced_glr_entry (name,t) =
  (match name with
  | Some n -> n ^ ":"
  | None -> ""
  )^
  string_of_qualified_name t

and string_of_bglr_entry dfns (name,symbol) =
  (match name with
  | Some n -> n ^ ":"
  | None -> ""
  )^
  (match symbol with
  | `Nonterm (i::_)
  | `Term i -> qualified_name_of_index dfns i
  | `Nonterm [] -> "<Undefined nonterminal>"
  )

and string_of_bbdcl dfns bbdfns (bbdcl:bbdcl_t) index : string =
  let name = qualified_name_of_index dfns index in
  let sobt t = string_of_btypecode dfns t in
  let se e = string_of_bound_expression dfns bbdfns e in
  let un = `BTYP_tuple [] in
  match bbdcl with
  | BBDCL_function (props,vs,ps,res,es) ->
    string_of_properties props ^
    "fun " ^ name ^ print_bvs vs ^
    "("^ (string_of_bparameters dfns bbdfns ps)^"): "^(sobt res) ^
    "{\n" ^
    cat "\n" (map (string_of_bexe dfns bbdfns 1) es) ^
    "}"


  | BBDCL_procedure (props,vs,ps,es) ->
    string_of_properties props ^
    "proc " ^ name ^ print_bvs vs ^
    "("^ (string_of_bparameters dfns bbdfns ps)^")" ^
    "{\n" ^
    cat "\n" (map (string_of_bexe dfns bbdfns 1) es) ^
    "}"

  | BBDCL_val (vs,ty) ->
    "val " ^ name ^ print_bvs vs ^ ": " ^ sobt ty ^ ";"

  | BBDCL_var (vs,ty) ->
    "var " ^ name ^ print_bvs vs ^ ": " ^ sobt ty ^ ";"

  | BBDCL_ref (vs,ty) ->
    "ref " ^ name ^ print_bvs vs ^ ": " ^ sobt ty ^ ";"

  | BBDCL_tmp (vs,ty) ->
    "tmp " ^ name ^ print_bvs vs ^ ": " ^ sobt ty ^ ";"

  (* binding structures [prolog] *)
  | BBDCL_newtype (vs,t) ->
    "type " ^ name ^  print_bvs vs ^
    " = new " ^ sobt t ^ ";"

  | BBDCL_abs (vs,quals,code,reqs) ->
    (match quals with [] ->"" | _ -> string_of_bquals dfns quals ^ " ") ^
    "type " ^ name ^  print_bvs vs ^
    " = " ^ string_of_code_spec code ^ ";"

  | BBDCL_const (props, vs,ty,code,reqs) ->
    string_of_properties props ^
     "const " ^ name ^ print_bvs vs ^
     ": " ^ sobt ty ^
     " = " ^ string_of_code_spec code ^
     string_of_breqs dfns reqs ^
     ";"

  | BBDCL_fun (props,vs,ps,rt,code,reqs,prec) ->
    string_of_properties props ^
    "fun " ^ name ^ print_bvs vs ^
    ": " ^
    (sobt (typeoflist ps)) ^ " -> " ^
    (sobt rt) ^
    " = " ^ string_of_code_spec code ^
    (if prec = "" then "" else ":"^prec^" ")^
     string_of_breqs dfns reqs ^
    ";"

  | BBDCL_callback (props,vs,ps_cf,ps_c,k,rt,reqs,prec) ->
    string_of_properties props ^
    "callback fun " ^ name ^ print_bvs vs ^
    ": " ^
    (sobt (typeoflist ps_cf)) ^ " -> " ^
    (sobt rt) ^
    " : " ^
    (if prec = "" then "" else ":"^prec^" ")^
     string_of_breqs dfns reqs ^
    ";"

  | BBDCL_proc (props,vs, ps,code,reqs) ->
    string_of_properties props ^
    "proc " ^ name ^ print_bvs vs ^
    ": " ^
     (sobt (typeoflist ps)) ^
     " = " ^ string_of_code_spec code ^
     string_of_breqs dfns reqs ^
     ";"

  | BBDCL_insert (vs,s,ikind,reqs) ->
     (match ikind with
     | `Header -> "header "
     | `Body -> "body "
     | `Package -> "package "
     ) ^
    name^  print_bvs vs ^
    " "^ string_of_code_spec s ^
    string_of_breqs dfns reqs

  | BBDCL_union (vs,cs) ->
    let string_of_union_component (name,v,ty) =
      "  " ^ "|" ^name ^
     "="^si v^
      special_string_of_btypecode dfns ty
    in
    "union " ^ name ^ print_bvs vs ^ " = " ^
    "{\n" ^
    catmap ";\n" string_of_union_component cs ^ "\n" ^
    "}"

  | BBDCL_struct (vs,cs) ->
    let string_of_struct_component (name,ty) =
      "  " ^ name ^ ": " ^ sobt ty ^ ";"
    in
    "struct " ^ name ^ print_bvs vs ^ " = " ^
    "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    "}"

  | BBDCL_cstruct (vs,cs) ->
    let string_of_struct_component (name,ty) =
      "  " ^ name ^ ": " ^ sobt ty ^ ";"
    in
    "cstruct " ^ name ^ print_bvs vs ^ " = " ^
    "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    "}"

  | BBDCL_typeclass (props,vs) ->
    string_of_properties props ^
    "typeclass " ^ name ^ print_bvs vs ^ ";"

  | BBDCL_instance (props,vs,cons,bid,ts) ->
    string_of_properties props ^
    "instance "^print_bvs_cons dfns vs cons^
    " of <" ^ si bid ^">["^ catmap "," (sbt dfns) ts ^ "];"

  | BBDCL_nonconst_ctor (vs,uidx,ut,ctor_idx, ctor_argt, evs, etraint) ->
    "  uctor<" ^ name ^ ">"^ print_bvs vs ^
    " : " ^ sobt ut ^
    " of " ^ sobt ctor_argt ^
    ";"


let string_of_dfn dfns i =
  match Hashtbl.find dfns i with
  | { id=id; sr=sr; vs=vs; symdef=entry } ->
  string_of_symdef entry id vs
  ^ "\n  defined at " ^ Flx_srcref.short_string_of_src sr

let full_string_of_entry_kind dfns {base_sym=i; spec_vs=vs; sub_ts=ts} =
  string_of_dfn dfns i ^
  "\n  with view" ^
  " vs=" ^ catmap "," (fun (s,_)->s) vs ^
  " ts=" ^ catmap "," (sbt dfns) ts


let string_of_entry_kind {base_sym=i} = si i

let string_of_entry_set = function
  | NonFunctionEntry x -> string_of_entry_kind x
  | FunctionEntry ls ->
    "{" ^
      catmap "," string_of_entry_kind ls ^
    "}"

let full_string_of_entry_set dfns = function
  | NonFunctionEntry x -> full_string_of_entry_kind dfns x
  | FunctionEntry ls -> if length ls = 0 then "{}" else
    "{\n" ^
      catmap "\n" (full_string_of_entry_kind dfns) ls ^
    "\n}"

let string_of_myentry dfns {base_sym=i; spec_vs=vs; sub_ts=ts} =
 si i ^
 " vs=" ^ catmap "," (fun (s,_)->s) vs ^
 " ts=" ^ catmap "," (sbt dfns) ts

let print_name_table dfns table =
  Hashtbl.iter
  (fun s v ->
    print_endline (s ^ " --> " ^
      match v with
      | NonFunctionEntry i -> string_of_myentry dfns i
      | FunctionEntry ii -> "{"^ catmap "," (string_of_myentry dfns) ii ^ "}"
    );
  )
  table


let string_of_varlist dfns varlist =
  catmap ", " (fun (i,t)-> si i ^ "->" ^ sbt dfns t) varlist

let print_env e =
  let print_entry k v =
    print_endline
    (
      "  " ^ k ^ " " ^
      (
        match v with
        | (NonFunctionEntry (i)) -> string_of_entry_kind i
        | _ -> ""
      )
    )
  in
  let print_table htab =
    print_endline "--"; Hashtbl.iter print_entry htab

  in
  let print_level (index,id,htab,htabs,con) =
    print_string (id^"<"^si index^">");
    print_table htab;
    print_endline "OPENS:";
    List.iter print_table htabs;
    print_endline "ENDOFOPENS";
    print_endline ("CONSTRAINT: " ^ string_of_typecode con)
  in

  List.iter print_level e

let print_env_short e =
  let print_level (index,id,htab,htabs,con) =
    print_endline (id^"<"^si index^">")
  in
  List.iter print_level e

let print_function_body dfns bbdfns id i (bvs:bvs_t) ps exes parent =
  print_endline "";
  print_endline ("BODY OF " ^ id ^ "<" ^ si i ^ "> [" ^
  catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") bvs ^
  "] parent " ^
    (match parent with None -> "NONE" | Some k -> si k)
    ^
    "(" ^ string_of_bparameters dfns bbdfns ps ^ ")"
  );
  iter
  (fun exe -> print_endline (string_of_bexe dfns bbdfns 1 exe))
  exes

let print_function dfns bbdfns i =
  match Hashtbl.find bbdfns i with (id,parent,_,entry) ->
  match entry with
  | BBDCL_function (_,bvs,ps,_,exes)
  | BBDCL_procedure (_,bvs,ps,exes) ->
    print_function_body dfns bbdfns id i bvs ps exes parent
  | _ -> ()

let print_functions dfns (bbdfns:fully_bound_symbol_table_t) =
  Hashtbl.iter
  (fun i (id,parent,_,entry) -> match entry with
  | BBDCL_function (_,bvs,ps,_,exes)
  | BBDCL_procedure (_,bvs,ps,exes) ->
    print_function_body dfns bbdfns id i bvs ps exes parent

  | _ -> ()
  )
  bbdfns

let print_symbols dfns (bbdfns:fully_bound_symbol_table_t) =
  Hashtbl.iter
  (fun i (id,parent,_,entry) -> match entry with
  | BBDCL_function (_,bvs,ps,_,exes)
  | BBDCL_procedure (_,bvs,ps,exes) ->
    print_function_body dfns bbdfns id i bvs ps exes parent
  | BBDCL_var (bvs,t) ->
    print_endline ("VARIABLE " ^ id ^ "<" ^ si i ^ "> [" ^
      catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") bvs ^
      "] parent " ^
      (match parent with None -> "NONE" | Some k -> si k) ^
      " type " ^ sbt dfns t
    )
  | BBDCL_val (bvs,t) ->
    print_endline ("VALUE " ^ id ^ "<" ^ si i ^ "> [" ^
      catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") bvs ^
      "] parent " ^
      (match parent with None -> "NONE" | Some k -> si k) ^
      " type " ^ sbt dfns t
    )
  | _ -> ()
  )
  bbdfns
