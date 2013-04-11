open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_typing
open List

module L = Flx_literal

let rec string_of_string s = Flx_string.c_quote_of_string s

let string_of_char c =
  if c = -1 then "<<EOF>>" else
  if c < 32 || c > 126
  then "\\x" ^ Flx_string.hex2 c
  else String.make 1 (Char.chr c)

let string_of_id id = Flx_id.to_string id

let string_of_bid bid =
  string_of_int bid

let string_of_bidset bidset =
  let bidlist = BidSet.fold (fun i lst -> i :: lst) bidset [] in
  Printf.sprintf "{%s}"
    (String.concat ";" (List.map string_of_bid bidlist))

let string_of_literal {Flx_literal.felix_type=t; internal_value=v; c_value=c} = 
  "literal["^t^"]("^v^")"

let rec string_of_qualified_name (n:qualified_name_t) =
  let se e = string_of_expr e in
  match n with
  | `AST_index (sr,name,idx) -> name ^ "<" ^ string_of_bid idx ^ ">"
  | `AST_void _ -> "void"
  | `AST_name (_,name,ts) ->
      string_of_id name ^
      (
        if List.length ts = 0 then ""
        else "[" ^ catmap ", " string_of_typecode ts ^ "]"
      )
  | `AST_case_tag (_,v) -> "case " ^ si v
  | `AST_typed_case (_,v,t) ->
    "(case " ^ si v ^
    " of " ^ string_of_typecode t ^ ")"

  | `AST_lookup (_,(e,name, ts)) ->
      "(" ^ se e ^ ")::" ^ string_of_id name ^
      (
        if length ts = 0 then "" else
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
  | EXPR_not (sr,e) -> "not(" ^ se e ^ ")"
  | EXPR_index (sr,name,idx) -> name ^ "<" ^ string_of_bid idx ^ ">"
  | EXPR_void _ -> "void"
  | EXPR_name (_,name,ts) ->
      string_of_id name ^
      (
        if List.length ts = 0 then ""
        else "[" ^ catmap ", " string_of_typecode ts ^ "]"
      )
  | EXPR_case_tag (_,v) -> "case " ^ si v
  | EXPR_typed_case (_,v,t) ->
    "(case " ^ si v ^
    " of " ^ string_of_typecode t ^ ")"

  | EXPR_lookup (_, (e, name, ts)) ->
      "(" ^ se e ^ ")::" ^ string_of_id name ^
      (
        if length ts = 0 then "" else
        "[" ^ catmap ", " string_of_typecode ts ^ "]"
      )
  | EXPR_callback (_,name) -> "callback " ^string_of_qualified_name name
  | EXPR_suffix (_,(name,suf)) ->
    string_of_qualified_name name ^ " of (" ^ string_of_typecode suf ^ ")"

  | EXPR_patvar (sr,s) -> "?" ^ string_of_id s
  | EXPR_patany sr -> "ANY"
  | EXPR_interpolate (sr,s) -> "q"^string_of_string s
  | EXPR_vsprintf (sr,s) -> "f"^string_of_string s
  | EXPR_ellipsis _ -> "..."
  (*
  | EXPR_noexpand (sr,e) -> "noexpand(" ^ string_of_expr e ^ ")"
  *)
  (* because 'noexpand' is too ugly .. *)
  | EXPR_noexpand (sr,e) -> string_of_expr e

  | EXPR_letin (sr,(pat,e1, e2)) ->
    "let " ^ string_of_letpat pat ^ " = " ^ se e1 ^ " in " ^ se e2
  | EXPR_coercion (_,(e,t)) ->
    "(" ^ sme e ^ ":" ^
    string_of_typecode t ^ ")"

  | EXPR_expr (_,s,t) ->
    "code ["^string_of_typecode t^"]" ^
    "'" ^ s ^ "'"

  | EXPR_cond (_,(e,b1,b2)) ->
    "if " ^ se e ^
    " then " ^ se b1 ^
    " else " ^ se b2 ^
    " endif"

  | EXPR_typeof (_,e) -> "typeof("^se e^")"
  | EXPR_as (_, (e1, name)) -> "(" ^ se e1 ^ ") as " ^ string_of_id name
  | EXPR_as_var (_, (e1, name)) -> "(" ^ se e1 ^ ") as var " ^ string_of_id name
  | EXPR_get_n (_,(n,e)) -> "get (" ^ si n ^ ", " ^se e^")"
  | EXPR_get_named_variable (_,(n,e)) ->
      "get (" ^ string_of_id n ^ ", " ^ se e ^ ")"
  | EXPR_map (_,f,e) -> "map (" ^ se f ^ ") (" ^ se e ^ ")"
  | EXPR_deref (_,e) -> "*(" ^ se e ^ ")"
  | EXPR_ref (_,e) -> "&" ^ "(" ^ se e ^ ")"
  | EXPR_likely (_,e) -> "likely" ^ "(" ^ se e ^ ")"
  | EXPR_unlikely (_,e) -> "unlikely" ^ "(" ^ se e ^ ")"
  | EXPR_new (_,e) -> "new " ^ "(" ^ se e ^ ")"
  | EXPR_literal (_,e) -> string_of_literal e
  | EXPR_apply  (_,(fn, arg)) -> "(" ^
    sme fn ^ " " ^
    sme arg ^
    ")"

  | EXPR_product (_,ts) ->
     cat "*" (map se ts)

  | EXPR_sum (_,ts) ->
     cat "+" (map se ts)

  | EXPR_intersect (_,ts) ->
     cat "&" (map se ts)

  | EXPR_isin (_,(a,b)) ->
     sme a ^ " isin " ^ sme b

  | EXPR_orlist (_,ts) ->
     cat " or " (map se ts)

  | EXPR_andlist (_,ts) ->
     cat " and " (map se ts)

  | EXPR_arrow (_,(a,b)) ->
    "(" ^ se a ^ " -> " ^ se b ^ ")"

  | EXPR_longarrow (_,(a,b)) ->
    "(" ^ se a ^ " --> " ^ se b ^ ")"

  | EXPR_superscript (_,(a,b)) ->
    "(" ^ se a ^ " ^ " ^ se b ^ ")"

  | EXPR_tuple (_,t) -> "(" ^ catmap ", " sme t ^ ")"
  | EXPR_get_tuple_tail (_,t) -> "get_tuple_tail(" ^ se t ^ ")"
  | EXPR_get_tuple_head (_,t) -> "get_tuple_head(" ^ se t ^ ")"
  | EXPR_tuple_cons (_,eh, et) -> "tuple_cons (" ^ se eh ^ "," ^ se et ^ ")"

  | EXPR_record (_,ts) ->
      "struct {" ^
      catmap " " (fun (s,e) -> string_of_id s ^ "=" ^ sme e ^ ";") ts ^
      "}"

  | EXPR_record_type (_,ts) ->
      "struct {" ^
      catmap " "
        (fun (s,t) -> string_of_id s ^ ":" ^ string_of_typecode t ^ ";")
        ts ^
      "}"

  | EXPR_variant (_, (s, e)) -> "case " ^ string_of_id s ^ " of (" ^ se e ^ ")"

  | EXPR_variant_type (_,ts) ->
      "union {" ^
      catmap "; "
        (fun (s,t) -> string_of_id s ^ " of " ^ string_of_typecode t ^ ";")
        ts ^
      "}"

  | EXPR_arrayof (_,t) -> "[|" ^ catmap ", " sme t ^ "|]"
  (*
  | EXPR_dot (_,(e,n,ts)) ->
    "get_" ^ n ^
      begin match ts with | [] ->
        "" | _ -> "[" ^ catmap "," string_of_typecode ts^ "]"
      end ^
      "(" ^ se e ^ ")"
  *)

  | EXPR_dot (_,(e1,e2)) ->
    "(" ^ se e1 ^ "." ^ se e2 ^  ")"

  | EXPR_lambda (_,(kind,vs,paramss,ret, sts)) ->
    "(" ^ string_of_funkind kind ^ " " ^
     string_of_vs vs ^
    catmap " "
    (fun ps -> "(" ^ string_of_parameters ps ^ ")") paramss
    ^
    (match ret with
    | TYP_none -> ""
    | _ -> ": " ^string_of_typecode ret) ^
    " = " ^
    string_of_compound 0 sts ^ ")"

  | EXPR_ctor_arg (_,(cn,e)) ->
    "ctor_arg " ^ sqn cn ^ "(" ^
    se e ^ ")"

  | EXPR_case_arg (_,(n,e)) ->
    "case_arg " ^ si n ^ "(" ^
    se e ^ ")"

  | EXPR_case_index (_,e) ->
    "caseno (" ^ se e ^ ")"

  | EXPR_match_ctor (_,(cn,e)) ->
    "match_ctor " ^ sqn cn ^ "(" ^
    se e ^ ")"

  | EXPR_match_case (_,(v,e)) ->
    "match_case " ^ si v ^ "(" ^
    se e ^ ")"

  | EXPR_match (_,(e, ps)) ->
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
  | EXPR_type_match (_,(e, ps)) ->
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

  | EXPR_type_match (_,(e, ps)) ->
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

  | EXPR_range_check (_,mi,v,mx) ->
    "range_check " ^ se mi ^ " <= " ^ se v ^ " < " ^ se mx

  | EXPR_extension (sr, bases, extension) ->
    "extend " ^ catmap "," se bases ^ " with " ^ se extension

 
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
    | TYP_tuple_cons (sr, t1, t2) -> 6, st 4 t1 ^ "**" ^ st 4 t2

    | TYP_index (sr,name,idx) -> 0, name ^ "<" ^ string_of_bid idx ^ ">"
    | TYP_void _ -> 0, "void"
    | TYP_name (_,name,ts) ->
        0, string_of_id name ^
        (
          if List.length ts = 0 then ""
          else "[" ^ catmap ", " string_of_typecode ts ^ "]"
        )
    | TYP_case_tag (_,v) -> 0, "case " ^ si v
    | TYP_typed_case (_,v,t) ->
      0, "(case " ^ si v ^ " of " ^ string_of_typecode t ^ ")"

    | TYP_lookup (_,(e,name, ts)) ->
        0,
        "(" ^ string_of_expr e ^ ")::" ^ string_of_id name ^
        (
          if length ts = 0 then "" else
          "[" ^ catmap ", " string_of_typecode ts ^ "]"
        )
    | TYP_callback (_,name) -> 0, "callback " ^ string_of_qualified_name name

    | TYP_suffix (_,(name,suf)) ->
      0,
      string_of_qualified_name name ^ " of (" ^ string_of_typecode suf ^ ")"

    | TYP_patvar (sr,s) -> 0, "?" ^ string_of_id s
    | TYP_patany sr -> 0,"ANY"
    | TYP_none -> 0,"<none>"
    | TYP_ellipsis-> 0,"..."

    | TYP_type_match (e,ps) -> 0,
      "typematch " ^ string_of_typecode e ^ " with " ^
      catmap ""
      (fun (p,t) ->
      "\n  | " ^ string_of_typecode p ^ " => " ^ string_of_typecode t
      )
      ps
      ^
      "\nendmatch"

    | TYP_var i -> 0,"<var " ^ string_of_bid i ^ ">"
    | TYP_unitsum k ->
      0,
      begin match k with
      | 0 -> "void"
      | 1 -> "unit"
      | 2 -> "bool"
      | _ -> si k
      end

    | TYP_tuple ls ->
      begin match ls with
      | [] -> 0,"unit"
      | _ -> 4, cat " * " (map (st 4) ls)
      end

    | TYP_record ls ->
      begin match ls with
      | [] -> 0,"unit"
      | _ ->
          0, "struct {" ^
          catmap "" (fun (s,t) -> string_of_id s ^ ":" ^ st 0 t ^ "; ") ls ^
          "}"
      end

    | TYP_variant ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ ->
          0, "union {" ^
          catmap "" (fun (s,t) -> string_of_id s ^ " of " ^ st 0 t ^ "; ") ls ^
          "}"
      end

    | TYP_sum ls ->
      begin match ls with
      | [] -> 0,"void"
      | [TYP_tuple[];TYP_tuple[]] -> 0,"bool"
      | _ -> 5,cat " + " (map (st 5) ls)
      end

    | TYP_typeset ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ -> 0,"{" ^ cat ", " (map (st 0) ls) ^  "}"
      end

    | TYP_intersect ls ->
      let ls = filter (fun t -> t <> TYP_tuple []) ls in
      begin match ls with
      | [] -> 0,"unit"
      | _ -> 9,cat " & " (map (st 9) ls)
      end

    | TYP_setintersection ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ -> 9,cat " && " (map (st 9) ls)
      end

    | TYP_setunion ls ->
      begin match ls with
      | [] -> 0,"unit"
      | _ -> 9,cat " || " (map (st 9) ls)
      end

    | TYP_function (args, result) ->
      9,st 9 args ^ " -> " ^ st 9 result

    | TYP_cfunction (args, result) ->
      9,st 9 args ^ " --> " ^ st 9 result

    | TYP_array (vt,it) -> 3, st 1 vt ^ "^" ^ st 3 it

    | TYP_pointer t -> 1,"&" ^ st 1 t
(*    | TYP_lvalue t -> 0,"lvalue[" ^ st 1 t ^"]" *)

    | TYP_typeof e -> 0,"typeof(" ^ string_of_expr e ^ ")"
    | TYP_as (t,s) -> 11, st 11 t ^ " as " ^ string_of_id s

    | TYP_dual t -> 2,"~"^ st 2 t

    | TYP_isin (t1,t2) -> 6,st 2 t1 ^ " isin " ^ st 6 t2

    | TYP_apply (t1,t2) -> 2,st 2 t1 ^ " " ^ st 2 t2
    | TYP_type -> 0,"TYPE"
    | TYP_type_tuple ls ->
      4, cat ", " (map (st 4) ls)

    | TYP_typefun (args,ret,body) ->
       10,
       (
         "fun(" ^ cat ", "
         (
           map
           (fun (n,t) -> string_of_id n ^ ": " ^ st 10 t)
           args
         ) ^
         "): " ^ st 0 ret ^ "=" ^ st 10 body
       )
    | TYP_type_extension (sr,ts,t) ->
      0,"extend {" ^ cat ", " (map (st 0) ts) ^ " with " ^ st 0 t ^ "}"

  in
    if iprec >= prec
    then "(" ^ txt ^ ")"
    else txt

and string_of_typecode tc = st 99 tc

and qualified_name_of_index_with_vs sym_table index =
  let parent, sym = Flx_sym_table.find_with_parent sym_table index in
  match parent with
  | Some parent ->
      qualified_name_of_index_with_vs sym_table parent ^
      string_of_id sym.Flx_sym.id ^
      string_of_ivs sym.Flx_sym.vs ^
      "::"
  | None ->
      (* If this entity has no parent, its the root module, and we don't bother
       * to print its name as a prefix *)
      ""

and string_of_dir_t d = match d with
| DIR_open (ivs,qn) -> "DIR_open " ^ string_of_qualified_name qn
| DIR_inject_module (ivs,qn) -> "DIR_inject_module " ^ string_of_qualified_name qn
| DIR_use (name, qn) -> "DIR_use " ^ name ^"<-"  ^ string_of_qualified_name qn

and qualified_name_of_index' sym_table index =
  let parent, sym = Flx_sym_table.find_with_parent sym_table index in
  begin match parent with
  | Some parent -> qualified_name_of_index_with_vs sym_table parent
  | None -> ""
  end ^
  string_of_id sym.Flx_sym.id

and qualified_name_of_index sym_table index =
  try qualified_name_of_index' sym_table index ^ "<" ^ string_of_bid index ^ ">"
  with Not_found -> "index_"^ string_of_bid index

and get_name_parent bsym_table index =
  try
    let parent, bsym = Flx_bsym_table.find_with_parent bsym_table index in
    string_of_id (Flx_bsym.id bsym), parent
  with Not_found -> "index_" ^ string_of_bid index, None

and qualified_name_of_bindex bsym_table index =
  let name,parent = get_name_parent bsym_table index in
  match parent with
  | Some index' ->
    qualified_name_of_bindex bsym_table index' ^ "::" ^ name
  | None -> name

and bound_name_of_bindex bsym_table index =
  let name,parent = get_name_parent bsym_table index in
  name ^ "<" ^ (string_of_bid index) ^ ">"

(* fixppoint labeller .. very sloppy, ignores precedence .. *)
and get_label i =
  if i = 0 then ""
  else
    let ch = Char.chr (i mod 26 + Char.code('a')-1) in
    get_label (i/26) ^ String.make 1 ch

and string_of_fixpoints depth fixlist =
  match fixlist with
  | (d,lab) :: t when d = depth ->
    let txt,lst = string_of_fixpoints depth t in
    " as " ^ lab ^ " " ^ txt, lst
  | _ -> "", fixlist

and sb bsym_table depth fixlist counter prec tc =
  let sbt prec t = sb bsym_table (depth+1) fixlist counter prec t in
  let iprec, term =
    match tc with
    | BTYP_none -> 0,"none"
    | BTYP_tuple_cons (t1,t2) -> 
      5,(sbt 5 t1) ^ " ** " ^ (sbt 5 t2)

    | BTYP_type_match (t,ps) ->
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

    | BTYP_fix (i,mt) ->
       0,
       (
         try assoc (depth+i) !fixlist
         with Not_found ->
           incr counter; (* 'a is 1 anyhow .. *)
           let lab = "fix" ^ si i ^ "_"^get_label !counter in
           fixlist := (depth+i,lab) :: !fixlist;
           lab
       )

    | BTYP_type_var (i,mt) -> 0,"<T" ^ string_of_bid i ^
      (match mt with BTYP_type i ->"" | _ -> ":"^sbt 0 mt)^
      ">"

    | BTYP_inst (i,ts) ->
      0, (match bsym_table with | Some tab -> qualified_name_of_bindex tab i | None -> "<Prim " ^ si i^">") ^
      (if List.length ts = 0 then "" else
      "[" ^cat ", " (map (sbt 9) ts) ^ "]"
      )

    | BTYP_tuple ls ->
      begin match ls with
      | [] -> 0,"unit"
      | [x] -> failwith ("UNEXPECTED TUPLE OF ONE ARGUMENT " ^ sbt 9 x)
      | _ -> 4,cat " * " (map (sbt 4) ls)
      end

    | BTYP_record (n,ls) ->
      begin match ls with
      | [] -> 0,"record_unit"
      | _ -> 0,"struct "^n^" {"^catmap "" (fun (s,t)->s^":"^sbt 0 t^";") ls ^"}"
      end

    | BTYP_variant ls ->
      begin match ls with
      | [] -> 0,"void"
      | _ -> 0,"union {"^catmap "" (fun (s,t)->s^" of "^sbt 0 t^";") ls ^"}"
      end

    | BTYP_unitsum k ->
      begin match k with
      | 0 -> 0,"/*unitsum*/void"
      | 2 -> 0,"bool"
      | _ -> 0,si k
      end

    | BTYP_sum ls ->
      begin match ls with
      | [] -> 9,"UNEXPECTED EMPTY SUM = void"
      | [BTYP_tuple[]; BTYP_tuple[]] -> 0,"unexpected bool"
      | [x] -> (* failwith *) (9,"UNEXPECTED SUM OF ONE ARGUMENT " ^ sbt 9 x)
      | _ ->
        if (all_units ls)
        then
          0,si (length ls)
        else
          5,cat " + " (map (sbt 5) ls)
      end

    | BTYP_type_set ls ->
      begin match ls with
      | [] -> 9,"UNEXPECTED EMPTY TYPESET = void"
      | _ ->
          0,"{" ^ cat "," (map (sbt 0) ls) ^ "}"
      end

    | BTYP_intersect ls ->
      begin match ls with
      | [] -> 9,"/*intersect*/void"
      | _ ->
          4,cat " and " (map (sbt 5) ls)
      end

    | BTYP_type_set_intersection ls ->
      begin match ls with
      | [] -> 9,"/*typesetintersect*/void"
      | _ ->
          4,cat " && " (map (sbt 5) ls)
      end

    | BTYP_type_set_union ls ->
      begin match ls with
      | [] -> 9,"/*typesetunion*/unit"
      | _ ->
          4,cat " || " (map (sbt 5) ls)
      end

    | BTYP_function (args, result) ->
      6,(sbt 6 args) ^ " -> " ^ (sbt 6 result)

    | BTYP_cfunction (args, result) ->
      6,(sbt 6 args) ^ " --> " ^ (sbt 6 result)

    | BTYP_array (t1,t2) ->
      begin match t2 with
      | BTYP_unitsum k -> 3, sbt 3 t1 ^"^"^si k
      | _ -> 3, sbt 3 t1 ^"^"^sbt 3 t2
      end

    | BTYP_pointer t -> 1,"&" ^ sbt 1 t
    | BTYP_void -> 0,"void"

    | BTYP_type_apply (t1,t2) -> 2,sbt 2 t1 ^ " " ^ sbt 2 t2
    | BTYP_type i -> 0,"TYPE " ^ si i
    | BTYP_type_tuple ls ->
      begin match ls with
      | [] -> 0,"UNEXPECTED TYPE TUPLE NO ARGS"
      | _ -> 4, cat ", " (map (sbt 4) ls)
      end

    | BTYP_type_function (args,ret,body) ->
       8,
       (
         "fun (" ^ cat ", "
         (
           map
           (fun (i,t)-> "T" ^ string_of_bid i ^ ": " ^ sbt 8 t)
           args
         ) ^
         "): " ^ sbt 0 ret ^ "=" ^ sbt 8 body ^" endfun"
       )
  in
    let txt,lst = string_of_fixpoints depth !fixlist in
    fixlist := lst;
    if txt = "" then
      if iprec >= prec then "(" ^ term ^ ")"
      else term
    else
    "(" ^ term ^ txt ^ ")"

and string_of_btypecode bsym_table tc =
  let fixlist = ref [] in
  let term = sb bsym_table 0 fixlist (ref 0) 99 tc in
  let bad = ref "" in
  while List.length !fixlist > 0 do
    match !fixlist with
    | (d,v)::t ->
      bad := !bad ^ " [Free Fixpoint " ^ si d ^ " " ^ v ^"]";
      fixlist := t
    | [] -> assert false
  done;
  term ^ !bad

and sbt a b = string_of_btypecode (Some a) b
and qsbt a b = string_of_btypecode None b

and string_of_basic_parameters (ps: simple_parameter_t list) =
  cat
    ", "
    (map (fun (x,y) -> string_of_id x ^ ": " ^ (string_of_typecode y)) ps)

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
        string_of_id x ^ ": "^(string_of_typecode y) ^
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
and string_of_iparameters sym_table ps =
  let ps,traint = ps in
  cat
    ", "
    (map (fun (x,(i,y))-> x ^ "["^si i^"]: "^(string_of_typecode y)) ps)
  ^
  (match traint with
  | Some x ->  " where " ^ sbe bsym_table x
  | None -> ""
  )
*)

and string_of_basic_bparameters bsym_table ps : string =
  catmap "," begin fun {pid=x; pkind=kind; pindex=i; ptyp=y} ->
    Printf.sprintf "%s %s<%s>: %s"
      (string_of_param_kind kind)
      x
      (string_of_bid i)
      (string_of_btypecode (Some bsym_table) y)
  end ps

and string_of_bparameters bsym_table ps : string =
  let ps, traint = ps in
  string_of_basic_bparameters bsym_table ps
  ^
  (match traint with
  | Some x -> " where " ^ sbe bsym_table x
  | None -> ""
  )

and string_of_arguments ass =
  catmap ", " string_of_expr ass


and string_of_component level (name, typ) =
   spaces level ^ name ^ ": " ^ (string_of_typecode typ)

and string_of_pattern p =
  let se e = string_of_expr e in
  match p with
  | PAT_coercion (_,p,t) -> "(" ^ string_of_pattern p ^ ":" ^ string_of_typecode t ^ ")"
  | PAT_none _ -> "<none>"
  | PAT_literal (sr,l) -> string_of_literal l

  | PAT_range (sr,l1,l2) -> string_of_literal l1 ^ ".." ^ string_of_literal l2
  | PAT_name (_,s) -> string_of_id s
  | PAT_tuple (_,ps) -> "(" ^ catmap ", "  string_of_pattern ps ^ ")"
  | PAT_tuple_cons (_,a,b) -> string_of_pattern a ^ ",," ^ string_of_pattern b
  | PAT_any _ -> "any"
  | PAT_const_ctor (_,s) -> "|" ^ string_of_qualified_name s
  | PAT_nonconst_ctor (_,s,p)-> "|" ^ string_of_qualified_name s ^ " " ^ string_of_pattern p
  | PAT_as (_,p,n) ->
    begin match p with
    | PAT_any _ -> string_of_id n
    | _ -> "(" ^ string_of_pattern p ^ " as " ^ (string_of_id n) ^ ")"
    end
  | PAT_when (_,p,e) -> "(" ^ string_of_pattern p ^ " when " ^ se e ^ ")"
  | PAT_record (_,ps) ->
     "struct { " ^ catmap "; " (fun (s,p) ->
       string_of_id s ^ "=" ^ string_of_pattern p) ps ^ "; }"
  | PAT_expr (_,e) -> "$(" ^ string_of_expr e ^ ")"

and string_of_letpat p =
  match p with
  | PAT_name (_,s) -> string_of_id s
  | PAT_tuple (_,ps) -> "(" ^ catmap ", " string_of_letpat ps ^ ")"
  | PAT_tuple_cons (_,a,b) -> string_of_pattern a ^ ",," ^ string_of_pattern b
  | PAT_any _ -> "_"
  | PAT_const_ctor (_,s) -> "|" ^ string_of_qualified_name s
  | PAT_nonconst_ctor (_,s,p)-> "|" ^ string_of_qualified_name s ^ " " ^ string_of_letpat p
  | PAT_as (_,p,n) -> "(" ^ string_of_pattern p ^ " as " ^ string_of_id n ^ ")"
  | PAT_record (_,ps) ->
     "struct { " ^ catmap "; " (fun (s,p) -> string_of_id s ^ "="^string_of_pattern p) ps ^"; }"

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
  | TYP_tuple [] -> ""
  | _ -> " of " ^ string_of_typecode ty

and special_string_of_btypecode bsym_table ty =  (* used for constructors *)
  match ty with
  | BTYP_tuple [] -> ""
  | _ -> " of " ^ string_of_btypecode (Some bsym_table) ty

(*
and string_of_maybe_tpattern = function
  | TPAT_any -> ""
  | t -> ": " ^ string_of_tpattern t
*)

and string_of_maybe_typecode = function
  | TYP_patany _ -> ""
  | t -> ": " ^ string_of_typecode t

and string_of_tconstraint = function
  | TYP_tuple [] -> ""
  | TYP_intersect [TYP_tuple []] -> ""
  | t -> let x = string_of_typecode t in
    if x <> "unit" then " where " ^ x else ""

and string_of_tclass_req qn = string_of_qualified_name qn

and string_of_tclass_reqs = function
  | [] -> ""
  | t -> " with " ^ catmap "," string_of_tclass_req t

and string_of_tcon {raw_type_constraint=tcon; raw_typeclass_reqs=rtcr} =
  string_of_tconstraint tcon ^ string_of_tclass_reqs rtcr

and string_of_plain_ivs ivs =
  catmap ", "
  (fun (name,ix,tpat) -> string_of_id name ^ string_of_maybe_typecode tpat)
  ivs

and string_of_ivs (ivs,({raw_type_constraint=tcon; raw_typeclass_reqs=rtcr} as con)) =
  match ivs,tcon,rtcr with
  | [],TYP_tuple [],[] -> ""
  | _ ->
      let ivs = catmap ", "
        (fun (name,ix,tpat) -> string_of_id name ^ string_of_maybe_typecode tpat)
        ivs
      in
      Printf.sprintf "[%s%s]" ivs (string_of_tcon con)

and string_of_vs (vs,({raw_type_constraint=tcon; raw_typeclass_reqs=rtcr} as con)) =
  match vs,tcon,rtcr with
  | [],TYP_tuple [],[] -> ""
  | _ ->
      let vs = catmap ", "
        (fun (name,tpat) -> string_of_id name ^ string_of_maybe_typecode tpat)
        vs
      in
      Printf.sprintf "[%s%s]" vs (string_of_tcon con)

and string_of_bvs' bvs =
  catmap ", " (fun (s, i)-> Printf.sprintf "%s<%s>" s (string_of_bid i)) bvs

and string_of_bvs = function
  | [] -> ""
  | bvs -> Printf.sprintf "[%s]" (string_of_bvs' bvs)

and string_of_bvs_cons bsym_table vs cons = match vs,cons with
  | [], BTYP_tuple [] -> ""
  | bvs, cons ->
      Printf.sprintf "[%s%s]"
        (string_of_bvs' bvs)
        (match cons with
        | BTYP_tuple [] -> ""
        | _ -> " where " ^ sbt bsym_table cons)

and string_of_ts bsym_table ts = String.concat "," (List.map (string_of_btypecode (Some bsym_table)) ts)

and string_of_inst bsym_table = function
  | [] -> ""
  | ts -> Printf.sprintf "[%s]" (catmap ", " (string_of_btypecode (Some bsym_table)) ts)

and sl x = string_of_lvalue x
and string_of_lvalue (x,t) =
  begin match x with
  | `Val (sr,x) -> "val " ^ string_of_id x
  | `Var (sr,x) -> "var " ^ string_of_id x
  | `Name (sr,x) -> string_of_id x
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
| `GeneratedInline -> "inline(generated)"
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
| `ImPure -> "impure"
| `Total -> "total"
| `Partial -> "partial"
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
| `Tag s -> "Tag " ^ s

and string_of_properties ps =
  match ps with
  | [] -> ""
  | ps -> catmap " " string_of_property ps ^ " "

and string_of_code_spec =
  let module CS = Flx_code_spec in
  function
  | CS.Str_template s -> "\"" ^ s ^  "\""
  | CS.Str s -> "c\"" ^ s ^  "\""
  | CS.Virtual -> "virtual"
  | CS.Identity -> "identity"

and string_of_long_code_spec c =
  let module CS = Flx_code_spec in
  let triple_quote = "\"\"\"" in
  match c with
  | CS.Str_template s -> triple_quote ^ s ^ triple_quote
  | CS.Str s -> "c" ^ triple_quote ^ s ^ triple_quote
  | CS.Virtual -> "virtual"
  | CS.Identity -> "identity"

and string_of_raw_req = function
  | Named_req s -> string_of_qualified_name s
  | Body_req c -> "body " ^ string_of_code_spec c
  | Header_req c -> "header " ^ string_of_code_spec c
  | Property_req s -> "property \"" ^ s ^ "\""
  | Package_req c -> "package " ^ string_of_code_spec c
  | Scanner_req c -> "scanner " ^ string_of_code_spec c
  | Finaliser_req c -> "finaliser " ^ string_of_code_spec c
  | Encoder_req c -> "encoder " ^ string_of_code_spec c
  | Decoder_req c -> "decoder " ^ string_of_code_spec c

(* fairly lame excess brackets here *)
and string_of_raw_req_expr = function
  | RREQ_atom r -> string_of_raw_req r
  | RREQ_and (a,b) -> "(" ^ string_of_raw_req_expr a ^ ") and (" ^ string_of_raw_req_expr b ^")"
  | RREQ_or (a,b) -> "(" ^ string_of_raw_req_expr a ^ ") or (" ^ string_of_raw_req_expr b ^")"
  | RREQ_true -> "(true)"
  | RREQ_false -> "(false)"

(* fairly lame excess brackets here *)
and string_of_named_req_expr = function
  | NREQ_atom r -> string_of_qualified_name r
  | NREQ_and (a,b) -> "(" ^ string_of_named_req_expr a ^ ") and (" ^ string_of_named_req_expr b ^")"
  | NREQ_or (a,b) -> "(" ^ string_of_named_req_expr a ^ ") or (" ^ string_of_named_req_expr b ^")"
  | NREQ_true -> "(true)"
  | NREQ_false -> "(false)"

and string_of_raw_reqs x = match x with
  | RREQ_true -> "" (* required nothing *)
  | x -> " requires " ^ string_of_raw_req_expr x

and string_of_named_reqs x = match x with
  | NREQ_true -> "" (* requires nothing *)
  | x -> " requires " ^ string_of_named_req_expr x

and string_of_base_qual = function
| `Incomplete -> "incomplete"
| `Pod -> "pod"
| `GC_pointer -> "GC_pointer"

and string_of_qual = function
| #base_type_qual_t as x -> string_of_base_qual x
| `Raw_needs_shape t -> "needs_shape(" ^ string_of_typecode t ^ ")"
| `Scanner cs -> "scanner(" ^ string_of_code_spec cs ^ ")"
| `Finaliser cs -> "finaliser(" ^ string_of_code_spec cs ^ ")"
| `Encoder cs -> "encoder(" ^ string_of_code_spec cs ^ ")"
| `Decoder cs -> "decoder(" ^ string_of_code_spec cs ^ ")"

and string_of_bqual bsym_table = function
| #base_type_qual_t as x -> string_of_base_qual x
| `Bound_needs_shape t -> "needs_shape(" ^ string_of_btypecode (Some bsym_table) t ^ ")"
| `Scanner cs -> "scanner(" ^ string_of_code_spec cs ^ ")" 
| `Finaliser cs -> "finaliser(" ^ string_of_code_spec cs ^ ")" 
| `Encoder cs -> "encoder(" ^ string_of_code_spec cs ^ ")" 
| `Decoder cs -> "decoder(" ^ string_of_code_spec cs ^ ")" 

and string_of_quals qs = catmap " " string_of_qual qs
and string_of_bquals bsym_table qs = catmap " " (string_of_bqual bsym_table) qs

and string_of_ast_term level (term:ast_term_t) =
  let sast level x = string_of_ast_term level x in
  match term with
  | Statement_term s -> string_of_statement (level+1) s
  | Statements_term ss -> catmap "\n" (string_of_statement (level+1)) ss
  | Expression_term e -> string_of_expr e
  | Identifier_term s -> s
  | Keyword_term s -> s
  | Apply_term (t,ts) -> "apply("^ sast 0 t ^ ",(" ^ catmap ", " (sast 0) ts ^ "))"

and string_of_class_component level mem =
  let kind, name, mix,vs,ty,cc = match mem with
  | `MemberVar (name,typ,cc) -> "var",name,None,dfltvs,typ,cc
  | `MemberVal (name,typ,cc) -> "val",name,None,dfltvs,typ,cc
  | `MemberFun (name,mix,vs,typ,cc) -> "fun",name,mix,vs,typ,cc
  | `MemberProc (name,mix,vs,typ,cc) -> "proc",name,mix,vs,typ,cc
  | `MemberCtor (name,mix,typ,cc) -> "ctor",name,mix,dfltvs,typ,cc
  in
    (spaces (level+1)) ^
    kind ^ " " ^ name ^ string_of_vs vs ^ ": " ^ string_of_typecode ty ^
    (match cc with None -> "" | Some cc -> string_of_code_spec cc) ^
    ";"

and string_of_ikind = function
  | `Header -> "header "
  | `Body -> "body "
  | `Package -> "package "

and string_of_axiom_method a = match a with
  | Predicate e -> string_of_expr e
  | Equation (l,r) -> string_of_expr l ^ " = " ^ string_of_expr r

and string_of_baxiom_method bsym_table a = match a with
  | `BPredicate e -> string_of_expr e
  | `BEquation (l,r) -> sbe bsym_table l ^ " = " ^ sbe bsym_table r

and string_of_statements level ss = String.concat "" (List.map (fun s -> string_of_statement level s) ss)

and string_of_funkind kind = 
  match kind with
    | `Function -> "fun"
    | `CFunction -> "cfun"
    | `GeneratedInlineProcedure -> "inline procedure(generated,block)"
    | `GeneratedInlineFunction-> "inline function(lambda)"
    | `InlineFunction -> "inline fun"
    | `NoInlineFunction -> "noinline fun"
    | `Virtual -> "virtual fun"
    | `Ctor -> "ctor"
    | `Generator -> "generator"
    | `GeneratorMethod-> "method generator"
    | `Method-> "method"
    | `Object -> "object"

and string_of_statement level s =
  let se e = string_of_expr e in
  let sqn n = string_of_qualified_name n in
  match s with
  | STMT_try _ -> spaces level ^ "try"
  | STMT_endtry _ -> spaces level ^ "endtry"
  | STMT_catch (_,id,t) -> spaces level ^ "catch "^id ^ " : " ^ string_of_typecode t^" => "

  | STMT_seq (_,sts) -> catmap "" (string_of_statement level) sts
  (*
  | STMT_public (_,s,st) ->
    "\n" ^
    spaces level ^ "public '" ^ s ^ "'\n" ^
    string_of_statement (level+1) st
  *)

  | STMT_private (_,st) ->
    spaces level ^ "private " ^
    string_of_statement 0 st

  | STMT_export_fun (_,flx_name,cpp_name) ->
    spaces level ^
    "export fun " ^
    string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | STMT_export_cfun (_,flx_name,cpp_name) ->
    spaces level ^
    "export cfun " ^
    string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | STMT_export_python_fun (_,flx_name,cpp_name) ->
    spaces level ^
    "export python fun " ^
    string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | STMT_export_type (_,flx_type,cpp_name) ->
    spaces level ^
    "export type (" ^
    string_of_typecode flx_type ^
    ") as \"" ^ cpp_name ^ "\";"

  | STMT_label (_,s) -> string_of_id s ^ ":"
  | STMT_goto (_,s) -> spaces level ^ "goto " ^ string_of_id s ^ ";"

  | STMT_assert (_,e) -> spaces level ^ "assert " ^ se e ^ ";"

  | STMT_init (_,v,e) ->
    spaces level ^ string_of_id v ^ " := " ^ se e ^ ";"

  | STMT_comment (_,s) -> spaces level ^ "// " ^ s

  | STMT_open (_,vs,n) ->
    spaces level ^ "open " ^ string_of_vs vs ^ " " ^ sqn n ^ ";"

  | STMT_inject_module (_,vs,n) ->
    spaces level ^ "inherit " ^ string_of_vs vs ^ " " ^ sqn n ^ ";"

  | STMT_include (_,s) ->
    spaces level ^ "include " ^ string_of_string s ^ ";"

  | STMT_use (_,n,qn) ->
    spaces level ^ "use " ^ string_of_id n ^ " = " ^ sqn qn ^ ";"

  | STMT_type_alias (_,t1,vs,t2) ->
    spaces level ^ "typedef " ^ string_of_id t1 ^ string_of_vs vs ^
    " = " ^
    string_of_typecode t2 ^ ";"

  | STMT_inherit (_,name,vs,qn) ->
    spaces level ^ "inherit " ^ string_of_id name ^ string_of_vs vs ^
    " = " ^
    string_of_qualified_name qn ^ ";"

  | STMT_inherit_fun (_,name,vs,qn) ->
    spaces level ^ "inherit fun " ^ string_of_id name ^ string_of_vs vs ^
    " = " ^
    string_of_qualified_name qn ^ ";"

  | STMT_untyped_module (_,name, vs,sts)  ->
    spaces level ^ "module " ^ string_of_id name ^ string_of_vs vs ^
    " = " ^
    "\n" ^
    string_of_compound level sts

  | STMT_struct (_,name, vs, cs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ string_of_id name ^ ": " ^
      string_of_typecode ty ^ ";"
    in
    spaces level ^ "struct " ^ string_of_id name ^ string_of_vs vs ^ " = " ^
    spaces level ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    spaces level ^ "}"

  | STMT_cstruct (_,name, vs, cs, reqs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ string_of_id name ^ ": " ^
      string_of_typecode ty ^ ";"
    in
    spaces level ^ "cstruct " ^ string_of_id name ^ string_of_vs vs ^ " = " ^
    spaces level ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    spaces level ^ "}" ^ string_of_raw_reqs reqs ^";"

  | STMT_typeclass (_,name, vs, sts) ->
    spaces level ^ "typeclass " ^ string_of_id name ^ string_of_vs vs ^ " = " ^
    string_of_compound level sts

  | STMT_instance (_,vs,name, sts) ->
    spaces level ^ "instance " ^ string_of_vs vs ^ " " ^
    string_of_qualified_name name ^ " = " ^
    string_of_compound level sts

  | STMT_union (_,name, vs,cs) ->
    let string_of_union_component (name,cval, vs,ty) =
      (spaces (level+1)) ^ "|" ^ string_of_id name ^
      (match cval with None -> "" | Some i -> "="^ si i) ^
      special_string_of_typecode ty
    in
    spaces level ^ "union " ^ string_of_id name ^ string_of_vs vs ^ " = " ^
    spaces level ^ "{\n" ^
    catmap ";\n" string_of_union_component cs ^ "\n" ^
    spaces level ^ "}"

  | STMT_ctypes (_,names, quals, reqs) -> spaces level ^
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "ctypes " ^ catmap "," (fun (_,name) -> string_of_id name) names ^
    string_of_raw_reqs reqs ^
    ";"

  | STMT_abs_decl (_,t,vs, quals, ct, reqs) -> spaces level ^
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "type " ^ string_of_id t ^ string_of_vs vs ^
    " = " ^ string_of_code_spec ct ^
    string_of_raw_reqs reqs ^
    ";"

  | STMT_newtype (_,t,vs, nt) -> spaces level ^
    "type " ^ string_of_id t ^ string_of_vs vs ^
    " = new " ^ string_of_typecode nt ^
    ";"

  | STMT_callback_decl (_,name,args,result, reqs) -> spaces level ^
    "callback " ^ string_of_id name ^ ": " ^
    (string_of_typecode (TYP_tuple args)) ^ " -> " ^
    (string_of_typecode result) ^
    string_of_raw_reqs reqs ^
    ";"

  | STMT_fun_decl (_,name,vs,args, result, code, reqs,prec) ->
    spaces level ^
    "fun " ^ string_of_id name ^ string_of_vs vs ^
    ": " ^
    (string_of_typecode (TYP_tuple args)) ^ " -> " ^
    (string_of_typecode result) ^
    " = " ^ string_of_code_spec code ^
    (if prec = "" then "" else ":"^prec^" ")^
    string_of_raw_reqs reqs ^
    ";"

  | STMT_const_decl (_,name,vs,typ, code, reqs) ->
    spaces level ^
     "const " ^ string_of_id name ^
     ": " ^ string_of_typecode typ ^
     " = "^string_of_code_spec code^
     string_of_raw_reqs reqs ^
     ";"

  | STMT_insert (_,n,vs,s, ikind, reqs) ->
    spaces level ^ string_of_ikind ikind ^
    string_of_id n ^ string_of_vs vs ^
    "\n" ^ string_of_code_spec s ^ " " ^
     string_of_raw_reqs reqs ^
    ";\n"

  | STMT_code (_,s) ->
    "code \n" ^ string_of_long_code_spec s ^ ";\n"

  | STMT_noreturn_code (_,s) ->
    "noreturn_code \n" ^ string_of_long_code_spec s ^ ";\n"

  | STMT_reduce (_,name, vs, ps, rsrc, rdst) ->
    spaces level ^
    "reduce " ^ string_of_id name ^ string_of_vs vs ^
    "("^string_of_basic_parameters ps^"): "^
    string_of_expr rsrc ^ " => " ^ string_of_expr rdst ^
    ";\n"

  | STMT_axiom (_,name, vs, ps, a) ->
    spaces level ^
    "axiom " ^ string_of_id name ^ string_of_vs vs ^
    "("^string_of_parameters ps^"): "^
    string_of_axiom_method a ^
    ";\n"

  | STMT_lemma (_,name, vs, ps, a) ->
    spaces level ^
    "lemma " ^ string_of_id name ^ string_of_vs vs ^
    "("^string_of_parameters ps^"): "^
    string_of_axiom_method a ^
    ";\n"

  | STMT_function (_,name, vs, ps, (res,post), props, ss) ->
    spaces level ^
    string_of_properties props ^
    "fun " ^ string_of_id name ^ string_of_vs vs ^
    "("^string_of_parameters ps^"): "^string_of_typecode res^
    (match post with
    | None -> ""
    | Some x -> " when " ^ string_of_expr x
    )^
    begin match ss with
    | [STMT_fun_return (_,e)] -> " => " ^ se e ^ ";\n"
    | _ -> "\n" ^ string_of_compound level ss
    end

  | STMT_curry (_,name, vs, pss, (res,traint) , kind, ss) ->
    spaces level ^ string_of_funkind kind ^ " "
    ^
    string_of_id name ^ string_of_vs vs ^
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
    | [STMT_fun_return (_,e)] -> " => " ^ se e ^ ";\n"
    | _ -> "\n" ^ string_of_compound level ss
    end

  | STMT_macro_val (_,names, e) ->
    spaces level ^
    "macro val " ^ String.concat ", " (List.map string_of_id names) ^ " = " ^
    se e ^
    ";"

  | STMT_val_decl (_,name, vs,ty, value) ->
    spaces level ^
    "val " ^ string_of_id name ^
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

  | STMT_ref_decl (_,name, vs,ty, value) ->
    spaces level ^
    "ref " ^ string_of_id name ^
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


  | STMT_lazy_decl (_,name, vs,ty, value) ->
    spaces level ^
    "fun " ^ string_of_id name ^
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

  | STMT_var_decl (_,name, vs,ty, value) ->
    spaces level ^
    "var " ^ string_of_id name ^
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

  | STMT_macro_forall (_,vs,e,sts) ->
    let se e = string_of_expr e in
    spaces level
    ^ "forall " ^ String.concat ", " (List.map string_of_id vs) ^
    " in " ^ se e ^ " do\n" ^
    catmap "\n" (string_of_statement (level +2)) sts ^
    spaces level ^ "done;"

  | STMT_call (_,pr, args) ->
    spaces level
    ^ "call " ^ se pr ^ " " ^ se args ^ ";"

  | STMT_assign (_,name,l,r) ->
    spaces level
    ^ "call " ^ string_of_id name ^ "(" ^ sl l ^ "," ^ se r ^ ");"

  | STMT_cassign (_,l,r) ->
    spaces level ^
    se l ^ " = " ^ se r ^ ";"

  | STMT_jump (_,pr, args) ->
    spaces level
    ^ "jump " ^ se pr ^ " " ^ se args ^ ";"

  | STMT_loop (_,pr, args) ->
    spaces level
    ^ "call " ^ string_of_id pr ^ " " ^ se args ^ ";"

  | STMT_nop (_,s) -> spaces level ^ "{/*"^s^"*/;}"

  | STMT_ifgoto (_,e,lab) ->
    spaces level ^
    "if("^string_of_expr e^")goto " ^ string_of_id lab ^ ";"

  | STMT_ifreturn (_,e) ->
    spaces level ^
    "if("^string_of_expr e^")return;"

  | STMT_ifdo (_,e,ss1,ss2) ->
    spaces level ^
    "if("^string_of_expr e^")do\n" ^
    catmap "\n" (string_of_statement (level+1)) ss1 ^
    spaces level ^ "else\n" ^
    catmap "\n" (string_of_statement (level+1)) ss2 ^
    spaces level ^ "done;"

  | STMT_fun_return (_,e) ->
    spaces level ^ "return " ^ (se e) ^ ";"

  | STMT_yield (_,e) ->
    spaces level ^ "yield " ^ (se e) ^ ";"

  | STMT_proc_return _ ->
    spaces level ^ "return;"

  | STMT_proc_return_from (_,s) ->
    spaces level ^ "return from "^s^";"


  | STMT_halt (_,s) ->
    spaces level ^ "halt "^string_of_string s^";"

  | STMT_trace (_,v,s) ->
    spaces level ^ "trace " ^ string_of_id v ^ ", msg=" ^
    string_of_string s ^ ";"

  | STMT_svc (_,name) ->
    spaces level ^ "read " ^ string_of_id name ^ ";"

  | STMT_scheme_string (_,s) ->
    spaces level ^ "Scheme string " ^ s ^ ";\n"

  | STMT_stmt_match (_,(e, ps)) ->
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

  | IFACE_export_cfun (flx_name,cpp_name) ->
    spc ^ "export cfun " ^ string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | IFACE_export_python_fun (flx_name,cpp_name) ->
    spc ^ "export python fun " ^ string_of_suffixed_name flx_name ^
    " as \"" ^ cpp_name ^ "\";"

  | IFACE_export_type (flx_type,cpp_name) ->
    spc ^ "export type (" ^ string_of_typecode flx_type ^
    ") as \"" ^ cpp_name ^ "\";"

and string_of_symdef entry name vs =
  let se e = string_of_expr e in
  let st t = string_of_typecode t in
  match entry with
  | SYMDEF_instance qn ->
    "instance " ^ string_of_ivs vs ^ " " ^
    string_of_qualified_name qn ^ ";\n"

  | SYMDEF_const_ctor (uidx,ut,idx,vs') ->
     st ut ^ "  const_ctor: " ^
     string_of_id name ^ string_of_ivs vs ^
     ";"

  | SYMDEF_nonconst_ctor (uidx,ut,idx,vs',argt) ->
     st ut ^ "  nonconst_ctor: " ^
     string_of_id name ^ string_of_ivs vs ^
     " of " ^ st argt ^
     ";"

  | SYMDEF_type_alias t ->
    "typedef " ^ string_of_id name ^ string_of_ivs vs ^" = " ^ st t ^ ";"

  | SYMDEF_inherit qn ->
    "inherit " ^ string_of_id name ^ string_of_ivs vs ^" = " ^
    string_of_qualified_name qn ^ ";"

  | SYMDEF_inherit_fun qn ->
    "inherit fun " ^ string_of_id name ^ string_of_ivs vs ^" = " ^
    string_of_qualified_name qn ^ ";"

  | SYMDEF_abs (quals,code, reqs) ->
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "type " ^ string_of_id name ^ string_of_ivs vs ^
    " = " ^ string_of_code_spec code ^
    string_of_named_reqs reqs ^
    ";"

  | SYMDEF_newtype (nt) ->
    "type " ^ string_of_id name ^ string_of_ivs vs ^
    " = new " ^ st nt ^
    ";"

  | SYMDEF_var (t) ->
    "var " ^ string_of_id name ^ string_of_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_val (t) ->
    "val " ^ string_of_id name ^ string_of_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_ref (t) ->
    "ref " ^ string_of_id name ^ string_of_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_lazy (t,e) ->
    "fun " ^ string_of_id name ^ string_of_ivs vs ^
    ": "^ st t ^
    "= " ^ se e ^
    ";"

  | SYMDEF_parameter (k,t) ->
    "parameter " ^ string_of_param_kind k ^ " " ^
    string_of_id name ^ string_of_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_typevar (t) ->
    "typevar " ^ string_of_id name ^ string_of_ivs vs ^":"^ st t ^ ";"

  | SYMDEF_const (props,t,ct, reqs) ->
    string_of_properties props ^
    "const " ^ string_of_id name ^ string_of_ivs vs ^":"^
    st t ^ " = " ^string_of_code_spec ct^
    string_of_named_reqs reqs ^
    ";"

  | SYMDEF_union (cts) ->
    "union " ^ string_of_id name ^ string_of_ivs vs ^ ";"

  | SYMDEF_struct (cts) ->
    "struct " ^ string_of_id name ^ string_of_ivs vs ^ ";"

  | SYMDEF_cstruct (cts, reqs) ->
    "cstruct " ^ string_of_id name ^ string_of_ivs vs ^ string_of_named_reqs reqs ^ ";"

  | SYMDEF_typeclass ->
    "typeclass " ^ string_of_id name ^ string_of_ivs vs ^ ";"

  | SYMDEF_fun (props, pts,res,cts, reqs,prec) ->
    string_of_properties props ^
    "fun " ^ string_of_id name ^ string_of_ivs vs ^
    ": " ^ st
    (
      TYP_function
      (
        (
          match pts with
          | [x] -> x
          | x -> TYP_tuple x
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
    "callback fun " ^ string_of_id name ^ string_of_ivs vs ^
    ": " ^ st
    (
      TYP_cfunction
      (
        (
          match pts with
          | [x] -> x
          | x -> TYP_tuple x
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
    string_of_id name ^ string_of_ivs vs ^
    " "^ string_of_code_spec s ^
     string_of_named_reqs reqs ^
    ";\n"

  | SYMDEF_reduce (ps,e1,e2) ->
    "reduce " ^ string_of_id name ^ string_of_ivs vs ^ ";"

  | SYMDEF_axiom (ps,e1) ->
    "axiom " ^ string_of_id name ^ string_of_ivs vs ^ ";"

  | SYMDEF_lemma (ps,e1) ->
    "lemma " ^ string_of_id name ^ string_of_ivs vs ^ ";"

  | SYMDEF_function (ps,res,props,es) ->
    let ps,traint = ps in
    string_of_properties props ^
    "fun " ^ string_of_id name ^ string_of_ivs vs ^
    ": " ^ st
    (
      TYP_function
      (
        (
          match map (fun (x,y,z,d) -> z) ps with
          | [x] -> x
          | x -> TYP_tuple x
        )
        ,
        res
      )
    ) ^
    ";"

  | SYMDEF_match_check (pat,(mvname,i))->
    "match_check " ^ string_of_id name ^ " for " ^ string_of_pattern pat ^ ";"

  | SYMDEF_module exes ->
    "module " ^ string_of_id name ^ "{"^catmap ";" (string_of_sexe 2) exes ^"};"

  | SYMDEF_root exes ->
    "root {"^catmap ";" (string_of_sexe 2) exes ^"};"

and string_of_sexe level (sr,x) = string_of_exe level x

and string_of_exe level s =
  let spc = spaces level
  and se e = string_of_expr e
  in
  match s with

  | EXE_proc_return_from s -> "return from " ^ s

  | EXE_try  -> "try"
  | EXE_catch (id,typ)  -> "catch " ^ id ^ " : " ^ string_of_typecode typ ^ " => "
  | EXE_endtry -> "endtry"

  | EXE_goto s -> spc ^ "goto " ^ s ^ ";"
  | EXE_assert e -> spc ^ "assert " ^ se e ^ ";"

  | EXE_ifgoto (e,s) -> spc ^
     "if(" ^ se e ^ ")goto " ^ s ^ ";"

  | EXE_label s -> s ^ ":"

  | EXE_comment s -> spc ^
    "// " ^ s

  | EXE_call (p,a) -> spc ^
    "call " ^
    se p ^ " " ^
    se a ^ ";"

  | EXE_jump (p,a) -> spc ^
    "jump " ^
    se p ^ " " ^
    se a ^ ";"

  | EXE_loop (p,a) -> spc ^
    "loop " ^
    string_of_id p ^ " " ^
    se a ^ ";"

  | EXE_svc v -> spc ^
    "_svc " ^ string_of_id v

  | EXE_fun_return x -> spc ^
    "return " ^ se x ^ ";"

  | EXE_yield x -> spc ^
    "yield " ^ se x ^ ";"

  | EXE_proc_return -> spc ^
    "return;"

  | EXE_halt s -> spc ^
    "halt "^string_of_string s^";"

  | EXE_trace (v,s) -> spc ^
    "trace " ^ string_of_id v ^ "=" ^ string_of_string s ^ ";"


  | EXE_nop s -> spc ^
    "/*" ^ s ^ "*/"

  | EXE_code s -> spc ^
    "code " ^ string_of_code_spec s

  | EXE_noreturn_code s -> spc ^
    "noreturn_code " ^ string_of_code_spec s

  | EXE_init (l,r) -> spc ^
    string_of_id l ^ " := " ^ se r ^ ";"

  | EXE_iinit ((l,i),r) -> spc ^
    string_of_id l ^ "<" ^ string_of_bid i ^ "> := " ^ se r ^ ";"

  | EXE_assign (l,r) -> spc ^
    se l ^ " = " ^ se r ^ ";"

and sbe bsym_table e =
  string_of_bound_expression bsym_table e

and tsbe bsym_table e =
  string_of_bound_expression_with_type bsym_table e

and string_of_bound_expression_with_type bsym_table ((e',t) as e) =
  string_of_bound_expression'
    bsym_table
    (tsbe bsym_table)
    e ^
    ":" ^
    sbt bsym_table t

and string_of_bound_expression bsym_table e =
  string_of_bound_expression' bsym_table (sbe bsym_table) e

and string_of_bound_expression' bsym_table se e =
  (*
  let sid n = qualified_name_of_bindex bsym_table n in
  let sid n = fst (get_name_parent bsym_table n) in
  *)
  let sid n = bound_name_of_bindex bsym_table n in
  match fst e with

  | BEXPR_tuple_head e -> "tuple_head ("^ se e ^")"
  | BEXPR_tuple_tail e -> "tuple_tail("^ se e ^")"
  | BEXPR_tuple_cons (eh,et) -> "tuple_cons("^ se eh ^"," ^ se et ^")"
  | BEXPR_get_n (n,e') -> "/*proj*/"^ se n ^ "(" ^ se e' ^ ")"

  | BEXPR_not e -> "not("^ se e ^ ")"
  | BEXPR_deref e -> "*("^ se e ^ ")"
  | BEXPR_name (i,ts) -> sid i ^ string_of_inst bsym_table ts
  | BEXPR_closure (i,ts) -> sid i ^ string_of_inst bsym_table ts
  | BEXPR_ref (i,ts) -> "&" ^ sid i ^ string_of_inst bsym_table ts
  | BEXPR_new e -> "new " ^ se e
  | BEXPR_class_new (t,e) -> "new " ^ sbt bsym_table t ^ "(" ^ se e ^ ")"
  | BEXPR_address e -> "&" ^ se e
  | BEXPR_likely e -> "likely(" ^ se e ^")"
  | BEXPR_unlikely e -> "unlikely(" ^ se e ^")"

  | BEXPR_literal e -> string_of_literal e
  | BEXPR_apply  (fn, arg) -> "(" ^
    se fn ^ " " ^
    se arg ^
    ")"

  | BEXPR_compose (fn1, fn2) -> "(" ^
    se fn1 ^ " . " ^
    se fn2 ^
    ")"

  | BEXPR_apply_prim (i,ts, arg) -> "(" ^
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_apply_direct  (i,ts, arg) -> "(" ^
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_apply_struct (i,ts, arg) -> "(" ^
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_apply_stack (i,ts, arg) -> "(" ^
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se arg ^
    ")"

  | BEXPR_tuple t -> "(" ^ catmap ", " se t ^ ")"

  | BEXPR_record ts -> "struct { " ^
      catmap "" (fun (s,e)-> s^":"^ se e ^"; ") ts ^ "}"

  | BEXPR_variant (s,e) -> "case " ^ s ^ " of (" ^ se e ^ ")"

  | BEXPR_case (v,t) ->
    "(case " ^ si v ^ " of " ^ string_of_btypecode (Some bsym_table) t ^ ")"

  | BEXPR_match_case (v,e) ->
    "(match case " ^ si v ^ ")(" ^ se e ^ ")"

  | BEXPR_case_arg (v,e) ->
    "(arg of case " ^ si v ^ " of " ^ se e ^ ")"

  | BEXPR_case_index e ->
    "caseno (" ^ se e ^ ")"

  | BEXPR_expr (s,t) ->
    "code ["^string_of_btypecode (Some bsym_table) t^"]" ^ "'" ^ s ^ "'"

  | BEXPR_range_check (e1,e2,e3) ->
    "range_check(" ^ se e1 ^"<=" ^ se e2 ^"<" ^se e3 ^ ")"

  | BEXPR_coerce (e,t) -> se e ^ " : " ^ string_of_btypecode (Some bsym_table) t

and string_of_biface bsym_table level s =
  let spc = spaces level in
  let se e = string_of_bound_expression bsym_table e in
  let sid n = qualified_name_of_bindex bsym_table n in
  match s with
  | BIFACE_export_fun (_,index,cpp_name) ->
    spc ^ "export fun " ^ qualified_name_of_bindex bsym_table index ^
    " as \"" ^ cpp_name ^ "\";"

  | BIFACE_export_cfun (_,index,cpp_name) ->
    spc ^ "export cfun " ^ qualified_name_of_bindex bsym_table index ^
    " as \"" ^ cpp_name ^ "\";"

  | BIFACE_export_python_fun (_,index,cpp_name) ->
    spc ^ "export python fun " ^ qualified_name_of_bindex bsym_table index ^
    " as \"" ^ cpp_name ^ "\";"

  | BIFACE_export_type (_,btyp,cpp_name) ->
    spc ^ "export type (" ^ string_of_btypecode (Some bsym_table) btyp ^
    ") as \"" ^ cpp_name ^ "\";"

and sbx bsym_table s =  string_of_bexe bsym_table 0 s

and string_of_bexe bsym_table level s =
  let spc = spaces level in
  let se e = string_of_bound_expression bsym_table e in
  let sid n = bound_name_of_bindex bsym_table n in
  match s with
  | BEXE_goto (_,s) -> spc ^ "goto " ^ s ^ ";"

  | BEXE_assert (_,e) -> spc ^ "assert " ^ se e ^ ";"
  | BEXE_axiom_check2 (_,_,e1,e2) -> spc ^ "axiom_check2 " ^
    (match e1 with Some e1 -> se e1 ^ " implies " | None -> "") ^
    se e2^";"
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
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se a ^ ";"

  | BEXE_jump_direct (_,i,ts,a) -> spc ^
    "direct tail call " ^
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se a ^ ";"

  | BEXE_call_stack (_,i,ts,a) -> spc ^
    "stackcall " ^
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se a ^ ";"

  | BEXE_call_prim (_,i,ts,a) -> spc ^
    "primcall " ^
    sid i ^ string_of_inst bsym_table ts ^ " " ^
    se a ^ ";"

  | BEXE_jump (_,p,a) -> spc ^
    "tail call " ^
    se p ^ " " ^
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
  | BEXE_try _ -> "try {"
  | BEXE_endtry _ -> "}"
  | BEXE_catch (_,id,t) -> "} catch ("^ sbt bsym_table t ^" &"^id^") {"

and string_of_dcl level name seq vs (s:dcl_t) =
  let se e = string_of_expr e in
  let st t = string_of_typecode t in
  let sl = spaces level in
  let seq = match seq with Some i -> "<" ^ string_of_bid i ^ ">" | None -> "" in
  match s with
  | DCL_type_alias (t2) ->
    sl ^ "typedef " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    " = " ^ st t2 ^ ";"

  | DCL_inherit qn ->
    sl ^ "inherit " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    " = " ^ string_of_qualified_name qn ^ ";"

  | DCL_inherit_fun qn ->
    sl ^ "inherit fun " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    " = " ^ string_of_qualified_name qn ^ ";"

  | DCL_module (asms) ->
    sl ^ "module " ^ string_of_id name ^ seq ^ string_of_vs vs ^ " = " ^
    "\n" ^
    string_of_asm_compound level asms

  | DCL_root (asms) ->
    sl ^ "root" ^ " = " ^
    "\n" ^
    string_of_asm_compound level asms

  | DCL_instance (name,asms) ->
    sl ^ "instance " ^ string_of_vs vs ^ " " ^
    string_of_qualified_name name ^seq ^ " = " ^
    "\n" ^
    string_of_asm_compound level asms

  | DCL_struct (cs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ string_of_id name ^ ": " ^ st ty ^ ";"
    in
    sl ^ "struct " ^ string_of_id name ^ seq ^ string_of_vs vs ^ " = " ^
    sl ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    sl ^ "}"

  | DCL_cstruct (cs, reqs) ->
    let string_of_struct_component (name,ty) =
      (spaces (level+1)) ^ string_of_id name ^ ": " ^ st ty ^ ";"
    in
    sl ^ "cstruct " ^ string_of_id name ^ seq ^ string_of_vs vs ^ " = " ^
    sl ^ "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    sl ^ "} " ^ string_of_named_reqs reqs ^ ";"

  | DCL_typeclass (asms) ->
    sl ^ "type class " ^ string_of_id name ^ seq ^ string_of_vs vs ^ " =\n" ^
    string_of_asm_compound level asms

  | DCL_union (cs) ->
    let string_of_union_component (name,v,vs,ty) =
      (spaces (level+1)) ^
      "|" ^ string_of_id name ^
      (match v with | None -> "" | Some i -> "="^si i) ^
      special_string_of_typecode ty
    in
    sl ^ "union " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    " = " ^
    sl ^ "{\n" ^
    catmap ";\n" string_of_union_component cs ^ "\n" ^
    sl ^ "}"

  | DCL_newtype (nt)-> sl ^
    "type " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    " = new " ^ st nt ^ ";"

  | DCL_abs (quals, code, reqs) -> sl ^
    (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
    "type " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    " = " ^ string_of_code_spec code ^
    string_of_named_reqs reqs ^
    ";"

  | DCL_fun (props, args, result, code, reqs,prec) ->
    let argtype:typecode_t = type_of_argtypes args in
    let t:typecode_t = TYP_function (argtype,result) in
    sl ^
    string_of_properties props ^
    "fun " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    ": " ^ st t ^
    " = " ^ string_of_code_spec code ^
    (if prec = "" then "" else ":"^prec^" ")^
    string_of_named_reqs reqs ^
    ";"

  | DCL_callback (props, args, result, reqs) ->
    let argtype:typecode_t = type_of_argtypes args in
    let t:typecode_t = TYP_cfunction (argtype,result) in
    sl ^
    string_of_properties props ^
    "callback fun " ^ string_of_id name ^ seq ^ string_of_vs vs ^
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
    string_of_id name ^ seq ^  string_of_vs vs ^
    " = "^ string_of_code_spec s ^
    string_of_named_reqs reqs ^ ";"

  | DCL_const (props,typ, code, reqs) ->
    sl ^
    string_of_properties props ^
    "const " ^ string_of_id name ^ seq ^string_of_vs vs ^
    ": " ^ st typ ^
    " = "^string_of_code_spec code^
    string_of_named_reqs reqs ^
    ";"

  | DCL_reduce (ps, e1,e2) ->
    sl ^
    "reduce " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    "("^ string_of_basic_parameters ps ^"): " ^
    string_of_expr e1 ^ " => " ^ string_of_expr e2 ^ ";"

  | DCL_axiom (ps, e1) ->
    sl ^
    "axiom " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    "("^ string_of_parameters ps ^"): " ^
    string_of_axiom_method e1 ^ ";"

  | DCL_lemma (ps, e1) ->
    sl ^
    "lemma " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    "("^ string_of_parameters ps ^"): " ^
    string_of_axiom_method e1 ^ ";"

  | DCL_function (ps, res, props, ss) ->
    sl ^
    string_of_properties props ^
    "fun " ^ string_of_id name ^ seq ^ string_of_vs vs ^
    "("^ (string_of_parameters ps)^"): "^(st res)^"\n" ^
    string_of_asm_compound level ss


  | DCL_match_check (pat,(s,i)) ->
    sl ^
    "function " ^ string_of_id name ^ seq ^ "() { " ^
    s ^ "<" ^ string_of_bid i ^ "> matches " ^ string_of_pattern pat ^
    " }"

  | DCL_match_handler (pat,(varname, i), sts) ->
    sl ^
    "match_handler " ^ string_of_id name ^ seq ^
    "(" ^ string_of_pattern pat ^ ")" ^
    string_of_asm_compound level sts

  | DCL_value (ty, kind) ->
    let make_suffix () =
      string_of_id name ^ seq ^ string_of_vs vs ^ ": " ^ st ty ^ ";"
    in
    sl ^
    begin match kind with
    | `Val -> "val " ^ make_suffix ()
    | `Var -> "var " ^ make_suffix ()
    | `Ref -> "ref " ^ make_suffix ()
    | `Lazy e ->
        "fun " ^ string_of_id name ^ seq ^ string_of_vs vs ^ ": " ^ st ty ^ " = " ^ se e ^
        ";"
    end

and string_of_access = function
  | `Private -> "private "
  | `Public -> "public"

and string_of_asm level s =
  match s with
  | Dcl (sr,name,seq,access,vs, d) ->
    (match access with
    | `Private -> "private "
    | `Public -> ""
    ) ^
    string_of_dcl level name seq vs d
  | Exe (sr,s) -> string_of_exe level s
  | Iface (sr,s) -> string_of_iface level s
  | Dir (sr,s) -> string_of_dir level s

and string_of_dir level s =
  let sqn n = string_of_qualified_name n in
  match s with
  | DIR_open (vs,qn) ->
    spaces level ^ "open " ^ string_of_ivs vs ^ sqn qn ^ ";"

  | DIR_use (n,qn) ->
    spaces level ^ "use " ^ string_of_id n ^ " = " ^ sqn qn ^ ";"

  | DIR_inject_module (vs,qn) ->
    spaces level ^ "inherit " ^ string_of_ivs vs ^ sqn qn ^ ";"

and string_of_breq bsym_table (i,ts) =
  "rq<" ^ string_of_bid i ^ ">" ^ string_of_inst bsym_table ts
and string_of_breqs bsym_table reqs = catmap ", " (string_of_breq bsym_table) reqs
and string_of_production p = catmap " " string_of_glr_entry p
and string_of_reduced_production p = catmap " " string_of_reduced_glr_entry p
and string_of_bproduction bsym_table p =
  catmap " " (string_of_bglr_entry bsym_table) p

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

and string_of_bglr_entry sym_table (name,symbol) =
  (match name with
  | Some n -> n ^ ":"
  | None -> ""
  )^
  (match symbol with
  | `Nonterm (i::_)
  | `Term i -> qualified_name_of_index sym_table i
  | `Nonterm [] -> "<Undefined nonterminal>"
  )

and string_of_bbdcl bsym_table bbdcl index : string =
  let name = qualified_name_of_bindex bsym_table index in
  let sobt t = string_of_btypecode (Some bsym_table) t in
  let se e = string_of_bound_expression bsym_table e in
  let un = btyp_tuple [] in
  match bbdcl with
  | BBDCL_invalid -> assert false

  | BBDCL_module ->
    "module " ^ name ^ " {}"

  | BBDCL_fun (props,vs,ps,res,es) ->
    let is_proc = Flx_btype.is_void res in
    string_of_properties props ^
    (if is_proc then "proc " else "fun ") ^
    name ^ string_of_bvs vs ^
    "(" ^ (string_of_bparameters bsym_table ps) ^ ")" ^
    (if is_proc then "" else ": " ^ sobt res) ^
    "{\n" ^
    cat "\n" (map (string_of_bexe bsym_table 1) es) ^
    "}"

  | BBDCL_val (vs,ty,kind) ->
    begin match kind with
    | `Val -> "val "
    | `Var -> "var "
    | `Ref -> "ref "
    | `Tmp -> "<tmp> "
    end ^ name ^ string_of_bvs vs ^ ": " ^ sobt ty ^ ";"

  (* binding structures [prolog] *)
  | BBDCL_newtype (vs,t) ->
    "type " ^ name ^  string_of_bvs vs ^
    " = new " ^ sobt t ^ ";"

  | BBDCL_external_type (vs,quals,code,reqs) ->
    (match quals with [] ->"" | _ -> string_of_bquals bsym_table quals ^ " ") ^
    "type " ^ name ^  string_of_bvs vs ^
    " = " ^ string_of_code_spec code ^ ";"

  | BBDCL_external_const (props, vs,ty,code,reqs) ->
    string_of_properties props ^
     "const " ^ name ^ string_of_bvs vs ^
     ": " ^ sobt ty ^
     " = " ^ string_of_code_spec code ^
     string_of_breqs bsym_table reqs ^
     ";"

  | BBDCL_external_fun (props,vs,ps,rt,reqs,prec,kind) ->
    let is_proc = Flx_btype.is_void rt in
    string_of_properties props ^
    (match kind with | `Callback _ -> "callback " | _ -> "") ^
    (if is_proc then "proc " else "fun ") ^
    name ^ string_of_bvs vs ^
    ": " ^
    (sobt (btyp_tuple ps)) ^
    (if is_proc then " " else " -> " ^ sobt rt) ^
    begin match kind with
    | `Code code -> " = " ^ string_of_code_spec code
    | _ -> ""
    end ^
    (if prec = "" then "" else ":" ^ prec ^ " ") ^
    string_of_breqs bsym_table reqs ^
    ";"

  | BBDCL_external_code (vs,s,ikind,reqs) ->
     (match ikind with
     | `Header -> "header "
     | `Body -> "body "
     | `Package -> "package "
     ) ^
    name^  string_of_bvs vs ^
    " "^ string_of_code_spec s ^
    string_of_breqs bsym_table reqs

  | BBDCL_union (vs,cs) ->
    let string_of_union_component (name,v,ty) =
      "  " ^ "| " ^ string_of_id name ^
     "="^si v^
      special_string_of_btypecode bsym_table ty
    in
    "union " ^ name ^ string_of_bvs vs ^ " = " ^
    "{\n" ^
    catmap ";\n" string_of_union_component cs ^ "\n" ^
    "}"

  | BBDCL_struct (vs,cs) ->
    let string_of_struct_component (name,ty) =
      "  " ^ string_of_id name ^ ": " ^ sobt ty ^ ";"
    in
    "struct " ^ name ^ string_of_bvs vs ^ " = " ^
    "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    "}"

  | BBDCL_cstruct (vs,cs, reqs) ->
    let string_of_struct_component (name,ty) =
      "  " ^ string_of_id name ^ ": " ^ sobt ty ^ ";"
    in
    "cstruct " ^ name ^ string_of_bvs vs ^ " = " ^
    "{\n" ^
    catmap "\n" string_of_struct_component cs ^ "\n" ^
    "} " ^
    string_of_breqs bsym_table reqs ^ ";"

  | BBDCL_typeclass (props,vs) ->
    string_of_properties props ^
    "typeclass " ^ name ^ string_of_bvs vs ^ ";"

  | BBDCL_instance (props,vs,cons,bid,ts) ->
    string_of_properties props ^
    "instance "^string_of_bvs_cons bsym_table vs cons^
    " of <" ^ string_of_bid bid ^">["^ catmap "," (sbt bsym_table) ts ^ "];"

  | BBDCL_const_ctor (vs,uidx,ut,ctor_idx, evs, etraint) ->
    "uctor<" ^ name ^ ">" ^ string_of_bvs vs ^
    " : " ^ sobt ut ^
    ";"

  | BBDCL_nonconst_ctor (vs,uidx,ut,ctor_idx, ctor_argt, evs, etraint) ->
    "uctor<" ^ name ^ ">" ^ string_of_bvs vs ^
    " : " ^ sobt ut ^
    " of " ^ sobt ctor_argt ^
    ";"

  | BBDCL_axiom -> "axiom ...;"
  | BBDCL_lemma -> "lemma ...;"
  | BBDCL_reduce -> "reduce ...;"


let full_string_of_entry_kind sym_table bsym_table {base_sym=i; spec_vs=vs; sub_ts=ts} =
  let sym = 
      try Flx_sym_table.find sym_table i 
      with Not_found -> failwith ("full_string_of_entry_kind: Help, can't find index " ^ string_of_int
      i ^ " in sym table")
  in
  string_of_symdef sym.Flx_sym.symdef sym.Flx_sym.id sym.Flx_sym.vs ^
  "\n  defined at " ^ Flx_srcref.short_string_of_src sym.Flx_sym.sr ^ "\n  with view" ^
  " vs=" ^ catmap "," (fun (s,i)->s^"<"^si i^">") vs ^
  " ts=" ^ catmap "," (sbt bsym_table) ts


let string_of_entry_kind {base_sym=i} = string_of_bid i

let string_of_entry_set = function
  | NonFunctionEntry x -> string_of_entry_kind x
  | FunctionEntry ls ->
    "{" ^
      catmap "," string_of_entry_kind ls ^
    "}"

let full_string_of_entry_set sym_table bsym_table = function
  | NonFunctionEntry x -> full_string_of_entry_kind sym_table bsym_table x
  | FunctionEntry ls -> if length ls = 0 then "{}" else
    "{\n" ^
      catmap "\n" (full_string_of_entry_kind sym_table bsym_table) ls ^
    "\n}"

let string_of_myentry bsym_table {base_sym=i; spec_vs=vs; sub_ts=ts} =
 string_of_bid i ^
 " vs=" ^ catmap "," (fun (s,_)->s) vs ^
 " ts=" ^ catmap "," (sbt bsym_table) ts

let print_name_table bsym_table table =
  Hashtbl.iter
  (fun s v ->
    print_endline (s ^ " --> " ^
      match v with
      | NonFunctionEntry i -> string_of_myentry bsym_table i
      | FunctionEntry ii ->
          "{"^ catmap "," (string_of_myentry bsym_table) ii ^ "}"
    );
  )
  table

let string_of_varlist bsym_table varlist =
  catmap ", " (fun (i,t)-> si i ^ "->" ^ sbt bsym_table t) varlist

let print_env e =
  let print_entry k v =
    Printf.printf "  %s : %s\n" k (string_of_entry_set v)
  in
  let print_table htab =
    print_endline "--";
    Hashtbl.iter print_entry htab
  in
  let print_level (index,id,htab,htabs,con) =
    Printf.printf "%s<%s>\n" id (string_of_bid index);
    print_table htab;
    print_endline "OPENS:";
    List.iter print_table htabs;
    print_endline "ENDOFOPENS";
    print_endline ("CONSTRAINT: " ^ string_of_typecode con)
  in

  List.iter print_level e

let print_env_long sym_table bsym_table e =
  let print_entry k v =
    print_endline ("EntrySet for " ^ k ^ ":"); print_endline (full_string_of_entry_set sym_table bsym_table v)
  in
  let print_table htab =
    print_endline "--";
    Hashtbl.iter print_entry htab
  in
  let print_level (index,id,htab,htabs,con) =
    Printf.printf "%s<%s>\n" id (string_of_bid index);
    print_table htab;
    print_endline "OPENS:";
    List.iter print_table htabs;
    print_endline "ENDOFOPENS";
    print_endline ("CONSTRAINT: " ^ string_of_typecode con)
  in

  List.iter print_level e


let print_env_short e =
  let print_level (index,id,htab,htabs,con) =
    Printf.printf "%s<%s>\n" id (string_of_bid index);
  in
  List.iter print_level e

let print_function_body bsym_table id i (bvs:bvs_t) ps exes =
  print_endline "";
  print_endline ("BODY OF " ^ string_of_id id ^ "<" ^ string_of_bid i ^ "> [" ^
  catmap "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") bvs ^
  "] (" ^ string_of_bparameters bsym_table ps ^ ")"
  );
  iter
  (fun exe -> print_endline (string_of_bexe bsym_table 1 exe))
  exes

let print_function bsym_table i =
  let bsym = Flx_bsym_table.find bsym_table i in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (_,bvs,ps,_,exes) ->
      print_function_body 
        bsym_table
        (Flx_bsym.id bsym)
        i
        bvs
        ps
        exes
  | _ -> ()

let print_functions bsym_table =
  Flx_bsym_table.iter begin fun i _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (_,bvs,ps,_,exes) ->
        print_function_body
          bsym_table
          (Flx_bsym.id bsym)
          i
          bvs
          ps
          exes
    | _ -> ()
  end bsym_table

let print_symbols bsym_table =
  Flx_bsym_table.iter begin fun i _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (_,bvs,ps,_,exes) ->
        print_function_body
          bsym_table
          (Flx_bsym.id bsym)
          i
          bvs
          ps
          exes
    | BBDCL_val (bvs,t,kind) ->
        let kind =
          match kind with
          | `Val -> "VALUE"
          | `Var -> "VARIABLE"
          | `Ref -> "REFERENCE"
          | `Tmp -> "TEMPORARY"
        in
        Printf.printf "%s %s <%s> [%s] type %s\n"
          kind
          (string_of_id (Flx_bsym.id bsym))
          (string_of_bid i)
          (catmap "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") bvs)
          (sbt bsym_table t)
    | _ -> ()
  end bsym_table

let string_of_name_map name_map =
  let s =
    Hashtbl.fold begin fun k v s ->
      k ^ "=" ^ (string_of_entry_set v) ^ ", " ^ s
    end name_map ""
  in
  "{" ^ s ^ "}"


let print_sym sym_table bid =
  let parent, sym = Flx_sym_table.find_with_parent sym_table bid in

  print_endline ("index: " ^ string_of_bid bid);
  print_endline ("id: " ^ string_of_id sym.Flx_sym.id);
  print_endline ("parent: " ^ 
    match parent with
    | Some parent -> string_of_bid parent
    | None -> "");

  if Hashtbl.length sym.Flx_sym.pubmap != 0 then
    print_endline ("pubmap: " ^
      (string_of_name_map sym.Flx_sym.pubmap));

  if Hashtbl.length sym.Flx_sym.privmap != 0 then
    print_endline ("privmap: " ^
      (string_of_name_map sym.Flx_sym.privmap));

  print_endline ("symdef: " ^ (string_of_symdef
    sym.Flx_sym.symdef
    sym.Flx_sym.id
    sym.Flx_sym.vs))


let print_sym_table sym_table =
  let syms = Flx_sym_table.fold (fun k _ v acc -> (k,v) :: acc) sym_table [] in
  let syms = List.sort (fun (k1,_) (k2,_) -> compare k1 k2) syms in

  List.iter (fun (bid, _) -> print_sym sym_table bid) syms


let string_of_bsym bsym_table bid =
  let bsym = Flx_bsym_table.find bsym_table bid in

  string_of_bid bid ^ " --> " ^
  string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) bid


let print_bsym bsym_table bid =
  let parent, bsym = Flx_bsym_table.find_with_parent bsym_table bid in

  print_endline ("index: " ^ string_of_bid bid);
  print_endline ("id: " ^ string_of_id (Flx_bsym.id bsym));
  print_endline ("parent: " ^ 
    match parent with
    | Some parent -> string_of_bid parent
    | None -> "");

  print_endline ("bbdcl: " ^ (string_of_bbdcl
    bsym_table 
    (Flx_bsym.bbdcl bsym)
    bid))


let print_bsym_table bsym_table =
  let bsyms = Flx_bsym_table.fold
    (fun k _ v acc -> (k,v) :: acc)
    bsym_table
    []
  in
  let bsyms = List.sort (fun (k1,_) (k2,_) -> compare k1 k2) bsyms in

  List.iter (fun (bid, _) -> print_bsym bsym_table bid) bsyms
