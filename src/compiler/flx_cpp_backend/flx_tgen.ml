open Flx_util
open Flx_list
open Flx_types
open Flx_btype
open Flx_bbdcl
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_unify
open Flx_name
open Flx_cexpr
open Flx_csubst
open Flx_exceptions
open List
open Flx_ctype
open Flx_maps
open Flx_btype_subst

module CS = Flx_code_spec

let debug = false
(*
 * Now some code to generate the bases, given the hashtable. We also mangle
 * c++ abstract type names.
 *)

let gen_tuple name tn typs =
  let n = length typs in
  "struct " ^ name ^ " {\n" ^
  catmap ""
  (fun (t,i) ->
    if t = btyp_tuple []
    then "  // elided mem_" ^ si i ^ "(type unit)\n"
    else "  "^tn t^ " mem_" ^ si i ^ ";\n"
  )
  (combine typs (nlist n))
  ^
  "  " ^ name ^ "(){}\n" (* default constructor *)
  ^
  (
    if fold_left (fun r t -> r && t = btyp_tuple []) true typs
    then ""
    else
    "  " ^ name ^ "(" ^
    fold_left
    (fun s (t,i) ->
      if t = btyp_tuple [] then s
      else
        s ^
        (if String.length s > 0 then ", " else "") ^
        tn t^" a" ^ si i
    )
    ""
    (combine typs (nlist n))
    ^
    "):\n    "
    ^
    fold_left
    (fun s (t,i) ->
      if t = btyp_tuple [] then s
      else
        s ^
        (if String.length s > 0 then ", " else "") ^
        "mem_"^si i ^ "(a" ^ si i^")"
    )
    ""
    (combine typs (nlist n))
    ^
    "{}\n"
  )
  ^
  "};\n"

let gen_record tname tn typs =
  let n = length typs in
    (* keep track of duplicates with magic names *)
  let dups = Hashtbl.create n in
  let name s = 
    if Hashtbl.mem dups s then
      let count = Hashtbl.find dups s in
      Hashtbl.replace dups s (count+1);
      "_" ^ s ^ "_" ^ string_of_int count
    else begin
      Hashtbl.add dups s 1;
      if s = "" then "_blank_" else s
    end
  in
  let typs = List.map (fun (n,t) -> Flx_name.cid_of_flxid (name n),t) typs in
  "// Record\n" ^
  "struct " ^ tname ^ " {\n" ^
  catmap ""
  (fun (n,t) ->
    if t = btyp_tuple []
    then "  // elided " ^ n ^ "(type unit)\n"
    else "  "^tn t^ " " ^ n ^ ";\n"
  )
  typs
  ^
  "  " ^ tname ^ "(){}\n" (* default constructor *)
  ^
  (
    if fold_left (fun r (n,t) -> r && t = btyp_tuple []) true typs
    then ""
    else
    "  " ^ tname ^ "(" ^
    (
      fold_left
      (fun s (n,t) ->
        if t = btyp_tuple [] then s
        else
          s ^
          (if String.length s > 0 then ", " else "") ^
          tn t^" _" ^ n ^ "_a"
      )
    )
    ""
    typs
    ^
    "):\n    "
    ^
    ( 
    fold_left
      (fun s (n,t) ->
        if t = btyp_tuple [] then s
        else
          s ^
          (if String.length s > 0 then ", " else "") ^
          n ^ "(_" ^ n ^"_a)"
      )
      ""
      typs
    )
    ^
    "{}\n"
  )
  ^
  "};\n"

(* copy ctor, assignment, and destructor are generated;
  we have to supply the pointer constructor and default
  constructor though. Note that it matters not if this
  type is sliced, since it's nothing more than a type
  correct wrapper for its base
*)


(* this routine generates a typedef (for primitives)
or struct declaration which names the type.
*)

let rec gen_type_name syms bsym_table (index,typ) =
  if debug then
  print_endline (
    "Flx_tgen.GENERATING TYPE NAME " ^
    string_of_bid index ^ ": " ^
    Flx_btype.st typ
  );
  let cn t = cpp_type_classname syms bsym_table t in
  let tn t = cpp_typename syms bsym_table t in
  let sn t = cpp_structure_name syms bsym_table t in
  let descr =
    "\n//TYPE " ^ string_of_bid index ^ ": " ^ sbt bsym_table typ ^ "\n" ^
    "// typedef " ^ sn  typ ^ " " ^ cn typ ^ ";\n"
  in

  let t = unfold "flx_tgen: gen_type_name" typ in
  match t with
  | t when Flx_btype.islinear_type bsym_table t -> descr 
      (* "typedef int " ^ tn typ ^ ";\n" *)

  | BTYP_fix (i,_) -> ""
  | BTYP_type_var (i,mt) -> failwith "[gen_type_name] Can't gen name of type variable"

  | BTYP_cltpointer _ 
  | BTYP_cltwref _
  | BTYP_cltrref _
  | BTYP_pointer _ -> ""
    (* NEW *)
    (*
    descr ^
    "typedef " ^ tn b ^ " *"^ tn t ^ ";\n"
    *)

  | BTYP_tuple _
  | BTYP_record _
  | BTYP_array _ ->
    descr ^
    let name = cn typ in
    "struct " ^ name ^ ";\n"

  | BTYP_function _ ->
    descr ^
    let name = cn typ in
    "struct " ^ name ^ ";\n"

  | BTYP_cfunction (d,c) ->
    descr ^
    let name = cn typ in
    let ds = match d with
      | BTYP_tuple ls -> ls
      | BTYP_array (t,n) -> (* not sure if this is enough or even right .. *) 
        begin match n with
        | BTYP_unitsum n ->
          let rec aux ls n = if n = 0 then ls else aux (t::ls) (n-1) in 
          aux [] n
        | _ -> failwith "flx_tgen unexpected array indexed by non-unit sum"
        end
      | x -> [x]
    in
    let ctn t = `Ct_base (cpp_typename syms bsym_table t) in
    let t = `Ct_fun (ctn c,map ctn ds) in
    let cdt = `Cdt_value t in
    "typedef " ^ string_of_cdecl_type name cdt ^ ";\n"

  | BTYP_rptsum _ 
  | BTYP_sum _ 
  | BTYP_variant _ -> ""
    (*
    descr ^
    begin match Flx_vrep.cal_variant_rep bsym_table t with
    | Flx_vrep.VR_self -> "// VR_self\n"
    | Flx_vrep.VR_int -> "typedef int " ^ tn typ ^ "; // VR_int\n"
    | Flx_vrep.VR_packed -> "typedef void *" ^ tn typ ^"; // VR_packed \n"
    | Flx_vrep.VR_uctor -> "typedef ::flx::rtl::_uctor_ " ^ tn typ ^ "; //VR_uctor\n"
    end
    *)


  | BTYP_inst (i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i
      with _ -> failwith ("[gen_type_name] can't find type" ^ string_of_bid i)
    in
    let sr = Flx_bsym.sr bsym in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_external_type (vs,quals,ct,_) ->
      let complete = not (mem `Incomplete quals) in
      let descr =
        "\n//"^(if complete then "" else "INCOMPLETE ")^
        "PRIMITIVE " ^ string_of_bid i ^" INSTANCE " ^
        string_of_bid index ^ ": " ^
        sbt bsym_table typ ^
        "\n"
      in
      let instance_name = cn typ in
      let tss = map tn ts in
      let instance =
        match ct with
        | CS.Virtual -> clierrx "[flx_cpp_backend/flx_tgen.ml:239: E312] " (Flx_bsym.sr bsym) "Instantiate virtual type!"
        | CS.Identity -> syserr (Flx_bsym.sr bsym) "Idendity type is nonsense!"
        | CS.Str c -> c
        | CS.Str_template c ->
        try sc "expr" (csubst (ref Flx_set.StringSet.empty) (Flx_bsym.sr bsym) (Flx_bsym.sr bsym) c 
           ~arg:(fun () -> Flx_cexpr.ce_atom "Error") 
           ~args:[] ~typs:[] ~argtyp:"Error" ~retyp:"Error" 
           ~gargs:tss 
           ~prec:"atom" ~argshape:"Error" ~argshapes:["Error"] ~display:["Error"] ~gargshapes:["Error"]
           ~name:(Flx_bsym.id bsym)
          )
        with Not_found -> failwith "[gen_type_name] Unexpected error in csubst"
      in

      (* special hack to avoid 'typedef int int' when we decide
      to use the native typename in generated code instead of
      an alias
      *)
      (if instance = instance_name
      then descr ^ "//"
      else descr
      )
      ^
      "typedef " ^ instance ^ " " ^ instance_name ^ ";\n"

    | BBDCL_newtype (_,t') -> ""

    | BBDCL_cstruct _ -> if ts = [] then "" else
      let descr =
        "\n//CSTRUCT " ^ string_of_bid i ^ " INSTANCE " ^
        string_of_bid index ^ ": " ^
        sbt bsym_table typ ^
        "\n"
      in
      let instance_name = cn typ in
      let instance = Flx_bsym.id bsym ^ "<" ^ catmap "," cn ts ^"> " in
      descr ^
      "typedef " ^ instance ^ " " ^ instance_name ^ ";\n"

    | BBDCL_struct _ ->
      let descr =
        "\n//STRUCT " ^ string_of_bid i ^ " INSTANCE " ^
        string_of_bid index ^ ": " ^
        sbt bsym_table typ ^
        "\n"
      in
      let name = cn typ in
      descr ^ "struct " ^ name ^ ";\n"

(*
    | BBDCL_union (vs,[id,_,[],t',_]) -> 
print_endline ("[flx_tgen] One component union should have been removed");
      let t'' = tsubst sr vs ts t' in
      gen_type_name syms bsym_table (index,t'')
*)
    | BBDCL_union (vs,ls) -> ""
(*
      let descr =
        "\n//UNION " ^ string_of_bid i ^ " INSTANCE " ^
        string_of_bid index ^ ": " ^
        sbt bsym_table typ ^
        "\n"
      in
      let name = cn typ in
      descr ^
      begin match Flx_vrep.cal_variant_rep bsym_table t with
      | Flx_vrep.VR_self -> "// VR_self\n"
      | Flx_vrep.VR_int -> "typedef int " ^ tn typ ^ "; // VR_int\n"
      | Flx_vrep.VR_packed -> "typedef void *" ^ tn typ ^"; // VR_packed \n"
      | Flx_vrep.VR_uctor -> "typedef ::flx::rtl::_uctor_ " ^ tn typ ^ "; //VR_uctor\n"
      end
*)
      (*
      let lss = map (fun (_,_,t)->t) ls in
      let lss = map (tsubst vs ts) lss in
      let len = si (length lss) in
      if all_voids lss
      then
        "typedef int " ^ name ^ "; //ncases="^len^"\n"
      else
        "typedef ::flx::rtl::_uctor_ " ^ name ^ "; //ncases="^len^"\n"
      *)

    | _ ->
      failwith
      (
        "[gen_type_name] Expected definition " ^ string_of_bid i ^
        " to be generic primitive, got " ^
        string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) i ^
        " instance types [" ^
        catmap ", " tn ts ^
        "]"
      )
    end

  | _ -> failwith ("[Flx_tgen] Unexpected metatype "^ sbt bsym_table t ^ " in gen_type_name")

let mk_listwise_ctor syms i name typ cts ctss =
  if length cts = 1 then
  let ctn,ctt = hd ctss in
    "  " ^ name ^ "("^ ctt ^ " const & _a): " ^
    ctn^"(_a){}\n"
  else ""


(* This routine generates complete types when needed *)
let rec gen_type syms bsym_table (index,typ) =
  if debug then
  print_endline (
    "Flx_tgen.GENERATING TYPE " ^
    string_of_bid index ^ ": " ^
    sbt bsym_table typ
  );
  let tn t = cpp_typename syms bsym_table t in
  let cn t = cpp_type_classname syms bsym_table t in
  let descr =
    "\n//TYPE " ^ string_of_bid index ^ ": " ^
    sbt bsym_table typ ^
    "\n"
  in
  let t = unfold "flx_tgen: gen_type" typ in
  match t with
  | _ when islinear_type bsym_table t -> ""
  | BTYP_type_var _ -> failwith "[gen_type] can't gen type variable"
  | BTYP_fix _ -> failwith "[gen_type] can't gen type fixpoint"

  (* PROCEDURE *)
  | BTYP_cfunction _ -> ""

  | BTYP_function (a,BTYP_fix (0,_))
  | BTYP_function (a,BTYP_void) ->
    descr ^
    let name = cn typ
    and argtype = tn a
    and unitproc = a = btyp_tuple [] || a = btyp_void ()
    in
    "struct " ^ name ^
    ": ::flx::rtl::con_t {\n" ^
    "  typedef void rettype;\n" ^
    "  typedef " ^ (if unitproc then "void" else argtype) ^ " argtype;\n" ^
    (if unitproc
    then
    "  virtual ::flx::rtl::con_t *call(::flx::rtl::con_t *)=0;\n"
    else
    "  virtual ::flx::rtl::con_t *call(::flx::rtl::con_t *, "^argtype^" const &)=0;\n"
    ) ^
    "  virtual "^name^" *clone()=0;\n"  ^
    "  virtual ::flx::rtl::con_t *resume()=0;\n"  ^
    "};\n"

  (* FUNCTION *)
  | BTYP_function (a,r) ->
    descr ^
    let name = cn typ
    and argtype = tn a
    and rettype = tn r
    and unitfun = a = btyp_tuple [] || a = btyp_void ()
    in
    "struct " ^ name ^ " {\n" ^
    "  typedef " ^ rettype ^ " rettype;\n" ^
    "  typedef " ^ (if unitfun then "void" else argtype) ^ " argtype;\n" ^
    "  virtual "^rettype^" apply("^
    (if unitfun then "" else argtype^" const &") ^
    ")=0;\n"  ^
    "  virtual "^name^" *clone()=0;\n"  ^
    "  virtual ~"^name^"(){};\n" ^
    "};\n"

  | BTYP_rptsum _
  | BTYP_sum _ -> "" (* union typedef *)
  | BTYP_variant _ -> ""

  | BTYP_tuple ts ->
     descr ^
     gen_tuple (tn typ) tn ts

  | BTYP_record (ts) ->
     descr ^
     gen_record (cn typ) tn ts

  | BTYP_cltpointer _
  | BTYP_cltrref _
  | BTYP_cltwref _
  | BTYP_pointer _ ->
    ""
    (*
    let name = tn typ in
    let t = tn t in
    descr ^ gen_ref name t
    *)

  | BTYP_array (t,i) ->
    let name = tn typ in
    let v = tn t in
    let n = 
      try Flx_btype.int_of_linear_type bsym_table i 
      with Invalid_int_of_unitsum ->
        failwith ("Invalid type for int of unit sum: " ^ sbt bsym_table t)
    in
    let requires_tuple_ctor = 
      try 
        Hashtbl.mem syms.array_as_tuple_registry 
        (Flx_treg.find_type_index syms bsym_table typ)
      with Not_found -> false 
    in
(*
    if n < 2 then failwith "[flx_tgen] unexpected array length < 2";
*)
    descr ^
    "struct " ^ name ^ " {\n" ^
    "  static ::std::size_t const len = " ^ si n ^ ";\n" ^
    "  typedef " ^ v ^ " element_type;\n" ^
    "  " ^ v ^ " data[" ^ si n ^ "];\n" ^
    "  " ^ name ^ "() {}\n" ^ (* default constructor *)
    (if requires_tuple_ctor then
    "  " ^ name ^ "(" ^
    List.fold_left begin fun s i ->
      if t = btyp_tuple [] then s else
      s ^
      (if String.length s > 0 then ", " else "") ^
      tn t ^ " a" ^ string_of_int i
    end "" (nlist n) ^
    ") {\n" ^
    List.fold_left begin fun s i ->
      if t = btyp_tuple [] then s else
      s ^ "    data[" ^ string_of_int i ^ "] = a" ^ string_of_int i ^ ";\n"
    end "" (nlist n) ^
    "  }\n" 
    else ""
    ) ^ 
    "};\n"


  | BTYP_inst (i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i
      with _ -> failwith ("[gen_type_name] can't find type" ^ string_of_bid i)
    in
    let sr = Flx_bsym.sr bsym in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_newtype (vs,t') ->
      let descr =
        "\n//NEWTYPE " ^ string_of_bid i ^ " INSTANCE " ^
        string_of_bid index ^ ": " ^
        sbt bsym_table typ ^
        "\n"
      in
      let instance_name = cn typ in
      let instance = cn t' in
      descr ^
      "typedef " ^ instance ^ " " ^ instance_name ^ ";\n"

    | BBDCL_external_type _ -> ""
    | BBDCL_cstruct _ -> ""

    | BBDCL_struct (vs,cts) ->
      let cts = map (fun (name,typ) -> name, tsubst sr vs ts typ) cts in
      let ctss = map (fun (name,typ) -> name, tn typ) cts in
      let name = cn typ in
      let listwise_ctor = mk_listwise_ctor syms i name typ cts ctss in
      let descr =
        "\n//GENERIC STRUCT " ^ string_of_bid i ^ " INSTANCE " ^
        string_of_bid index ^ ": " ^
        sbt bsym_table typ ^
        "\n"
      in
      descr ^ "struct " ^ name ^ " {\n"
      ^
      catmap ""
      (fun (name,typ) -> "  " ^ typ ^ " " ^ cid_of_flxid name ^ ";\n")
      ctss
      ^
      "  " ^ name ^ "(){}\n" ^
      listwise_ctor
      ^
      "};\n"

(*
    | BBDCL_union (vs,[id,n,[],t',_]) -> 
print_endline ("[flx_tgen2] One component union should have been removed");
      (* ("\n// Skipping solo union " ^ Flx_bsym.id bsym) *)
      "\n// SOLO UNION tgen\n" ^
      let t'' = tsubst sr vs ts t' in
      gen_type syms bsym_table (index,t'')
*)

    | BBDCL_union _ -> ""

    | _ ->
      failwith
      (
        "[gen_type] Expected definition " ^ string_of_bid i ^
        " to be generic primitive, got " ^
        string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) i ^
        " instance types [" ^
        catmap ", " tn ts ^
        "]"
      )
    end

  | _ -> failwith ("[gen_type] Unexpected metatype " ^ sbt bsym_table t)

(* NOTE: distinct types can have the same name if they have the
same simple representation, two types t1,t2 both represented by "int".
This is due to special code that allows Felix to generate "int" etc
for a type mapped to "int" to make the code more readable.
So we have to check the name at this point, because this special
trick is based on the representation.
*)

let gen_type_names syms bsym_table ts =
  if debug then
  print_endline "Flx_tgen.GENERATING TYPE NAMES";
  let s = Buffer.create 100 in
  let handled = ref [] in
  iter
  (fun (i,t) ->
if debug then
print_endline ("Flx_tgen.gen_type_names index = " ^
  string_of_int i ^ ", t = " ^ Flx_btype.st t);

    try
      let name = 
        try cpp_typename syms bsym_table t 
        with exn -> print_endline ("gen_type_names: cpp_typename failed!"); raise exn
      in
      if mem name !handled then
        () (* print_endline ("WOOPS ALREADY HANDLED " ^ name) *)
      else (
        handled := name :: !handled;
        Buffer.add_string s (gen_type_name syms bsym_table (i,t))
      )
    with 
    | Not_found ->
      if debug then
      print_endline ("Can't gen type name " ^ string_of_bid i ^ "=" ^
        Flx_btype.st t);
      failwith ("Can't gen type name " ^ string_of_bid i ^ "=" ^
        Flx_btype.st t)
    | exn -> print_endline ("Seriously screwed up!"); raise exn 
  )
  ts;
  if debug then
  print_endline "Flx_tgen.GENERATING TYPE NAMES -- complete";
  Buffer.contents s

let gen_types syms bsym_table ts =
  if debug then
  print_endline "Flx_tgen.GENERATING TYPES";
  let handled_names = ref [] in
  let s = Buffer.create 100 in
  iter
  (fun ((i,t) as t') ->
    let name = cpp_typename syms bsym_table t in
    if mem name !handled_names then
      () (* print_endline ("WOOPS ALREADY HANDLED " ^ name) *)
    else (
      handled_names := name :: !handled_names;
      Buffer.add_string s (gen_type syms bsym_table t')
    )
  )
  ts;
  if debug then
  print_endline "Flx_tgen.GENERATING TYPES -- complete";
  Buffer.contents s

