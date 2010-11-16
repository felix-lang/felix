open Flx_ast
open Flx_types
open Flx_btype
open Flx_bbdcl
open Flx_mtypes2
open Flx_unify
open Flx_print
open Flx_util
open Flx_exceptions
open Flx_backend_config

(* these words are either keywords or peculiar to the
   compiler code generator, so we have to avoid a clash.
   This list has been constructed by trial and error ..

   note the RHS value is irrelevant, it just has to be different
   to the LHS value ..
*)

let fixups = [
  (* special names in thread frame *)
  "argc","_argc";
  "argv","_argv";
  "flx_stdin","_flx_stdin";
  "flx_stdout","_flx_stdout";
  "flx_stderr","_flx_stderr";
  "gc","_gc";
] @ List.map (fun k -> k, "_" ^ k) cpp_keywords

let cid_of_flxid s =
  let n = String.length s in
  let id = Buffer.create (n+10) in
  (* if the value is prefixed with a number, prepend an underscore *)
  if n > 1 && s.[0] >= '0' && s.[0] <= '9' then Buffer.add_char id '_';
  for i=0 to n - 1 do
    (* from http://www.w3.org/TR/html4/sgml/entities.html *)
    match s.[i] with
    | ' '  -> Buffer.add_string id "__sp_"
    | '!'  -> Buffer.add_string id "__excl_"
    | '"'  -> Buffer.add_string id "__quot_"
    | '#'  -> Buffer.add_string id "__num_"
    | '$'  -> Buffer.add_string id "__dollar_"
    | '%'  -> Buffer.add_string id "__percnt_"
    | '&'  -> Buffer.add_string id "__amp_"
    | '\'' -> Buffer.add_string id "__apos_"
    | '('  -> Buffer.add_string id "__lpar_"
    | ')'  -> Buffer.add_string id "__rpar_"
    | '*'  -> Buffer.add_string id "__ast_"
    | '+'  -> Buffer.add_string id "__plus_"
    | ','  -> Buffer.add_string id "__comma_"
    | '-'  -> Buffer.add_string id "__hyphen_"
    | '.'  -> Buffer.add_string id "__period_"
    | '/'  -> Buffer.add_string id "__sol_"
    | ':'  -> Buffer.add_string id "__colon_"
    | ';'  -> Buffer.add_string id "__semi_"
    | '<'  -> Buffer.add_string id "__lt_"
    | '='  -> Buffer.add_string id "__equals_"
    | '>'  -> Buffer.add_string id "__gt_"
    | '?'  -> Buffer.add_string id "__quest_"
    | '@'  -> Buffer.add_string id "__commat_"
    | '['  -> Buffer.add_string id "__lsqb_"
    | '\\' -> Buffer.add_string id "__bsol_"
    | ']'  -> Buffer.add_string id "__rsqb_"
    | '^'  -> Buffer.add_string id "__caret_"
    (* | '_'  -> Buffer.add_string id "__lowbar_" *)
    | '`'  -> Buffer.add_string id "__grave_"
    | '{'  -> Buffer.add_string id "__lcub_"
    | '|'  -> Buffer.add_string id "__verbar_"
    | '}'  -> Buffer.add_string id "__rcub_"
    | '~'  -> Buffer.add_string id "__tilde_"
    | x    -> Buffer.add_char id x
  done;
  let name = Buffer.contents id in
  let name = match name with
    | "errno" -> "__flx_errno_"
    | _ -> name
  in
  try List.assoc name fixups with Not_found -> name

let cid_of_bid bid =
  string_of_int bid

(* basic name mangler *)
let cpp_name bsym_table index =
  let bsym =
    try Flx_bsym_table.find bsym_table index
    with _ -> failwith ("[cpp_name] Can't find index " ^ string_of_bid index)
  in
  (match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (_,_,_,BTYP_void,_) -> "_p"
  | BBDCL_fun (_,_,_,_,_) -> "_f"
  | BBDCL_external_fun (_,_,_,_,_,_,`Callback _) -> "_cf"
  | BBDCL_val (_,_,(`Val | `Var | `Ref)) -> "_v"
  | BBDCL_val (_,_,`Tmp) -> "_tmp"
  | _ ->
      syserr (Flx_bsym.sr bsym) "cpp_name expected func,proc,var,val,ref, or tmp"
  ) ^ cid_of_bid index ^ "_" ^ cid_of_flxid (Flx_bsym.id bsym)

let cpp_instance_name' syms bsym_table index ts =
  let inst =
    try Hashtbl.find syms.instances (index,ts)
    with Not_found ->
    let id =
      try Flx_bsym_table.find_id bsym_table index with Not_found ->
        failwith ("[cpp_instance_name'] Can't find <" ^
          string_of_bid index ^ ">")
    in
    let has_variables =
      List.fold_left
      (fun truth t -> truth || var_occurs t)
      false
      ts
    in
    failwith
    (
      "[cpp_instance_name] unable to find instance " ^ id ^
      "<" ^ string_of_bid index ^ ">[" ^
      catmap ", " (sbt bsym_table) ts ^ "]"
      ^ (if has_variables then " .. a subscript contains a type variable" else "")
    )
  in
  "_i" ^ cid_of_bid inst ^ cpp_name bsym_table index

let is_export syms id =
  let bifaces = syms.bifaces in
  try
    List.iter
    (function
      | BIFACE_export_fun (_,_,s)
      | BIFACE_export_python_fun (_,_,s)
      | BIFACE_export_type (_,_,s) ->
        if id = s then raise Not_found
     )
     bifaces;
     false
  with Not_found -> true

let cpp_instance_name syms bsym_table index ts =
  let long_name = cpp_instance_name' syms bsym_table index ts in
  if syms.compiler_options.mangle_names then long_name else
  let id =
    try Flx_bsym_table.find_id bsym_table index
    with _ -> failwith ("[cpp_name] Can't find index " ^ string_of_bid index)
  in
  let id' = cid_of_flxid id in
  if id = id' then
  begin
    let inst =
      try Hashtbl.find syms.quick_names id
      with Not_found ->
        Hashtbl.add syms.quick_names id (index,ts);
        index,ts
    in
      if (index,ts) <> inst then long_name else
      if is_export syms id then long_name else id
  end
  else long_name

let tix syms bsym_table t =
  let t =
    match t with
    | BTYP_function (BTYP_void,cod) -> btyp_function (btyp_tuple [],cod)
    | x -> x
  in
  try Hashtbl.find syms.registry t
  with Not_found ->
    failwith ("Cannot find type " ^sbt bsym_table t ^" in registry")

let rec cpp_type_classname syms bsym_table t =
  let tix t = tix syms bsym_table t in
  let t = fold syms.counter t in
  try match unfold t with
  | BTYP_type_var (i,mt) ->
      failwith ("[cpp_type_classname] Can't name type variable " ^
        string_of_bid i ^ ":"^ sbt bsym_table mt)
  | BTYP_fix i -> failwith "[cpp_type_classname] Can't name type fixpoint"
  | BTYP_void -> "void" (* failwith "void doesn't have a classname" *)
  | BTYP_tuple [] -> "unit"

  | BTYP_pointer t' -> cpp_type_classname syms bsym_table t' ^ "*"
 
  | BTYP_function (_,BTYP_void) -> "_pt" ^ cid_of_bid (tix t)
  | BTYP_function _ -> "_ft" ^ cid_of_bid (tix t)
  | BTYP_cfunction _ -> "_cft" ^ cid_of_bid (tix t)
  | BTYP_array _ -> "_at" ^ cid_of_bid (tix t)
  | BTYP_tuple _ -> "_tt" ^ cid_of_bid (tix t)
  | BTYP_record _ -> "_art" ^ cid_of_bid (tix t)
  | BTYP_variant _ -> "_avt" ^ cid_of_bid (tix t)
  | BTYP_sum _ -> "_st" ^ cid_of_bid (tix t)
  | BTYP_unitsum k -> "_us" ^ string_of_int k

  | BTYP_inst (i,ts) ->
    let cal_prefix = function
      | BBDCL_struct _  -> "_s"
      | BBDCL_union _   -> "_u"
      | BBDCL_external_type _  -> "_a"
      | BBDCL_newtype _ -> "_abstr_"
      | _ -> "_unk_"
    in
    if ts = [] then
      let bsym = Flx_bsym_table.find bsym_table i in
      let fname = Flx_bsym.id bsym in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_cstruct _ -> fname
      (*
      | BBDCL_external_type (_,_,CS_str "char",_) -> "char" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str "int",_) -> "int" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str "short",_) -> "short" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str "long",_) -> "long" (* hack .. *)

      | BBDCL_external_type (_,_,CS_str "float",_) -> "float" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str "double",_) -> "double" (* hack .. *)

      | BBDCL_external_type (_,_,CS_str_template "char",_) -> "char" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str_template "int",_) -> "int" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str_template "short",_) -> "short" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str_template "long",_) -> "long" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str_template "float",_) -> "float" (* hack .. *)
      | BBDCL_external_type (_,_,CS_str_template "double",_) -> "double" (* hack .. *)
      *)

      (* this is more general than the above: if the felix name and the C name
       * are the same, just use the cname, don't synthesise a special name.
       * In this case the type generator will emit a commented out typedef
       * of the name to itself. There is an implied invariant here: the
       * felix and C names are identifiers. We hope this doesn't lead to any
       * name clashes.
       *
       * TODO: we could make some more nice special cases, such as C names
       * looking like "X*" and call them _p_X, or even _p<X> .. 
       *
       * We should note: Felix uses "function style casts" in some places,
       * and these require the typename be an identifier. Also other places
       * in the code generator require identifiers, for example the RTTI
       * generator mangles the C name with prefixes and suffices to get
       * unique names for the type records.
       *
       * Also, the hackery above which we just replaced will not support
       * names like "unsigned int". In fact C++ doesn't allow this name
       * in a functional style cast either. We would love to just use
       * the Felix names in C too, and provide standard typedefs,
       * but I fear names like "uint" or "unsigned_int" may clash with user
       * code.
       *
       * However this problem can now be "solved" in the library instead
       * of the compiler, by emitting a C typedef such as
       * "typedef unsigned int uint;" and then in Felix saying
       * type uint="uint"; and now the code generator will use "uint"
       * as the name for unsigned integers.
       *
       * The comparison in the type generator "cname = felix name" is a
       * real hack: we would like to use instead a particular name generated
       * here in some cases, instead of the name the generator makes, but
       * we have no way to propagate a flag saying "use this name".
       *)
      | BBDCL_external_type (_,_,CS_str cname,_) 
      | BBDCL_external_type (_,_,CS_str_template cname,_) 
         when cname = fname -> cname

      | bbdcl ->
          let prefix = cal_prefix bbdcl in
          prefix ^ cid_of_bid i ^ "t_" ^ cid_of_bid (tix t)
    else
      "_poly_" ^ cid_of_bid i ^ "t_" ^ cid_of_bid (tix t)

  | _ ->
    failwith
    (
      "[cpp_type_classname] Unexpected " ^
      sbt bsym_table t
    )
  with Not_found ->
    failwith
    (
      "[cpp_type_classname] Expected type "^
      sbt bsym_table t ^
      " to be in registry"
    )

let rec cpp_typename syms bsym_table t =
  match unfold t with
  | BTYP_function _ -> cpp_type_classname syms bsym_table t ^ "*"
  | BTYP_cfunction _ -> cpp_type_classname syms bsym_table t ^ "*"
  | BTYP_pointer t -> cpp_typename syms bsym_table t ^ "*"
  | _ -> cpp_type_classname syms bsym_table t

let cpp_ltypename syms bsym_table t = cpp_typename syms bsym_table t
