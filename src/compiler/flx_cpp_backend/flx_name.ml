open Flx_types
open Flx_btype
open Flx_bbdcl
open Flx_mtypes2
open Flx_unify
open Flx_print
open Flx_util
open Flx_exceptions
open Flx_backend_config

module CS = Flx_code_spec
let debug =false 

(* these words are either keywords or peculiar to the
   compiler code generator, so we have to avoid a clash.
   This list has been constructed by trial and error ..

   note the RHS value is irrelevant, it just has to be different
   to the LHS value ..
*)
let string_hash s =
  let hash = ref 5381 in
  String.iter (fun ch -> 
    let tmp = ((!hash) * 33 + Char.code ch) mod 1073741823 (* 2^30-1*) in
    hash := if tmp < 0 then (tmp * -1) else tmp) s;
  !hash

let fixups = [
  (* special names in thread frame *)
  "argc","_argc";
  "argv","_argv";
  "flx_stdin","_flx_stdin";
  "flx_stdout","_flx_stdout";
  "flx_stderr","_flx_stderr";
  "gc","_gc";
  "apply","_apply"; (* we get a class apply with method apply otherwise *)
] @ List.map (fun k -> k, "_" ^ k) cpp_keywords

let plain_cid_of_flxid s =
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
  let name = try List.assoc name fixups with Not_found -> name in
  name

let cid_of_flxid s =
  let name = plain_cid_of_flxid s in
  if String.length name > 40 then 
    let h = string_hash name in
    String.sub name 0 4 ^ "_hash_" ^ string_of_int h
  else 
    name

let cid_of_bid bid =
  string_of_int bid

let cpp_kind_prefix bsym =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (_,_,_,BTYP_void,_,_) -> "_p"
  | BBDCL_fun (_,_,_,BTYP_fix (0,_),_,_) -> "_nrtf"
  | BBDCL_fun (_,_,_,_,_,_) -> "_f"
  | BBDCL_external_fun (_,_,_,_,_,_,`Callback _) -> "_cf"
  | BBDCL_val (_,_,(`Val | `Var | `Ref | `Once)) -> "_v"
  | BBDCL_val (_,_,`Tmp) -> "_tmp"
  | _ ->
      syserr (Flx_bsym.sr bsym) "cpp_name expected func,proc,var,val,ref,once or tmp"

(* basic name mangler *)
let cpp_name bsym_table index = (* "_flxN_" ^ string_of_int index *)
  let bsym =
    try Flx_bsym_table.find bsym_table index
    with _ -> failwith ("[cpp_name] Can't find index " ^ string_of_bid index)
  in 
    cpp_kind_prefix bsym ^ 
    cid_of_bid index ^ "_" ^ 
    cid_of_flxid (Flx_bsym.id bsym)

let cpp_instance_name' syms bsym_table index ts =
  let inst =
    (* HACK! *)
    try Hashtbl.find syms.instances (index,ts)
    with Not_found ->
    let id =
      try Flx_bsym_table.find_id bsym_table index with Not_found ->
(*
print_endline "Cannot find entry";
*)
        failwith ("[cpp_instance_name'] Can't find <" ^
          string_of_bid index ^ ">")
    in
    let has_variables =
      List.fold_left
      (fun truth t -> truth || Flx_btype_occurs.var_occurs bsym_table t)
      false
      ts
    in
(*
print_endline "Cannot find instance";
*)
    failwith
    (
      "[cpp_instance_name] unable to find instance " ^ id ^
      "<" ^ string_of_bid index ^ ">[" ^
      catmap ", " (sbt bsym_table) ts ^ "]"
      ^ (if has_variables then " .. a subscript contains a type variable" else "")
    )
  in
(*
  "_i" ^ cid_of_bid inst ^ cpp_name bsym_table index
  "_flxI_"  ^ string_of_int inst ^ "_N_" ^ string_of_int index
*)
  let bsym =
    try Flx_bsym_table.find bsym_table index
    with _ -> failwith ("[cpp_name] Can't find index " ^ string_of_bid index)
  in 
    cpp_kind_prefix bsym ^ 
    "I"^ cid_of_bid inst ^ "_" ^ 
    cid_of_flxid (Flx_bsym.id bsym)

let is_export syms bsym_table id =
  let bifaces = syms.bifaces in
  try
    List.iter
    (function
      | BIFACE_export_fun (_,_,s)
      | BIFACE_export_cfun (_,_,s)
      | BIFACE_export_python_fun (_,_,s)
      | BIFACE_export_type (_,_,s) ->
        if id = s then raise Not_found
      | BIFACE_export_struct (_,idx) -> if 
        Flx_bsym_table.mem bsym_table idx then raise Not_found
      | BIFACE_export_union (_,idx,s) -> if (id = s ||
        Flx_bsym_table.mem bsym_table idx) then raise Not_found
      | BIFACE_export_requirement (_,_) -> ()
     )
     bifaces;
     false
  with Not_found -> true

let cpp_instance_name syms bsym_table index ts =
  let long_name = cpp_instance_name' syms bsym_table index ts in
  if syms.compiler_options.Flx_options.mangle_names then long_name else
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
      if is_export syms bsym_table id then long_name else id
  end
  else long_name

let tix msg syms bsym_table t =
  let t =
    match t with
    | BTYP_function (BTYP_void,cod) -> btyp_function (btyp_tuple [],cod)
    | x -> x
  in
  let t' = Flx_fold.minimise bsym_table syms.counter t in
  try Flx_treg.find_type_index syms bsym_table t'
  with Not_found ->
    if t = t' then
    failwith (msg^" Cannot find type " ^sbt bsym_table t ^" in registry")
    else
    failwith (msg^"[flx_name:tix] Cannot find type " ^sbt bsym_table t' ^
      "\n(=minimised("^ sbt bsym_table t ^"))\nin registry")


let rec cpp_type_classname syms bsym_table t =
if debug then
print_endline ("Flx_tgen.cpp_type_classname " ^ sbt bsym_table t);
  let tn t = cpp_typename syms bsym_table t in
  let tix t = tix "[flx_name:cpp_type_classname" syms bsym_table t in
  let t = Flx_fold.fold bsym_table syms.counter t in
  let t' = unfold "flx_name: cpp_type_classname" t in
  try match t' with

  | BTYP_typeof _ -> assert false
  | BTYP_hole -> assert false
  | BTYP_uniq _ -> assert false
  | BTYP_rref _ -> assert false
  | BTYP_wref _ -> assert false

  | BTYP_type_var (i,mt) ->
      failwith ("[cpp_type_classname] Can't name type variable " ^
        string_of_bid i ^ ":"^ Flx_kind.sk mt)
  | BTYP_fix (i,_) -> "void" (* failwith "[cpp_type_classname] Can't name type fixpoint" *)
  | BTYP_none -> "none" (* hack needed for null case in pgen *)
  | BTYP_void -> (* print_endline "WARNING cpp_type_classname of void"; *) "void" (* failwith "void doesn't have a classname" *)
  | BTYP_label -> " ::flx::rtl::jump_address_t" (* space required cause X<::y> is trigraph *)
  | BTYP_tuple [] -> " ::flx::rtl::cl_t" (* COMPACT LINEAR! *)
  | t when islinear_type bsym_table t -> " ::flx::rtl::cl_t"

  | BTYP_pointer t' -> cpp_type_classname syms bsym_table t' ^ "*"

  | BTYP_cltpointer (d,c)
  | BTYP_cltrref (d,c)
  | BTYP_cltwref (d,c) ->
      "::flx::rtl::clptr_t";

  | BTYP_effector (d,e,c) -> 
    print_endline ("[Flx_name:cpp_type_classname] Attempt to name effector type in code generator:" ^ sbt bsym_table t');
    print_endline (" .. using equivalent function type instead");
    cpp_type_classname syms bsym_table (btyp_function (d,c))

 
  | BTYP_function (_,BTYP_void) -> "_pt" ^ cid_of_bid (tix t)
  | BTYP_function _ -> "_ft" ^ cid_of_bid (tix t)

  | BTYP_cfunction _ -> "_cft" ^ cid_of_bid (tix t)
  | BTYP_array _ -> "_at" ^ cid_of_bid (tix t)
  | BTYP_tuple _ -> "_tt" ^ cid_of_bid (tix t)
  | BTYP_tuple_cons _ -> "_tt" ^ cid_of_bid (tix t)
  | BTYP_tuple_snoc _ -> "_tt" ^ cid_of_bid (tix t)
(*  | BTYP_tuple ts -> "_tt"^string_of_int (List.length ts)^"<" ^ catmap "," tn ts ^ ">"  *)
  | BTYP_record _  -> "_art" ^ cid_of_bid (tix t)
(*
  | BTYP_variant _ -> "_avt" ^ cid_of_bid (tix t)
  | BTYP_sum _ -> "_st" ^ cid_of_bid (tix t)
*)
  | BTYP_variant ls ->      
    (* "::flx::rtl::_variant_"; *)
    "::flx::rtl::_uctor_";

  | BTYP_rptsum _ 
  | BTYP_sum _ ->
    begin match Flx_vrep.cal_variant_rep bsym_table t with
    | Flx_vrep.VR_self -> print_endline "WARNING cpp_type_classname of VR_self (1)"; assert false
    | Flx_vrep.VR_int -> "int"
    | Flx_vrep.VR_nullptr -> "void*"
    | Flx_vrep.VR_packed -> "void*"
    | Flx_vrep.VR_uctor -> " ::flx::rtl::_uctor_"
    end

  | BTYP_unitsum k -> "_us" ^ string_of_int k

  | BTYP_vinst (i,ts,_) -> assert false

  | BTYP_inst (i,ts,_) ->
    let bsym = Flx_bsym_table.find bsym_table i in
    let fname = Flx_bsym.id bsym in
    let bbdcl = Flx_bsym.bbdcl bsym in
    let sr = Flx_bsym.sr bsym in
    let cal_prefix = function
      | BBDCL_struct _  -> "_s"
(*
      | BBDCL_union _   -> "_u"
*)
      | BBDCL_union _   -> ""
      | BBDCL_external_type _  -> "_a"
      | BBDCL_newtype _ -> "_abstr_"
      | _ -> "_unk_"
    in
    begin match bbdcl with 
(*
    | BBDCL_union (vs, [id,n,[],t',_]) -> 
print_endline ("[flx_name] One component union should have been removed");
      let t'' = tsubst sr vs ts t' in
      cpp_type_classname syms bsym_table t''
*)
    | BBDCL_union _ ->
      begin match Flx_vrep.cal_variant_rep bsym_table t with
      | Flx_vrep.VR_self -> print_endline "WARNING cpp_type_classname of VR_self (2)"; assert false
      | Flx_vrep.VR_int -> "int"
      | Flx_vrep.VR_nullptr -> "void*"
      | Flx_vrep.VR_packed -> "void*"
      | Flx_vrep.VR_uctor -> "::flx::rtl::_uctor_"
      end
    | _ ->
    if ts = [] then
      match bbdcl with
      | BBDCL_cstruct _ -> fname
      (* if the felix name and the C name
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
      | BBDCL_external_type (_,_,CS.Str cname,_)
      | BBDCL_external_type (_,_,CS.Str_template cname,_)
         when cname = fname -> cname

      | bbdcl ->
          let prefix = cal_prefix bbdcl in
          prefix ^ cid_of_bid i ^ "t_" ^ cid_of_bid (tix t)
    else
      "_poly_" ^ cid_of_bid i ^ "t_" ^ cid_of_bid (tix t)
  end
  | BTYP_rev _
  | BTYP_polyrecord _
  | BTYP_polyvariant _
  | BTYP_intersect _
  | BTYP_union _

  | BTYP_type_tuple _
  | BTYP_type_function _
  | BTYP_type_apply _
  | BTYP_type_map _
  | BTYP_type_match _
  | BTYP_subtype_match _

  | BTYP_type_set _
  | BTYP_type_set_union _
  | BTYP_type_set_intersection _
    ->
    failwith
    (
      "[cpp_type_classname] Unexpected type " ^
      sbt bsym_table t
    )
  with Not_found ->
    failwith
    (
      "[cpp_type_classname] Expected type "^
      sbt bsym_table t ^
      " to be in registry"
    )

and cpp_structure_name syms bsym_table t =
  let tn t = cpp_typename syms bsym_table t in
  let tix t = tix "[flx_name:cpp_structure_name]" syms bsym_table t in
  let t = Flx_fold.fold bsym_table syms.counter t in
  let t' = unfold "flx_name: cpp_structure_name" t in
  try match t' with
  | BTYP_type_var (i,mt) ->
      failwith ("[cpp_type_classname] Can't name type variable " ^
        string_of_bid i ^ ":"^ Flx_kind.sk mt)
  | BTYP_fix (i,_) -> "_fix<"^string_of_int (-i)^">" (* failwith "[cpp_type_classname] Can't name type fixpoint" *)
  | BTYP_none -> "none" (* hack needed for null case in pgen *)
  | BTYP_void -> print_endline ("WARNING cpp_structure_name of void"); "void" (* failwith "void doesn't have a classname" *)
  | BTYP_tuple [] -> "int" (* COMPACT LINEAR! *)

  | BTYP_pointer t' -> cpp_type_classname syms bsym_table t' ^ "*"
 
  | BTYP_effector (d,_,BTYP_void) -> "_pt<" ^tn d ^ ">"
  | BTYP_effector (d,_,c) -> "_ft<" ^ tn d ^ "," ^ tn c ^ ">" 
  | BTYP_function (d,BTYP_void) -> "_pt<" ^tn d ^ ">"
  | BTYP_function (d,c) -> "_ft<" ^ tn d ^ "," ^ tn c ^ ">" 
  | BTYP_cfunction (d,c) -> "_cft<" ^  tn d ^ "," ^ tn c ^">"
  | BTYP_array (e,BTYP_unitsum i) -> "_at<" ^ tn e ^ "," ^ string_of_int i ^ ">" 
  | BTYP_array (e,i) -> 
     (*failwith ("Generalisd arrays not supported") *)
      "_gat<" ^ tn e ^ "," ^ tn i ^ ">" 

  | BTYP_tuple ts -> "_tt"^string_of_int (List.length ts)^"<" ^ catmap "," tn ts ^ ">" 
  | BTYP_record _  -> "_art" ^ cid_of_bid (tix t)
(*
  | BTYP_variant _ -> "_avt" ^ cid_of_bid (tix t)
  | BTYP_sum _ -> "_st" ^ cid_of_bid (tix t)
*)
  | BTYP_variant _ -> "::flx::rtl::_uctor_"

  | BTYP_cltpointer (d,c)
  | BTYP_cltrref (d,c)
  | BTYP_cltwref (d,c) ->
      "::flx::rtl::clptr_t";


  | BTYP_rptsum _
  | BTYP_sum _ ->
    begin match Flx_vrep.cal_variant_rep bsym_table t with
    | Flx_vrep.VR_self -> print_endline ("WARNING cpp_structure_name of VR_self (1)"); assert false
    | Flx_vrep.VR_int -> "int"
    | Flx_vrep.VR_nullptr -> "void*"
    | Flx_vrep.VR_packed -> "void*"
    | Flx_vrep.VR_uctor -> "::flx::rtl::_uctor_"
    end

  | BTYP_unitsum k -> "_us" ^ string_of_int k

  | BTYP_vinst _ -> assert false

  | BTYP_inst (i,ts,_) ->
    let bsym = Flx_bsym_table.find bsym_table i in
    let fname = Flx_bsym.id bsym in
    let bbdcl = Flx_bsym.bbdcl bsym in
    let sr = Flx_bsym.sr bsym in
    let cal_prefix = function
      | BBDCL_struct _  -> "_s"
(*
      | BBDCL_union _   -> "_u"
*)
      | BBDCL_union _   -> ""
      | BBDCL_external_type _  -> "_a"
      | BBDCL_newtype _ -> "_abstr_"
      | _ -> "_unk_"
    in
    begin match bbdcl with 
    (* should have been removed by strabs *)
    | BBDCL_union (vs, [id,n,[],t',_,false]) -> assert false
(*
      let t'' = tsubst sr vs ts t' in
      cpp_type_classname syms bsym_table t''
*)

    | BBDCL_union _ ->
      begin match Flx_vrep.cal_variant_rep bsym_table t with
      | Flx_vrep.VR_self -> print_endline ("WARNING cpp_structure_name of VR_self (2)"); assert false
      | Flx_vrep.VR_int -> "int"
      | Flx_vrep.VR_nullptr -> "void*"
      | Flx_vrep.VR_packed -> "void*"
      | Flx_vrep.VR_uctor -> "::flx::rtl::_uctor_"
      end
    | _ ->
    if ts = [] then
      match bbdcl with
      | BBDCL_cstruct _ -> fname
      | BBDCL_external_type (_,_,CS.Str cname,_)
      | BBDCL_external_type (_,_,CS.Str_template cname,_)
         when cname = fname -> cname

      | bbdcl ->
          let prefix = cal_prefix bbdcl in
          prefix ^ cid_of_bid i ^ "t_" ^ cid_of_bid (tix t)
    else
      "_poly_" ^ cid_of_bid i ^ "t_" ^ cid_of_bid (tix t)
  end
  | _ ->
    failwith
    (
      "[cpp_structure_name] Unexpected " ^
      sbt bsym_table t
    )
  with Not_found ->
    failwith
    (
      "[cpp_structure_name] Expected type "^
      sbt bsym_table t ^
      " to be in registry"
    )


and cpp_typename syms bsym_table t =
  match unfold "flx_name: cpp_typename" t with
  | BTYP_effector _ -> cpp_type_classname syms bsym_table t ^ "*"
  | BTYP_function _ -> cpp_type_classname syms bsym_table t ^ "*"
  | BTYP_cfunction _ -> cpp_type_classname syms bsym_table t ^ "*"
  | BTYP_pointer t -> cpp_typename syms bsym_table t ^ "*"
(*
  | BTYP_inst (i,ts) ->
    let bsym = Flx_bsym_table.find bsym_table i in
    let fname = Flx_bsym.id bsym in
    let bbdcl = Flx_bsym.bbdcl bsym in
    let sr = Flx_bsym.sr bsym in
    begin match bbdcl with
    | _ -> cpp_type_classname syms bsym_table t
    end
*)

  | _ -> cpp_type_classname syms bsym_table t

let cpp_ltypename syms bsym_table t = cpp_typename syms bsym_table t

