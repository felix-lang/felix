open Flx_util
open Flx_list
open Flx_name
open List
open Flx_bid

let gen_ctor syms bsym_table name display funs extra_args extra_inits ts props =
  let requires_ptf = mem `Requires_ptf props in
  let requires_pc = mem `Yields props in
(*
print_endline ("Generating constructor for " ^ name ^ " ldisplay=" ^
string_of_int (length display) ^ ", lextras = " ^ string_of_int (length extra_args)  ^
", ptf = " ^ string_of_int (if requires_ptf then 1 else 0)  ^ ", display="
);
List.iter (fun (i,vslen) ->
  print_endline  (
       let instname = cpp_instance_name syms bsym_table i (list_prefix ts vslen) in
      "    " ^ instname ^ " *pptr" ^ instname
))
display;
*)
  name^"::"^name^
  (if length display + length extra_args = 0 then
  (if requires_ptf then "(FLX_FPAR_DECL_ONLY)" else "()")
  else
  "\n  (\n" ^
  (if requires_ptf then
  "    FLX_FPAR_DECL\n"
  else ""
  )
  ^
  cat ",\n"
  (
    map
    (
      fun (i,vslen) ->
        let instname = cpp_instance_name syms bsym_table i (list_prefix ts vslen) in
      "    " ^ instname ^ " *pptr" ^ instname
    )
    display
    @
    map
    (
      fun (t,a) -> "    " ^ t ^ " _"^a
    )
    extra_args
  )^
  "\n  )\n"
  )
  ^
  (if
    length display + length funs +
    length extra_args + length extra_inits +
    (if requires_pc then 1 else 0)
    = 0
  then (if requires_ptf then "FLX_FMEM_INIT_ONLY" else "")
  else
  (if requires_ptf then
  "  FLX_FMEM_INIT "
  else " : "
  )
  ^
  cat ",\n"
  (
    (if requires_pc then ["pc(0)"] else [])
    @
    map
    (
      fun (i,vslen) -> let instname = cpp_instance_name syms bsym_table i (list_prefix ts vslen) in
      "  ptr" ^ instname ^ "(pptr"^instname^")"
    )
    display
    @
    map
    (fun (index,t)->
      cpp_instance_name syms bsym_table index ts
      ^ "(0)"
    )
    funs
    @
    map
    (fun (t,a) -> "  " ^a ^ "(_"^a^")")
    extra_args
    @
    map
    (fun x -> "  " ^x)
    extra_inits
  )) ^
  " {}\n"

