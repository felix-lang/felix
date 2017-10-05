open Flx_btype
open Flx_bbdcl

let debug = false

(* The simplify routine runs through the symbol table, identifying
every symbol with requirements. It then replaces the symbol definition
with a simplified list of requirements.

If a symbol A requires R, where R is necessarily a BBDCL_external_code symbol,
then if the requirement is Str s or Str_template s and the argument s
is an empty string, A's requirement for R can be replaced by R's requirements.
Additionally, duplicate requirement indices can be stripped.
[Its not clear if this can impact the order in which requirements are emitted.]

The replacement list, however, may also index empty requirements, so the
list can be updated by first recursively stripping empty requirements 
for each requirement.

This recursion could explode, because requirements are allowed to be
recursive, so we keep trail to bottom out the recursion.

At the end, every requirement in a symbol will point at an Identity,
Virtual, or Str s or Str_template s in which s is a non-empty string,
therefore the effect is to reduce the length of requirement chains.

We note, requirement definitions (BBDCL_external_code) can be polymorphic.
In this case, the requirement must specify the types to be bound to
the definitions type variables. The binding is useless except for
Str s and Str_template s for which s is nonempty and contains
markup which is to be replaced by the C type the Felix type
maps to. Thus it is safe to strip empty polymorphic binding too,
since the polymorphism does not propagate up a dependency chain.

Simplify can be run any time after the symbol table is bound.
It does not remove unused definitions, Flx_use.copy_used should
do that.

A more sophisticated routine could do a full garbage collection.
*)

let rec strip_req bsym_table recstop (rid,ts) : breqs_t =
  if List.mem rid recstop then [rid,ts] else
  let recstop = rid :: recstop in
  let bsym = Flx_bsym_table.find bsym_table rid in
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_external_code  (_,cs,ik,breqs) when Flx_code_spec.isempty cs ->
      strip_reqs' bsym_table recstop breqs   

  | BBDCL_external_code  _ -> [rid,ts]
  | _ -> failwith "Unexpected requirement on non-float"
 
and strip_reqs' bsym_table recstop (rs:breqs_t):breqs_t =
  List.fold_left Flx_list.uniq_cat [] (List.map (strip_req bsym_table recstop) rs)

let strip_reqs bsym_table rs = strip_reqs' bsym_table [] rs

let strip_bbdcl bsym_table bbdcl =
  let f_breqs rs = strip_reqs bsym_table rs in
  match bbdcl with
  | BBDCL_external_type (a,b,c,breqs) ->
      let breqs = f_breqs breqs in
      bbdcl_external_type (a,b,c,breqs)

  | BBDCL_external_const (a,b,c,d,breqs) ->
      let breqs = f_breqs breqs in
      bbdcl_external_const (a,b,c,d,breqs)

  | BBDCL_external_fun (a,b,c,d,breqs,e,f) ->
      let breqs = f_breqs breqs in
      bbdcl_external_fun (a,b,c,d,breqs,e,f)

  | BBDCL_external_code (a,b,c,breqs) ->
      let breqs = f_breqs breqs in
      bbdcl_external_code (a,b,c,breqs)

  | BBDCL_cstruct (a,b,breqs) ->
      let breqs = f_breqs breqs in
       bbdcl_cstruct (a,b,breqs)

  | _ -> bbdcl

let simplify_reqs bsym_table =
if debug then
print_endline ("Flx_breqs: simplify");
  Flx_bsym_table.iter (fun bid parent bsym -> let bbdcl = Flx_bsym.bbdcl bsym in
     let bbdcl = strip_bbdcl bsym_table bbdcl in
     Flx_bsym_table.update_bbdcl bsym_table bid bbdcl
  ) 
  bsym_table 
