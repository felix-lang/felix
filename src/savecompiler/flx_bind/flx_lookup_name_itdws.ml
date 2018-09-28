open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_name_map
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

let debug = false 
let debugid = ""

let handle_function 
  inner_type_of_index_with_ts
  state bsym_table rs sra srn name ts index 
=
  let sym = get_data state.sym_table index in
  match sym.Flx_sym.symdef with
  | SYMDEF_function _
  | SYMDEF_fun _
  | SYMDEF_struct _
  | SYMDEF_cstruct _
  | SYMDEF_nonconst_ctor _
  | SYMDEF_callback _ ->
      let t = inner_type_of_index_with_ts
        state
        bsym_table
        rs
        sym.Flx_sym.sr
        index
        ts
      in

      (* Make sure we got a function type back. *)
      begin match t with
      | BTYP_cfunction _ | BTYP_function _ | BTYP_effector _ -> ()
      | BTYP_fix _ -> raise (Free_fixpoint t)
      | _ ->
          ignore (try unfold "flx_lookup" t with | _ -> raise (Free_fixpoint t));
          clierrx "[flx_bind/flx_lookup.ml:3272: E152] " sra
          (
            "[handle_function]: closure operator expected '" ^ name ^
            "' to have function type, got '" ^
            sbt bsym_table t ^ "'"
          )
      end;

if debug then
print_endline ("flx_lookup.handle_function.bexpr_closure");
      bexpr_closure t (index,ts)

  | SYMDEF_type_alias (TYP_typefun _) ->
      (* THIS IS A HACK .. WE KNOW THE TYPE IS NOT NEEDED BY THE CALLER .. *)
      let t = btyp_function (btyp_none (), btyp_none ()) in
if debug then
print_endline ("flx_lookup.SYMDEF_type_alias.bexpr_closure");
      bexpr_closure t (index,ts)

  | _ ->
      clierrx "[flx_bind/flx_lookup.ml:3288: E153] " sra
      (
        "[handle_function] Expected " ^ name ^ " to be function, got: " ^
        string_of_symdef sym.Flx_sym.symdef name sym.Flx_sym.vs
      )

and handle_variable 
  build_env
  bind_type'
  state 
  bsym_table env rs index id sr ts t t2 =
  (* HACKED the params argument to [] .. this is WRONG!! *)
  let mkenv i = build_env state bsym_table (Some i) in

  (* we have to check the variable is the right type *)
  let t = bind_type' state bsym_table env rs sr t [] mkenv in
  let ts = adjust_ts state.sym_table bsym_table sr index ts in
  let vs = find_vs state.sym_table bsym_table index in
  let bvs = List.map (fun (s,i,tp) -> s,i, Flx_btype.bmt "Flx_lookup_name_itdws" tp) (fst vs) in
  let t = beta_reduce "flx_lookup: handle_variabe" state.counter bsym_table sr (tsubst sr bvs ts t) in
(*
print_endline ("Handle variable " ^ si index ^ "=" ^ id);
*)
  match t with
  | BTYP_effector (d,_,c)
  | BTYP_cfunction (d,c)
  | BTYP_function (d,c) ->
      if type_match bsym_table state.counter d t2 then
      begin
(*
print_endline ("LOOKUP 2: varname " ^ si index);
*)
        Some (bexpr_varname t (index, ts))
      end
      else
        clierrx "[flx_bind/flx_lookup.ml:3318: E154] " sr
        (
          "[handle_variable(1)] Expected variable " ^ id ^
          "<" ^ string_of_bid index ^
          "> to have function type with signature " ^
          sbt bsym_table t2 ^
          ", got function type:\n" ^
          sbt bsym_table t
        )

    (* anything other than function type, dont check the sig,
       just return it..
    *)
  | _ -> 
(*
print_endline ("LOOKUP 3: varname " ^ si index);
*)
    Some (bexpr_varname t (index, ts))


let rec lookup_name_in_table_dirs_with_sig
  inner_type_of_index_with_ts
  resolve_overload
  build_env
  bind_type'

  state
  bsym_table
  table
  dirs
  caller_env env rs
  sra srn name ts t2
=
  let lookup_name_in_table_dirs_with_sig
    state
    bsym_table
    table
    dirs
    caller_env env rs
    sra srn name ts t2
  =
   lookup_name_in_table_dirs_with_sig
    inner_type_of_index_with_ts
    resolve_overload
    build_env
    bind_type'

    state
    bsym_table
    table
    dirs
    caller_env env rs
    sra srn name ts t2
  in

if name = debugid then
  print_endline
  (
    "LOOKUP NAME "^name ^"["^
    catmap "," (sbt bsym_table) ts ^
    "] IN TABLE DIRS WITH SIG " ^ catmap "," (sbt bsym_table) t2
  );

(* THIS IS AN UGLY HACK but it may impact the result. I modified this
in type lookup routine and hell broke out. I think this is because
returning None immediately doesn't cause opened modules to be looked in,
whereas a failure returning an empty FunctionEntry does.
*)

  let result:entry_set_t =
    match Flx_name_lookup.lookup_name_in_htab table name with
    | Some x -> x
    | None -> FunctionEntry []
  in
  match result with
  | NonFunctionEntry (index) ->
    begin match get_data state.sym_table (sye index) with
    { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry }->
if name = debugid then
    print_endline ("FOUND nonfunction " ^ id);
    begin match entry with
    | SYMDEF_inherit _ ->
      clierrx "[flx_bind/flx_lookup.ml:3369: E155] " sra "Woops found inherit in lookup_name_in_table_dirs_with_sig"
    | SYMDEF_inherit_fun _ ->
      clierrx "[flx_bind/flx_lookup.ml:3371: E156] " sra "Woops found inherit function in lookup_name_in_table_dirs_with_sig"

    | (SYMDEF_cstruct _ | SYMDEF_struct _ )
      when
        (match t2 with
        | [BTYP_record _] -> true
        | _ -> false
        )
      ->
        (*
        print_endline ("lookup_name_in_table_dirs_with_sig finds struct constructor " ^ id);
        print_endline ("Record Argument type is " ^ catmap "," (sbt bsym_table) t2);
        *)
if debug then
print_endline ("flx_lookup.SYMDEF_type_alias.bexpr_closure");
        Some (bexpr_closure (btyp_inst (sye index,ts,Flx_kind.KIND_type)) (sye index,ts))
        (*
        failwith "NOT IMPLEMENTED YET"
        *)

    | SYMDEF_struct _
    | SYMDEF_cstruct _
    | SYMDEF_nonconst_ctor _
      ->
(*
if name = "EInt" then begin
        print_endline ("lookup_name_in_table_dirs_with_sig finds struct constructor " ^ id);
        print_endline ("Argument types are " ^ catmap "," (sbt bsym_table) t2);
        print_endline ("Doing overload resolution");
end;
*)
        let ro =
          resolve_overload
          state bsym_table caller_env rs sra [index] name t2 ts
        in
          begin match ro with
          | Some (index,t,ret,mgu,ts) ->
            (*
            print_endline "handle_function (1)";
            *)
            let tb =
              handle_function
              inner_type_of_index_with_ts
              state
              bsym_table
              rs
              sra srn name ts index
            in
              Some tb
          | None -> 
(*
            if name = "EInt" then print_endline "Overload resolution failed";
*)
            None
          end
    | SYMDEF_newtype _
    | SYMDEF_abs _
    | SYMDEF_union _
    | SYMDEF_instance_type _
    | SYMDEF_type_alias _ ->

      (* recursively lookup using "_ctor_" ^ name :
         WARNING: we might find a constructor with the
         right name for a different cclass than this one,
         it isn't clear this is wrong though.
      *)
      (*
      print_endline "mapping type name to _ctor_type";
      *)
      lookup_name_in_table_dirs_with_sig
        state
        bsym_table
        table
        dirs
        caller_env env rs sra srn ("_ctor_" ^ name) ts t2

    | SYMDEF_const_ctor (_,t,_,_)
    | SYMDEF_const (_,t,_,_)
    | SYMDEF_once t
    | SYMDEF_var t
    | SYMDEF_ref t
    | SYMDEF_val t
    | SYMDEF_parameter (_,t)
      ->
(*
print_endline("Found var or param of type " ^ sbt bsym_table t);
*)
      let sign = try List.hd t2 with _ -> assert false in
      handle_variable 
        build_env
        bind_type'
        state bsym_table env rs (sye index) id srn ts t sign

    | _
      ->
        clierrx "[flx_bind/flx_lookup.ml:3448: E157] " sra
        (
          "[lookup_name_in_table_dirs_with_sig] Expected " ^id^
          " to be struct or variable of function type, got " ^
          string_of_symdef entry id vs
        )
    end
    end

  | FunctionEntry fs ->
if name = debugid then
    print_endline ("Lookup_name_in_table_dirs_with_sig Found function set size " ^ si (List.length fs));
    let ro =
      resolve_overload
      state bsym_table caller_env rs sra fs name t2 ts
    in
    match ro with
      | Some (index,t,ret,mgu,ts) ->
if name = debugid then
    begin
       print_endline ("Overload resolved to index " ^ si index);
       print_endline ("handle_function (3) ts=" ^ catmap "," (sbt bsym_table) ts);
    end;
        (*
        print_endline ("handle_function (3) ts=" ^ catmap "," (sbt bsym_table) ts);
        let ts = adjust_ts state.sym_table sra index ts in
        print_endline "Adjusted ts";
        *)
        let ((_,tt) as tb) =
          handle_function
          inner_type_of_index_with_ts
          state
          bsym_table
          rs
          sra srn name ts index
        in
          (*
          print_endline ("SUCCESS: overload chooses " ^ full_string_of_entry_kind state.sym_table (mkentry state.counter dfltvs index));
          print_endline ("Value of ts is " ^ catmap "," (sbt bsym_table) ts);
          print_endline ("Instantiated closure value is " ^ sbe bsym_table tb);
          print_endline ("type is " ^ sbt bsym_table tt);
          *)
          Some tb

      | None ->
(*
        if name = "EInt" then print_endline "Can't overload: Trying opens";
*)
        let opens : entry_set_t list =
          uniq_cat []
          (
            List.concat
            (
              List.map
              (fun table ->
                match Flx_name_lookup.lookup_name_in_htab table name with
                | Some x -> [x]
                | None -> []
              )
              dirs
            )
          )
        in
        (*
        print_endline (si (List.length opens) ^ " OPENS BUILT for " ^ name);
        *)
        match opens with
        | [NonFunctionEntry i] when
          (
            match get_data state.sym_table (sye i) with
            { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry }->
            (*
            print_endline ("FOUND " ^ id);
            *)
            match entry with
            | SYMDEF_abs _
            | SYMDEF_union _ -> true
            | _ -> false
           ) ->
            (*
            print_endline "mapping type name to _ctor_type2";
            *)
            lookup_name_in_table_dirs_with_sig
              state
              bsym_table
              table
              dirs
              caller_env env rs sra srn ("_ctor_" ^ name) ts t2
        | _ ->
        let fs =
          match opens with
          | [NonFunctionEntry i] -> [i]
          | [FunctionEntry ii] -> ii
          | _ ->
            Flx_name_lookup.merge_functions opens name
        in
          let ro =
            resolve_overload
            state bsym_table caller_env rs sra fs name t2 ts
          in
          (*
          print_endline "OVERLOAD RESOLVED .. ";
          *)
          match ro with
          | Some (result,t,ret,mgu,ts) ->
            (*
            print_endline "handle_function (4)";
            *)
            let tb =
              handle_function
              inner_type_of_index_with_ts
              state
              bsym_table
              rs
              sra srn name ts result
            in
              Some tb
          | None ->
            (*
            print_endline "FAILURE"; flush stdout;
            *)
            None


