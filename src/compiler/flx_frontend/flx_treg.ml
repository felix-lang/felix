open Flx_types
open Flx_mtypes2
open Flx_typing
open Flx_unify
open Flx_print
open Flx_exceptions
open Flx_util
open Flx_list
open Flx_srcref
open List
open Flx_maps
open Flx_beta

let register_type_nr syms t =
  (*
  let t' = Flx_maps.reduce_type t in
  if t <> t' then print_endline ("UNREDUCED TYPE! " ^ sbt syms.dfns t ^ " <> " ^ sbt syms.dfns t');
  *)
  match t with
  | `BTYP_fix _
  | `BTYP_tuple []
    -> ()
  | _
    ->
    let t = lstrip syms.dfns (fold syms.counter syms.dfns t) in
    if not (Hashtbl.mem syms.registry t)
    then begin
      let () = check_recursion t in
      let n = !(syms.counter) in
      incr syms.counter;
      if syms.compiler_options.print_flag then
      print_endline ("//Register type " ^ si n ^ ": " ^ string_of_btypecode syms.dfns t);
      Hashtbl.add syms.registry t n
    end

let register_tuple syms t =
  let t = lstrip syms.dfns (fold syms.counter syms.dfns t) in
  match t with
  | `BTYP_tuple [] -> ()
  | `BTYP_tuple [_] -> assert false

  | `BTYP_tuple ts ->
    let t = `BTYP_tuple (map reduce_type ts) in
    register_type_nr syms t

  | `BTYP_array (t',`BTYP_unitsum n) ->
    let t' = reduce_type t' in
    let ts = rev_map (fun _ -> t') (nlist n) in
    register_type_nr syms (`BTYP_tuple ts)

  | `BTYP_record ts ->
    let t = reduce_type t in
    begin match t with
    | `BTYP_tuple [] -> ()
    | _ -> register_type_nr syms t
    end

  | `BTYP_variant ts ->
    let t = reduce_type t in
    begin match t with
    | `BTYP_void -> ()
    | _ -> register_type_nr syms t
    end

  | _ -> assert false

let rec register_type_r ui syms bbdfns exclude sr t =
  let t = reduce_type (beta_reduce syms sr (lower (lstrip syms.dfns t))) in
  (*
  let sp = String.make (length exclude * 2) ' ' in
  print_endline (sp ^ "Register type " ^ string_of_btypecode syms.dfns t);
  if (mem t exclude) then print_endline (sp ^ "Excluded ..");
  *)
  if not (Hashtbl.mem syms.registry t) then
  if not (mem t exclude) then
  let rr t' = register_type_r ui syms bbdfns (t :: exclude) sr t' in
  let rnr t = register_type_nr syms t in
  let t' = unfold syms.dfns t in
  (*
  print_endline (sp ^ "Unfolded type " ^ string_of_btypecode syms.dfns t');
  *)
  match t' with
  | `BTYP_void -> ()
  | `BTYP_fix i -> clierr sr ("[register_type_r] Fixpoint "^si i^" encountered")
  | `BTYP_var (i,mt) -> clierr sr ("Attempt to register type variable " ^ si i ^":"^sbt syms.dfns mt)
  | `BTYP_function (ps,ret) ->
    let ps = match ps with
    | `BTYP_void -> `BTYP_tuple []
    | x -> x
    in
    rr ps; rr ret; rnr (`BTYP_function (ps,ret))

  | `BTYP_cfunction (ps,ret) -> rr ps; rr ret; rnr t

  | `BTYP_array (ps,ret) ->
    begin match ret with
    | `BTYP_unitsum 0 | `BTYP_void -> syserr sr "Unexpected array length 0"
    | `BTYP_unitsum 1 | `BTYP_tuple [] -> syserr sr "Unexpected array length 1"
    | `BTYP_unitsum _ ->
      rr ps; rr ret; rnr t
    | _ -> syserr sr "Array bound must be unitsum"
    end

  | `BTYP_tuple ps -> iter rr ps; rnr t

  | `BTYP_record ps -> iter (fun (s,t)->rr t) ps; rnr t
  | `BTYP_variant ps -> iter (fun (s,t)->rr t) ps; rnr t

  | `BTYP_sum ps ->
    (* iter rr ps; *) (* should be driven by constructors *)
    rnr t

  | `BTYP_unitsum k -> rnr t

  (* NOTE: pointer type is registered before the type it points
    to because it can be incomplete, whereas the type it
    points to may need a complete pointer type: this
    is always the case for recursion under a pointer
  *)

  (* pointer type is no longer registered, just us t* *)

  | `BTYP_pointer t' -> rr t'

  | `BTYP_inst (i,ts)->
    iter rr ts;

    let id, parent, sr,entry =
      try Hashtbl.find bbdfns i
      with Not_found ->
        try match Hashtbl.find syms.dfns i with
        { id=id; sr=sr; parent=parent; symdef=entry } ->
        clierr sr
        (
          "register_type_r Can't find " ^
          id ^ "[" ^ si i ^ "]" ^
          " in fully bound symbol table: " ^
          short_string_of_src sr
        )
        with Not_found -> failwith ("[register_type_r] Can't find index " ^ si i)
    in
    begin match entry with

    | `BBDCL_newtype (_,r) ->
      rr r;
      rnr t

    | `BBDCL_union (vs,cs) ->
      (*
      let cts = map snd cs in
      let cts = map (tsubst vs ts) cts in
      iter rr cts;
      *)
      rnr t

    | `BBDCL_class vs -> rnr t

    | `BBDCL_cclass (vs,cs) ->
       let cts = map (function
         | `BMemberVal (_,t)
         | `BMemberVar (_,t)
         | `BMemberCtor (_,t) -> t,[]
         | `BMemberFun (_,mvs,t)
         | `BMemberProc (_,mvs,t) -> t,mvs
         ) cs
       in
      (* I THINK THIS IS WRONG NOW .. only register
         the interface if it is used .. we need
         the method instance type variables too!
      *)
      (*
      let cts = map (fun (t,ts') -> tsubst vs ts t) cts in
      iter rr cts;
      *)
      rnr t
      (* NO CONSTRUCTOR! *)

    | `BBDCL_cstruct (vs,cs)
    | `BBDCL_struct (vs,cs) ->
      let cts = map snd cs in
      let cts = map (tsubst vs ts) cts in
      iter rr cts;

      (* HACKERY HERE... We should NOT need to register
        constructors, etc, unless they're actually used
      *)
      (*
      if length cts > 1 then rnr (`BTYP_tuple cts);
      *)

      (*
      let argt = typeoflist cts in
      rnr argt;                             (* argument tuple *)
      *)
      rnr t;
      (*
      rnr (`BTYP_function (argt,t))         (* constructor as function *)
      *)

    | `BBDCL_abs _ -> ui i ts; rnr t  (* instantiate the type too *)

    | _ ->
      clierr sr
      (
        "[register_type_r] expected type declaration, got " ^
        string_of_bbdcl syms.dfns bbdfns entry i
      )
    end

  | _ ->  ()
    (*
    clierr sr
    (
      "Unexpected kind in register type: " ^
      string_of_btypecode syms.dfns t
    )
    *)
