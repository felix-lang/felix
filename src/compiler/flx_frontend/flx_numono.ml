(* New monomorphisation routine *)
open Flx_util
open Flx_btype
open Map
open Flx_mtypes2
open Flx_print
open Flx_types
open Flx_bbdcl
open Flx_bexpr
open Flx_bexe
open Flx_bparameter

type symkind = Felix of int | External

module MonoMap = Map.Make (
  struct 
    type t = int * Flx_btype.t list 
    let compare = compare 
  end
)

let find_inst syms processed to_process i ts =
  try 
    MonoMap.find (i,ts) !processed
  with Not_found ->
  try
    MonoMap.find (i,ts) !to_process
  with Not_found ->
    let k = fresh_bid syms.counter in
    let target = Felix k in
    to_process := MonoMap.add (i,ts) target !to_process;
    target 

let find_felix_inst syms processed to_process i ts : int =
  match find_inst syms processed to_process i ts with
  | Felix k -> k
  | _ -> assert false
 

let fixup_type' syms bsym_table fi t =
  match t with
  | BTYP_inst (i,ts) ->
    (* typeclass fixup *)
    let i,ts = fi i ts in
    let t = btyp_inst (i,ts) in
    t
  | x -> x

let rec fixup_type syms bsym_table fi t =
  let f_btype t = fixup_type syms bsym_table fi t in
  let f_btype' t = fixup_type' syms bsym_table fi t in
  let t = Flx_btype.map ~f_btype t in
  f_btype' t

let fixup_expr' syms bsym_table fi mt (e,t) =
  (*
  print_endline ("FIXUP EXPR(up) " ^ sbe sym_table (e, btyp_void ()));
  *)
  let x = match e with
  | BEXPR_apply_prim (i',ts,a) ->
    let i,ts = fi i' ts in
    if i = i' then
      bexpr_apply_prim t (i,ts,a)
    else
      bexpr_apply_direct t (i,ts,a)

  | BEXPR_apply_direct (i,ts,a) ->
    let i,ts = fi i ts in
    bexpr_apply_direct t (i,ts,a)

  | BEXPR_apply_struct (i,ts,a) ->
    let i,ts = fi i ts in
    bexpr_apply_struct t (i,ts,a)

  | BEXPR_apply_stack (i,ts,a) ->
    let i,ts = fi i ts in
    bexpr_apply_stack t (i,ts,a)

  | BEXPR_ref (i,ts)  ->
    let i,ts = fi i ts in
    bexpr_ref t (i,ts)

  | BEXPR_name (i',ts') ->
    let i,ts = fi i' ts' in
    (*
    print_endline (
      "Ref to Variable " ^ si i' ^ "[" ^ catmap "," (sbt bsym_table) ts' ^"]" ^
      " mapped to " ^ si i ^ "[" ^ catmap "," (sbt bsym_table) ts ^"]"
    );
    *)
    bexpr_name t (i,ts)

  | BEXPR_closure (i,ts) ->
    let i,ts = fi i ts in
    bexpr_closure t (i,ts)

  | x -> x, t
  in
  (*
  print_endline ("FIXed UP EXPR " ^ sbe sym_table (x, btyp_void ()));
  *)
  x

let rec fixup_expr syms bsym_table fi mt e =
  (*
  print_endline ("FIXUP EXPR(down) " ^ sbe sym_table e);
  *)
  let f_bexpr e = fixup_expr syms bsym_table fi mt e in
  let f_bexpr' e = fixup_expr' syms bsym_table fi mt e in
  let e = Flx_bexpr.map ~f_btype:mt ~f_bexpr e in
  f_bexpr' e

let fixup_exe syms bsym_table fi mt exe =
  (*
  print_endline ("FIXUP EXE[In] =" ^ string_of_bexe sym_table 0 exe);
  *)
  let f_bexpr e = fixup_expr syms bsym_table fi mt e in
  let result =
  match Flx_bexe.map ~f_btype:mt ~f_bexpr exe with
  | BEXE_call_direct (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    bexe_call_direct (sr,i,ts,a)
    *)

  | BEXE_jump_direct (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    bexe_jump_direct (sr,i,ts,a)
    *)

  | BEXE_call_prim (sr, i',ts,a) -> assert false
    (*
    let i,ts = fi i' ts in
    if i = i' then
      bexe_call_prim (sr,i,ts,a)
    else
      bexe_call_direct (sr,i,ts,a)
    *)

  | BEXE_call_stack (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    bexe_call_stack (sr,i,ts,a)
    *)

  (* this is deviant case: implied ts is vs of parent! *)
  | BEXE_init (sr,i,e) ->
    (*
    print_endline ("[init] Deviant case variable " ^ si i);
    *)
    let vs = Flx_bsym_table.find_bvs bsym_table i in
    let ts = List.map (fun (s,j) -> mt (btyp_type_var (j, btyp_type 0))) vs in
    let i,ts = fi i ts in
    (*
    print_endline ("[init] Remapped deviant variable to " ^ si i);
    *)
    bexe_init (sr,i,e)

  | BEXE_svc (sr,i) ->
    (*
    print_endline ("[svc] Deviant case variable " ^ si i);
    *)
    let vs = Flx_bsym_table.find_bvs bsym_table i in
    let ts = List.map (fun (s,j) -> mt (btyp_type_var (j, btyp_type 0))) vs in
    let i,ts = fi i ts in
    (*
    print_endline ("[svc] Remapped deviant variable to " ^ si i);
    *)
    bexe_svc (sr,i)

  | x -> x
  in
  (*
  print_endline ("FIXUP EXE[Out]=" ^ string_of_bexe sym_table 0 result);
  *)
  result


let fixup_exes syms bsym_table fi mt exes =
  List.map (fixup_exe syms bsym_table fi mt) exes

let rec mono_element debug syms to_process processed bsym_table nutab i ts j =
  print_endline ("numono: " ^ si i ^ "[" ^ catmap "," (sbt bsym_table) ts ^"] -> " ^ si j);
  if i = j then begin
    assert (List.length ts = 0);
    if debug then print_endline "   Already monomorphic"; 
  end 
  else
  begin
    if debug then print_endline "  Add new monormophic symbol (not actually monomorphised yet!)"; 
    let parent,sym = Flx_bsym_table.find_with_parent bsym_table i in
    let {Flx_bsym.id=id;sr=sr;vs=vs;bbdcl=bbdcl} = sym in
    if debug then print_endline ("  Symbol " ^ id);
    let parent = match parent with
    | None -> None
    | Some p -> 
      let pvs = Flx_bsym_table.find_bvs bsym_table p in
      let n = List.length pvs in
      let pts = Flx_list.list_prefix ts n in 
      Some (find_felix_inst syms processed to_process p pts)
    in
    begin match bbdcl with
    | BBDCL_fun _ -> print_endline "  Function"
    | _ -> print_endline "Dunno what it is"
    end
    ;
    Flx_bsym_table.add nutab j parent sym
  end


let monomorphise2 debug syms bsym_table =
  let roots: BidSet.t = !(syms.roots) in
  assert (BidSet.cardinal roots > 0);


  (* to_process is the set of symbols yet to be scanned
     searching for symbols to monomorphise
  *)
  let to_process = ref MonoMap.empty in
  BidSet.iter (fun i -> to_process := MonoMap.add (i,[]) (Felix i) (!to_process)) roots;
  
  let processed = ref MonoMap.empty in

  (* new bsym_table *)
  let nutab = Flx_bsym_table.create () in
 
  while not (MonoMap.is_empty (!to_process)) do
    let (i,ts),target = MonoMap.choose (!to_process) in
    assert (not (MonoMap.mem (i,ts) (!processed) ));

    to_process := MonoMap.remove (i,ts) (!to_process);
    processed := MonoMap.add (i,ts) target (!processed);

    match target with 
    | External ->
      if debug then print_endline ("External target, leave polymorphic " ^ si i);
      let parent,sym = Flx_bsym_table.find_with_parent bsym_table i in
      assert (parent = None);
      Flx_bsym_table.add nutab i parent sym

    | Felix j ->
      assert (List.length ts > 0 || match target with Felix i -> i == j | _ -> true);
      assert (not (Flx_bsym_table.mem nutab j));
      mono_element debug syms to_process processed bsym_table nutab i ts j;
  done
  ;

  nutab

