(* New monomorphisation routine *)
open Flx_util
open Flx_btype
open Map
open Flx_mtypes2
open Flx_print
open Flx_types
open Flx_bbdcl

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

