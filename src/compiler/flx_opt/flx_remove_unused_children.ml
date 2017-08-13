open Flx_bid
open Flx_mtypes2
open Flx_options
open Flx_print

let remove_unused_children syms uses bsym_table i =
  let desc = Flx_bsym_table.find_descendants bsym_table i in
  if desc <> BidSet.empty then begin
    (* all the descendants of a routine, excluding self *)
    (*
    print_endline "CANDIDATE FOR CHILD REMOVAL";
    print_function bsym_table i;
    print_endline ("Descendants of " ^ si i ^ " =" ^ BidSet.fold (fun j s -> s ^ " " ^ si j) desc "");
    BidSet.iter (fun i-> print_function bsym_table i) desc;
    *)


    (* everything used by this routine directly or indirectly *)
    let used = Flx_call.use_closure uses i in

    (*
    print_endline ("Usage closure of " ^ si i ^ " =" ^ BidSet.fold (fun j s -> s ^ " " ^ si j) used "");
    *)
    (* any desendants not used by this routine *)
    let unused_descendants = BidSet.diff desc used in

    (* remove the item *)
    BidSet.iter
    (fun i ->
      (* remove from symbol table, child map, and usage map *)
      Flx_bsym_table.remove bsym_table i;
      Hashtbl.remove uses i;
      if syms.compiler_options.print_flag then
        print_endline ("REMOVED CHILD SYMBOL " ^
          qualified_name_of_bindex bsym_table i ^ "=" ^ string_of_int i)
    )
    unused_descendants
  end


