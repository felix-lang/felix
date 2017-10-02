open Flx_bbdcl
open Flx_mtypes2
open Flx_exceptions
open Flx_print
open Flx_bid
open Flx_btype

let catmap x = Flx_util.catmap x
let si i = string_of_int i

let debug = false

(* make recursive later *)
let rec remap_virtual_types syms bsym_table (* tc *) t =
   let f_btype t = remap_virtual_types syms bsym_table (* tc *) t in
   let t = Flx_btype.map ~f_btype t in
(*
print_endline ("Remap virtual types " ^ Flx_btype.st t);
*)
  match t with
  | BTYP_inst (i,ts) ->
    begin
      let parent,bsym = Flx_bsym_table.find_with_parent bsym_table i in
      let maybe_tc =
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_virtual_type _ -> 
          if debug then print_endline ("Remap: virtual type!");
          begin match parent with
          | None -> if debug then print_endline ("Remap: no parent, can't be virtual type"); None
          | Some tc' ->
            let pbsym = Flx_bsym_table.find bsym_table tc' in
            begin match Flx_bsym.bbdcl pbsym with
            | BBDCL_typeclass _ -> 
              if debug then print_endline ("Remap: Ok, found type class, so virtual type");
              Some tc'
            | _ -> 
              print_endline ("Remap: Wooops, virtual type doesn't have type class parent");
              assert false
            end
          end (* parent *)
        | _ -> None
        end
      in
(*
      let tckid = match parent with | Some tc' -> tc = tc' | None -> false in
      if tckid then begin
*)
      begin match maybe_tc with
      | None -> t
      | Some tc ->
        if debug then print_endline ("Remap Found type class kid type " ^ Flx_bsym.id bsym ^ "<" ^si i ^ ">" ^
           "["^catmap "," (sbt bsym_table) ts ^"]" );
        let entries = try Hashtbl.find syms.virtual_to_instances i with Not_found -> [] in
        if debug then print_endline ("instances of virtual type are:");
        if debug then List.iter (fun (inst_vs, inst_constraint, inst_ts, inst_idx)  ->
          print_endline ("     .. " ^ si inst_idx ^ 
          " inst_vs=" ^ catmap "," (fun (s,vidx) -> s) inst_vs ^
          " inst_ts=" ^ catmap "," (sbt bsym_table) inst_ts);
        )
        entries;
        let (inst_vs, inst_constraint, inst_ts, inst_idx) =
          match entries with
          | [entry] -> entry
          | _ -> print_endline ("Woops, didn't find unique instance of type class virtual type"); assert false
        in
          if debug then print_endline ("Length inst_vs = " ^ si (List.length inst_vs) ^ 
             " length ts = " ^ si (List.length ts));
(*
        assert (List.length inst_vs = List.length ts);
*)
        let inst_bsym = Flx_bsym_table.find bsym_table inst_idx in
        begin match Flx_bsym.bbdcl inst_bsym with
        | BBDCL_instance_type (bvs,repr) ->
          if debug then print_endline ("Found instance type, repr= " ^ sbt bsym_table repr);
          if debug then print_endline ("  instance type bvs = " ^catmap "," (fun (s,vidx) -> s) bvs);
          if debug then print_endline ("Length bvs = " ^ si (List.length bvs) ^ 
             " length inst_ts = " ^ si (List.length inst_ts));
(*
          assert (List.length bvs = List.length inst_ts);
*)
(* SEE THE CALCULATION FOR THE SUBSTITUTION REQUIRED IN sigmatch *)
          repr
        | _ -> 
          print_endline ("Woops, expected to find instance type for type class virtual type");
          assert false
        end (* instance_type *)
      end (* maybe tc *)
    end (* nominal type *)

  | _ -> t


