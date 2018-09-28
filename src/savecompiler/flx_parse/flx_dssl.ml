(** Domain Specific Sub-Language, or DSSL representation *)
open Flx_drules
open Flx_token


(* move this to elsewhere eventually *)

(** Calculate the dssls that need to be loaded based on those already loaded.
That is; the transiive closure. *)
let tran_cls_dssls drules installed_dssls dssls =
  (* Calculate the transitive closure of DSSL's required. *)
  let rec cal_install (visited, to_install) dssl =
    (* Ignore dssls we've already visited. *)
    if DStringSet.mem dssl visited then (visited, to_install) else
    let visited = DStringSet.add dssl visited in

    let drule =
      try Drules.find dssl drules
      with Not_found -> failwith ("Can't open syntax " ^ dssl)
    in

    (* Make sure we install all of the dssl's dependencies first. *)
    let visited, to_install = List.fold_left
      cal_install
      (visited, to_install)
      drule.deps
    in

    visited, dssl :: to_install
  in

  (* Finally, calculate the dssls we need to install. We'll consider all the
   * installed dssls as already visited. *)
  let _, to_install = List.fold_left
    cal_install
    (string_set_of_list installed_dssls, [])
    dssls
  in

  (* We got our list backwards. *)
  let to_install = List.rev to_install in

  (* print_endline ("Installing " ^ cat "," to_install); *)
  to_install

(** Extract rules and priority scheme from the dssls which are to be
 installed. *)
let extract_syntax to_install dssls =
  List.fold_left
    begin fun (acc_rules, acc_prios) dssl ->
      let rules, prios =
        let d =
          try Drules.find dssl dssls
          with Not_found -> failwith ("Can't open syntax " ^ dssl)
        in
        d.rules, d.prios
      in
      let rules = List.map (fun x -> dssl,x) rules in
      acc_rules @ rules, acc_prios @ prios
    end
    ([],[])
    to_install


(** Create the bindings from all the rules. *)
let bind_grammar_rules dyp (rules:(string * rule_t) list) =
  let bindings = ref [] in
  let add_rules = ref [] in
  List.iter
    begin fun rule ->
      match Flx_export_syntax.extend_grammar dyp rule with
      | (rule, action, binding) ->
        bindings := binding :: !bindings;
        add_rules := (rule,action) :: !add_rules
    end
    rules
  ;
  !add_rules, !bindings


(** Run all the scheme commands for a rule.
[load_scheme_defs env scm] runs each of the scheme commands
in the list [scm] in the environment [env]. Typically used to
add definitions to the environment.
@param env the scheme environment 
@param scm the scheme code to run
*)
let load_scheme_defs env scm =
  (*
  print_endline ("Doing scheme .. " ^ string_of_int (List.length scm) ^ " fragments");
  *)
  List.iter begin fun (sr,s) ->
    ignore (
      (* print_endline ("Scheme: " ^ s); *)
       try Flx_ocs_run.scheme_run sr env s
       with Ocs_error.Error err | Ocs_error.ErrorL (_,err) ->
         print_endline ("Error "^err^" executing " ^s);
         failwith "Error executing SCHEME"
     )
  end scm
  (*
  ; print_endline "Scheme Done .. "
  *)



