open Flx_types
open Flx_bexe
open List
open Flx_util
open Flx_mtypes2
open Flx_print
open Flx_label
open Flx_bid

let rec find_label tail label =
  match tail with
  | [] -> None
  | BEXE_label (_,x) :: tail when x = label -> Some tail
  | _ :: tail -> find_label tail label

(* tell whether there is any reachable executable code here:
  if not, a previous call is a tail call
*)
let rec tailable exes exclude tail =
  let rec aux tail = match tail with
  | [] -> true
  | h :: t -> match h with
    | BEXE_proc_return _ -> true
    | BEXE_comment _
    | BEXE_label _
    | BEXE_nop _
      -> aux t
    | BEXE_goto (_,idx) ->
      if mem idx exclude then false (* infinite loop *)
      else
        begin match find_label exes idx with
        | None -> false
        | Some tail -> tailable exes (idx::exclude) tail
        end
    | BEXE_ifgoto (_,_,idx) ->
      if mem idx exclude then false (* infinite loop *)
      else
        begin match find_label exes idx with
        | None -> false
        | Some tail ->
          if tailable exes (idx::exclude) tail then
            tailable exes exclude t
          else false
        end
    | _ -> false
  in aux tail

let rec skip_white tail =
  match tail with
  | [] -> []
  | h :: t ->
    match h with
    | BEXE_comment _
    | BEXE_nop _  -> skip_white t
    | _ -> tail

let rec can_drop s tail : bool =
  match tail with
  | [] -> false
  | h :: t ->
    match h with
    | BEXE_comment _
    | BEXE_nop _  -> can_drop s t
    | BEXE_label (_,s') ->
      if s <> s' then can_drop s t
      else true

    | _ -> false

let rec retarget name ret exes exe exclude =
  match exe with
  | BEXE_goto (sr,label) ->
    (*
    print_endline ("Checking label " ^ label);
    *)
    begin match find_label exes label with
    | None -> exe
    | Some tail ->
      match skip_white tail with
      | [] ->
        begin match ret with
        | Flx_btype.BTYP_void ->
(*
          print_endline ("[Flx_cflow: goto] in "^name^" Retargetting " ^ string_of_int label ^ " to proc return");
*)
          bexe_proc_return sr
        | _ ->
(*
          print_endline ("[Flx_cflow: goto] in "^name^" Retargetting " ^ string_of_int label ^ " to halt");
*)
          bexe_halt (sr,"Jump to end of function");
        end

      | h :: t ->
        match h with
        | BEXE_proc_return _ ->
          (*
          print_endline ("[goto] Retargetting " ^ label ^ " to return");
          *)
          h
        | BEXE_goto (_,idx) ->
          (*
          print_endline ("[goto] Retargetting " ^ label ^ " to " ^ s);
          *)
          if mem idx exclude then bexe_halt (sr,"infinite loop")
          else retarget name ret exes h (idx::exclude)
        | BEXE_label (_,idx) ->
          (*
          print_endline ("[goto] Retargetting " ^ label ^ " to " ^ s);
          *)
          retarget name ret exes (bexe_goto (sr,idx)) exclude

        | _ -> exe
    end

  | BEXE_ifgoto (sr,e,label) ->
    (*
    print_endline ("Checking label " ^ label);
    *)
    begin match find_label exes label with
    | None -> exe
    | Some tail ->
      match skip_white tail with
      | [] -> exe
      | h :: t ->
        match h with
        | BEXE_goto (_,idx) ->
          (*
          print_endline ("[ifgoto] Retargetting " ^ label ^ " to " ^ s);
          *)
          if mem idx exclude then bexe_halt (sr,"infinite loop")
          else retarget name ret exes (bexe_ifgoto (sr,e,idx)) (idx::exclude)
        | BEXE_label (_,idx) ->
          (*
          print_endline ("[ifgoto] Retargetting " ^ label ^ " to " ^ s);
          *)
          retarget name ret exes (bexe_ifgoto (sr,e,idx)) (idx::exclude)
        | _ -> exe
    end

  | _ -> exe

let chain_gotos' name ret exes =
  let rec aux tail out =
    match tail with
    | [] -> rev out
    | h :: t ->
      let h = retarget name ret exes h [] in
      aux t (h :: out)
  in aux exes []

let fix_dropthrus syms exes =
  let rec aux tail out =
    match tail with
    | [] -> rev out
    |
    ( BEXE_goto (_,s)
    | BEXE_ifgoto (_,_,s)
    ) as h :: t ->
      if can_drop s t
      then aux t out
      else aux t (h::out)
    | h::t -> aux t (h::out)
  in aux exes []

let chain_gotos syms name ret exes =
  let exes = chain_gotos' name ret exes in
  fix_dropthrus syms exes

(* this procedure converts tail calls into jumps, it is ONLY
intended to be used temporarily whilst the inlining code
can't handle jump instruction
*)

let final_tailcall_opt exes =
  let rec aux inp out = match inp with
    | [] -> rev out
    | BEXE_call_direct (sr,i,ts,a) :: tail
      when tailable exes [] tail
      -> aux tail (bexe_jump_direct (sr,i,ts,a) :: out)
    | BEXE_call (sr,a,b) :: tail
      when tailable exes [] tail
      -> aux tail (bexe_jump (sr,a,b) :: out)
    | head :: tail -> aux tail (head :: out)
  in aux exes []

