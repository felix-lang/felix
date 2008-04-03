(* Continuations *)

open Ocs_types
open Ocs_error
open Ocs_eval
open Ocs_env
open Ocs_misc

let rec find_depth fdx tdx al bl =
  match (fdx, tdx) with
    (Some f, Some t) ->
      if f.dynext_parent == t.dynext_parent then
	(List.rev (f.dynext_after::al), t.dynext_before::bl)
      else if f.dynext_depth > t.dynext_depth then
	find_depth f.dynext_parent tdx (f.dynext_after::al) bl
      else if f.dynext_depth < t.dynext_depth then
	find_depth fdx t.dynext_parent al (t.dynext_before::bl)
      else
	find_depth f.dynext_parent t.dynext_parent
	  (f.dynext_after::al) (t.dynext_before::bl)
  | (Some f, None) ->
      find_depth f.dynext_parent tdx (f.dynext_after::al) bl
  | (None, Some t) ->
      find_depth fdx t.dynext_parent al (t.dynext_before::bl)
  | _ -> (List.rev al, bl)
;;

(* Change from the dynamic extent fdx to the dynamic extent tdx *)
let dxswitch fdx tdx cont =
  if fdx == tdx then
    cont ()
  else
    let (al, bl) = find_depth fdx tdx [] [] in
    let rec bloop =
      function
	[] -> cont ()
      | h::t -> eval (fst h) (fun _ -> bloop t) (snd h)
    in
      let rec aloop =
	function
	  [] -> bloop bl
	| h::t -> eval (fst h) (fun _ -> aloop t) (snd h)
      in
	aloop al
;;

let continuation dx cc th _ =
  function
    [| x |] -> dxswitch th.th_dynext dx (fun () -> cc x)
  | av -> dxswitch th.th_dynext dx (fun () -> cc (Svalues av))
;;

let call_cc th cc =
  function
    [| proc |] ->
      let cont =
	Sprim { prim_fun = Pfcn (continuation th.th_dynext cc);
	        prim_name = "<continuation>" }
      in
	eval th cc (Capply1 (Cval proc, Cval cont))
  | _ -> raise (Error "call/cc: bad args")
;;

let values =
  function
    [| x |] -> x
  | av -> Svalues av
;;

let call_values th cc =
  function
    [| producer; consumer |] ->
      eval th
	(function
	  Svalues av ->
	    eval th cc (mkapply (Cval consumer)
	                        (Array.map (fun x -> Cval x) av))
	| x -> eval th cc (Capply1 (Cval consumer, Cval x)))
	(Capply0 (Cval producer))
  | _ -> raise (Error "call-with-values: bad args")
;;

let dynamic_wind th cc =
  function
    [| before; thunk; after |] ->
      let before = Capply0 (Cval before)
      and after = Capply0 (Cval after) in
      let ndx = {
	dynext_parent = th.th_dynext;
	dynext_depth =
	  (match th.th_dynext with
	    None -> 0
	  | Some dx -> dx.dynext_depth + 1);
	dynext_before = (th, before);
	dynext_after = (th, after)
      } in
	eval th
	  (fun _ ->
	    eval { th with th_dynext = Some ndx }
	      (fun r ->
		eval th (fun _ -> cc r) after) (Capply0 (Cval thunk))) before
  | _ -> raise (Error "dynamic-wind: bad args")
;;

let init e =
  set_pfcn e call_cc "call-with-current-continuation";
  set_pfcn e call_cc "call/cc";

  set_pfn e values "values";

  set_pfcn e call_values "call-with-values";
  set_pfcn e dynamic_wind "dynamic-wind";
;;

