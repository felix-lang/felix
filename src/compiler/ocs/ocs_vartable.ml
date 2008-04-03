(* Utility type for variable binding tables.  *)

module VarTable = Map.Make (String)

type 'a vartable = {
  vt_global : 'a vartable option;
  mutable vt_bindings : 'a VarTable.t
}

let vt_create () =
  { vt_global = None;
    vt_bindings = VarTable.empty }
;;

let vt_inherit vt =
  { vt_global =
      if vt.vt_global = None then Some vt
      else vt.vt_global;
    vt_bindings = vt.vt_bindings } 
;;

let vt_global vt =
  vt.vt_global = None
;;

let rec vt_copy vt vc =
  { vt_global =
      (match vt.vt_global with
	  Some t -> Some (vt_copy t vc)
	| _ -> None);
    vt_bindings =
      VarTable.map vc vt.vt_bindings }
;;

let var_insert vt key r =
  vt.vt_bindings <- VarTable.add key r vt.vt_bindings
;;

let rec var_find vt key =
  try
    Some (VarTable.find key vt.vt_bindings)
  with
    Not_found ->
      begin
	match vt.vt_global with
	  Some x -> var_find x key
	| _ -> None
      end
;;

let rec var_get vt key mkvar =
  try
    VarTable.find key vt.vt_bindings
  with
    Not_found ->
      begin
	match vt.vt_global with
	  Some x -> var_get x key mkvar
	| _ ->
	    let r = mkvar () in
	      var_insert vt key r;
	      r
      end
;;

