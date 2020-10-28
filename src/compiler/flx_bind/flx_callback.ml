open Flx_btype

let c_callback_type sr name ts_orig ret =
  let client_data_pos = ref (-1) in

  (* type of callback C expects *)
  let ts_c =
    List.map
    (function
      | `TYP_name (_,id,[]) when id = name ->
        `TYP_name (sr, "address", []) 
      | t -> t
    )
    ts_orig
  in
  let domain = match ts_c with
    | [x] -> x
    | x -> `TYP_tuple x
  in
  `TYP_cfunction (domain, ret)

let felix_callback_type sr name ts_orig ret =
  (* type of Felix callback passed to thunk *)
  let ts_f =
      List.filter
      (function
        | `TYP_name (_,id,[]) when id = name -> false
        | t -> true
      )
      ts_orig
  in
  let domain = match ts_f with
    | [x] -> x
    | x -> `TYP_tuple x
  in
  `TYP_function (domain, ret)

let felix_thunk_type sr name ts_orig ret =
  (* type of Felix callback passed to thunk *)
  let fback = felix_callback_type sr name ts_orig ret in

  let ts_f =
      List.map
      (function
        | `TYP_name (_,id,[]) when id = name -> fback
        | t -> t
      )
      ts_orig
  in
  let domain = match ts_f with
    | [x] -> x
    | x -> `TYP_tuple x
  in
  `TYP_cfunction (domain, ret)

let cal_callback_types bsym_table bt syscounter sr name ts_orig ret =
  let bret = bt ret in
  let client_data_pos = ref (-1) in

  (* type of callback C expects *)
  let ts_c =
    let counter = ref 0 in
    List.map
    (function
      | `TYP_name (_,id,[]) when id = name ->
        if !client_data_pos = -1 then
          client_data_pos := !counter
        ;
        let address = `TYP_name (sr, "address", []) in
        bt address
      | t -> incr counter; bt t
    )
    ts_orig
  in

  (* The type of the arguments of the Felix callback function,
    which are the same as the C function, but with the client
    data pointer dropped. Note this is the type of the arguments
    PASSED to the callback thunk.
  *)
  let ts_f =
    List.map bt
    (
      List.filter
      (function
        | `TYP_name (_,id,[]) when id = name -> false
        | t -> true
      )
      ts_orig
    )
  in

  (* The of the Felix function PASSED to the callback thunk *)
  let tf_args = match ts_f with
    | [x] -> x
    | lst -> btyp_tuple lst
  in
  let tf = btyp_function (tf_args, bret) in

  (* The type of the arguments Felix thinks the raw
     C function has on a call. A closure of this
     function is a Felix function .. NOT the raw
     C function.
  *)
  let ts_cf =
    List.map
    (function
      | `TYP_name (_,id,[]) when id = name -> tf
      | t -> bt t
    )
    ts_orig
  in

  (* type of the callback thunk Felix expects *)
  let tcf = btyp_cfunction (btyp_tuple ts_cf, bret) in
  let tcf = Flx_fold.fold bsym_table syscounter tcf in

  !client_data_pos,
  bret,  (* return type *)
  ts_c,  (* arguments to C callback thunk including void* *)
  ts_cf, (* arguments to C callback thunk expected by Felix *) 
  tf,    (* Type of the Felix function to be PASSED to the callback thunk *)
  tcf    (* Type of the callback thunk expected by Felix *)


