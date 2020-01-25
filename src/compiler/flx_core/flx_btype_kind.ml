open Flx_btype
open Flx_exceptions
open Flx_kind

let rec metatype sr term : kind =
  let t = metatype' sr term in
  t

and metatype' sr typ : kind =
  let st t = Flx_btype.st t in
  let mt t = metatype' sr t in
  if not (Flx_btype.iscopyable_type typ) then kind_linear else
  match typ with
  | BBOOL _ -> KIND_bool
  | BTYP_typeop (_,_,k) -> k
  | BTYP_hole -> assert false
  | BTYP_uniq _ -> KIND_linear

  | BTYP_type_match (_, bs)
  | BTYP_subtype_match (_, bs) ->
    kind_max (List.map (fun (_,t) -> mt t) bs)
 
  | BTYP_type_function (a,r,body) ->
    let ps = List.map snd a in
    let argt =
      match ps with
      | [x] -> x
      | _ -> kind_tuple ps
    in
    let bk =  mt body in
    if kind_ge2 r bk then
      kind_function (argt,r)
    else 
      clierrx "[Flx_btype_kind:32: E239] " sr
        (
          "Flx_btype_kind: In type function \n" ^
          st typ ^
          "\nFunction body metatype \n"^
          sk bk^
          "\nis not subkind or equal to declared kind \n" ^
          sk r
        )

  | BTYP_type_tuple ts ->
    kind_tuple (List.map mt ts)

  (* this is a hack, but should be ok for now: the type of a map
     of a type function over a tuple is a tuple of the mapped types,
     which is an ordinary type.
  *)
  | BTYP_type_map (_,_) -> kind_type

  | BTYP_type_apply (a,b) ->
(*
print_endline ("Flx_btype_kind.metatype' case type_apply: " ^ Flx_btype.st typ);
*)
    begin
      let ta = mt a
      and tb = mt b
      in match ta with
      | KIND_function (x,y) ->
        if x <> tb then 
          clierrx "Flx_btype_kind:52: E240] " sr 
          (
            "Flx_btype_kind:Metatype error: type term " ^
             st typ ^
            "\nfunction argument wrong metatype, expected:\n" ^
            sk  x ^
            "\nbut got:\n" ^
            sk tb
          );
       y

      | _ -> 
        clierrx "[Flx_btype_kind:61: E241] " sr
        (
          "Flx_btype_kind:Metatype error: function required for LHS of application:\n"^
          st typ ^
          ", got metatype:\n" ^
          sk ta
        );
        KIND_type (* HACK *)
    end
  | BTYP_type_var (i,k) -> k
  | BTYP_vinst (index,ts,k) -> k
  | BTYP_inst (index,ts,k) -> k
  | BTYP_fix (i,k) -> k

  | BTYP_void
  | BTYP_unitsum _
  | BTYP_tuple [] -> kind_unitsum

  | BTYP_sum _
  | BTYP_array _
  | BTYP_tuple _ ->
    if islinear_type () typ then kind_compactlinear else kind_type 
    

  (* Ordinary type expressions *)
  | BTYP_typeof _
  | BTYP_cfunction _
  | BTYP_function _
  | BTYP_effector _
  | BTYP_linearfunction _
  | BTYP_lineareffector _
  | BTYP_ptr _
  | BTYP_variant _
  | BTYP_polyvariant _
  | BTYP_record _
  | BTYP_rev _

  | BTYP_label

  | BTYP_polyrecord (_, _, _)
  | BTYP_tuple_cons (_, _)
  | BTYP_tuple_snoc (_, _)
  | BTYP_rptsum _
    -> kind_type

  | BTYP_type_set _
  | BTYP_type_set_union _
  | BTYP_type_set_intersection _
    -> kind_type (* WRONG but lets see what happens ! *)

  | BTYP_none
    ->
    clierrx "[flx_btype_kind:180: E246] " sr ("No meta type for type-like term " ^ 
     st typ)



