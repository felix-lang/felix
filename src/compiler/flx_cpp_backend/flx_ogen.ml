open Flx_util
open Flx_list
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bbdcl
open Flx_mtypes2
open Flx_name
open Flx_unify
open Flx_typing
open List
open Flx_print
open Flx_exceptions
open Flx_maps

let find_thread_vars_with_type bsym_table =
  let vars = ref [] in
  Flx_bsym_table.iter begin fun k bsym ->
    let bsym_parent = Flx_bsym_table.find_parent bsym_table k in
    match bsym_parent, Flx_bsym.bbdcl bsym with
    | None,BBDCL_var (_,t)
    | None,BBDCL_val (_,t)
      -> vars := (k,t) :: !vars
    | None,BBDCL_ref (_,t)
      -> vars := (k, btyp_pointer t) :: !vars

    | _ -> ()
  end bsym_table;
  !vars


let find_references syms bsym_table index ts =
  let children =
    try
      Flx_bsym_table.find_children bsym_table index
    with Not_found -> Flx_types.BidSet.empty
  in
  let references = ref [] in

  Flx_types.BidSet.iter begin fun idx ->
    try
      let bsym = Flx_bsym_table.find bsym_table idx in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_var (vs,t)
      | BBDCL_ref (vs,t)
      | BBDCL_val (vs,t)
        ->
        if length ts <> length vs then
        failwith
        (
          "[find_references] entry " ^ string_of_bid index ^
          ", child " ^ Flx_bsym.id bsym ^ "<" ^ string_of_bid idx ^ ">" ^
          ", wrong number of args, expected vs = " ^
          si (length vs) ^
          ", got ts=" ^
          si (length ts)
        );
        let t = tsubst vs ts t in
        references := (idx,t) :: !references
      | _ -> ()
    with Not_found -> ()
  end children;

  !references

let comma_sub s =
  let rec aux l r =
    try   (* note: breaks tail recursion optimisation *)
      let i = String.index r ',' in
      let n = String.length r in
      aux (l ^ String.sub r 0 i ^ " comma ") (String.sub r (i+1) (n-i-1))
    with Not_found -> l ^ r
  in
  aux "" s

(* this code handles pointers in types *)
let rec get_offsets' syms bsym_table typ : string list =
  let tname = cpp_typename syms bsym_table typ in
  let t' = unfold typ in
  match t' with
  | BTYP_pointer t -> ["0"]
    (*
    ["offsetof("^tname^",frame)"]
    *)

  | BTYP_sum args when not (all_units args) ->
    ["offsetof("^tname^",data)"]

  (* need to fix the rule for optimisation here .. *)
  | BTYP_variant _ ->
    ["offsetof("^tname^",data)"]

  | BTYP_inst (i,ts) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i
      with Not_found -> failwith
        ("get_offsets'] can't find index " ^ string_of_bid i)
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (vs,idts) ->
      let varmap = mk_varmap vs ts in
      let cpts = map (fun (_,_,t) -> varmap_subst varmap t) idts in
      if all_voids cpts then []
      else ["offsetof("^tname^",data)"]

    | BBDCL_struct (vs,idts) ->
      let varmap = mk_varmap vs ts in
      let n = ref 0 in
      let cpts = map (fun (s,t) -> s,varmap_subst varmap t) idts in
      let lst = ref [] in
      iter
      (fun (s,t) ->
        let prefix =
          "offsetof("^tname^","^s^")+"
        in
        iter
        (fun s -> lst := !lst @ [prefix ^ s])
        (get_offsets' syms bsym_table t)
      )
      cpts
      ;
      !lst

    | BBDCL_abs (vs,type_quals,_,_)
       when mem `GC_pointer type_quals -> ["0"]

    | _ -> []
    end

  | BTYP_array (t,BTYP_void) ->  []
  | BTYP_array (t,BTYP_unitsum k) ->
    let toffsets = get_offsets' syms bsym_table t in
    if toffsets = [] then [] else
    if k> 100 then
      failwith ("[get_offsets] Too many elements in array for shape, type " ^ sbt bsym_table t')
    else begin
      let eltype = cpp_typename syms bsym_table t in
      fold_left
      (fun result i ->
        let ss = "+" ^ si i ^ "*sizeof("^eltype^")" in
        fold_left
        (fun result s -> (s ^ ss) :: result)
        result
        toffsets
      )
      []
      (nlist k)
    end

  | BTYP_tuple args ->
    let n = ref 0 in
    let lst = ref [] in
    iter
    (fun t ->
      let prefix =
        "offsetof("^tname^",mem_"^si !n^")+"
      in
      iter
      (fun s -> lst := !lst @ [prefix ^ s])
      (get_offsets' syms bsym_table t)
      ;
      incr n
    )
    args
    ;
    !lst

  | BTYP_record args ->
    let lst = ref [] in
    iter
    (fun (s,t) ->
      let prefix =
        "offsetof("^tname^","^s^")+"
      in
      iter
      (fun s -> lst := !lst @ [prefix ^ s])
      (get_offsets' syms bsym_table t)
    )
    args
    ;
    !lst

  | BTYP_function _ -> ["0"]
  | BTYP_cfunction _ -> []

  | BTYP_unitsum _ -> []

  | BTYP_intersect _
    -> failwith "[ogen] Type intersection has no representation"

  (* this is a lie .. it does, namely a plain C union *)
  | BTYP_type_set _
    -> failwith "[ogen] Type set has no representation"

  | BTYP_sum _
  | BTYP_array _
  | BTYP_fix _
  | BTYP_void

  | BTYP_type  _
  | BTYP_type_var _
  | BTYP_type_apply _
  | BTYP_type_function _
  | BTYP_type_tuple _
  | BTYP_type_match _
  | BTYP_type_set_intersection _
  | BTYP_type_set_union _ -> assert false

let get_offsets syms bsym_table typ =
  map (fun s -> s^",") (get_offsets' syms bsym_table typ)

let gen_offset_data s n name offsets isfun props flags last_ptr_map =
  let this_ptr_map = name ^ "_ptr_map" in
  let old_ptr_map = !last_ptr_map in
  last_ptr_map := "&"^this_ptr_map;
  let noffsets =
    if isfun && mem `Requires_ptf props then si (n-1)^"+FLX_PASS_PTF"
    else si n
  in
  if n <> 0 then
  begin
    bcat s ("static std::size_t " ^ name ^
      "_offsets["^noffsets^ "]={\n");
    bcat s ("  " ^ cat "\n  " offsets);
    bcat s ("\n" ^  "};\n");
  end;
  bcat s ("FLX_FINALISER("^name^")\n");
  bcat s (  "static gc_shape_t "^ this_ptr_map ^" ={\n");
  bcat s ("  " ^ old_ptr_map ^ ",\n");
  bcat s ("  \"" ^ name ^ "\",\n");
  bcat s ("  1,sizeof("^name^"),\n  "^name^"_finaliser,\n");
  bcat s ("  "^noffsets^",\n  "^ (if n<>0 then name^"_offsets" else "0")^",\n  ");
  bcat s (match flags with None -> "gc_flags_default\n" | Some flags ->  flags^"\n");
  bcat s ( "};\n")

let is_instantiated syms i ts = Hashtbl.mem syms.instances (i,ts)

let gen_fun_offsets s syms bsym_table index vs ps ret ts instance props last_ptr_map : unit =
  let vars =  (find_references syms bsym_table index ts) in
  let vars = filter (fun (i, _) -> is_instantiated syms i ts) vars in
  let name = cpp_instance_name syms bsym_table index ts in
  let display = Flx_display.get_display_list bsym_table index in
  let offsets =
    (if mem `Requires_ptf props then
    ["FLX_EAT_PTF(offsetof(" ^ name ^ ",ptf)comma)"]
    else []
    )
    @
    (match ret with
      | BTYP_void -> [ ("offsetof(" ^ name ^ ",p_svc),");("offsetof(" ^ name ^ ",_caller),")    ]
      | _ -> []
    )
    @
    map
    (fun (didx, vslen) ->
    let dptr = "ptr" ^ cpp_instance_name syms bsym_table didx (list_prefix ts vslen) in
    "offsetof("^name^","^dptr^"),"
    )
    display
    @
    concat
    (
      map
      (fun (idx,typ)->
        let mem = cpp_instance_name syms bsym_table idx ts in
        let offsets = get_offsets syms bsym_table typ in
        map
        (fun offset ->
          "offsetof("^name^","^mem^")+" ^ offset
        )
        offsets
      )
      vars
    )
  in
  let n = length offsets in
  bcat s
  (
    "\n//OFFSETS for "^
    (match ret with BTYP_void -> "procedure " | _ -> "function ") ^
    name ^ "\n"
  );
  gen_offset_data s n name offsets true props None last_ptr_map
let gen_thread_frame_offsets s syms bsym_table last_ptr_map =
  let vars = find_thread_vars_with_type bsym_table in
  let ts = [] in
  let name = "thread_frame_t" in
  let offsets =
    concat
    (
      map
      (fun (idx,typ)->
        let mem = cpp_instance_name syms bsym_table idx ts in
        let offsets = get_offsets syms bsym_table typ in
        map
        (fun offset ->
          "offsetof("^name^","^mem^")+" ^ offset
        )
        offsets
      )
      vars
    )
  in
  let n = length offsets in
  bcat s
  (
    "\n//OFFSETS for "^ name ^ "\n"
  );
  gen_offset_data s n name offsets false [] (Some "gc_flags_immobile") last_ptr_map

let id x = ()

let scan_bexpr syms bsym_table allocable_types e : unit =
  let rec aux e = match e with
  | BEXPR_new ((_,t) as x),_ ->
    (*
    print_endline ("FOUND A NEW " ^ sbt bsym_table t);
    *)
    let index =
      try Hashtbl.find syms.registry t
      with Not_found -> failwith ("Can't find type in registry " ^ sbt bsym_table t)
    in
    Hashtbl.replace allocable_types t index;

  | x -> ()
  in
  Flx_bexpr.iter ~f_bexpr:aux e

let scan_exe syms bsym_table allocable_types exe : unit =
  Flx_bexe.iter ~f_bexpr:(scan_bexpr syms bsym_table allocable_types) exe

let scan_exes syms bsym_table allocable_types exes : unit =
  iter (scan_exe syms bsym_table allocable_types) exes

let gen_offset_tables syms bsym_table module_name =
  let allocable_types = Hashtbl.create 97 in
  let scan exes = scan_exes syms bsym_table allocable_types exes in
  let last_ptr_map = ref "NULL" in
  let primitive_shapes = Hashtbl.create 97 in
  let s = Buffer.create 20000 in

  (* print_endline "Function and procedure offsets"; *)
  Hashtbl.iter
  (fun (index,ts) instance ->
    let bsym =
      try Flx_bsym_table.find bsym_table index
      with Not_found ->
        failwith ("[gen_offset_tables] can't find index " ^ string_of_bid index)
    in
    (*
    print_endline ("Offsets for " ^ id ^ "<"^ si index ^">["^catmap "," (sbt bsym_table) ts ^"]");
    *)
    match Flx_bsym.bbdcl bsym with
    | BBDCL_function (props,vs,ps, ret,exes) ->
      scan exes;
      if mem `Cfun props then () else
      if mem `Heap_closure props then
        gen_fun_offsets s syms bsym_table index vs ps ret ts instance props last_ptr_map
      (*
      else
        print_endline ("Warning: no closure of " ^ id ^ "<"^si index ^"> is used")
      *)

    | BBDCL_procedure (props,vs,ps,exes) ->
      scan exes;
      if mem `Cfun props then () else
      if mem `Heap_closure props then
        gen_fun_offsets
          s
          syms
          bsym_table
          index
          vs
          ps
          btyp_void
          ts
          instance
          props
          last_ptr_map
      else if mem `Stack_closure props then ()
      else
        print_endline ("Warning: no closure of " ^ Flx_bsym.id bsym ^"<" ^
          string_of_bid index ^ "> is used, but not stackable?")
    | _ -> ()
  )
  syms.instances
  ;
  gen_thread_frame_offsets s syms bsym_table last_ptr_map
  ;

  (* We're not finished: we need offsets dynamically allocated types too *)

  (* currently the ONLY non-function types that can be allocated
    are the arguments of non-constant variant constructors:
    this WILL change when a 'new' operator is introduced.
  *)
  Hashtbl.iter
  (fun btyp index ->
    match unfold btyp with
    | BTYP_sum args ->
      iter begin fun t ->
        match t with
        | BTYP_tuple []
        | BTYP_void -> ()
        | _ ->
          try
            let index = Hashtbl.find syms.registry t in
            Hashtbl.replace allocable_types t index
          with Not_found -> ()
      end args

    | BTYP_variant args ->
      iter begin fun (_,t) ->
        match t with
        | BTYP_tuple []
        | BTYP_void -> ()
        | _ ->
          try
            let index = Hashtbl.find syms.registry t in
            Hashtbl.replace allocable_types t index
          with Not_found -> ()
      end args

    | BTYP_inst (i,ts) ->
      (*
      print_endline ("Thinking about instance type --> " ^ string_of_btypecode sym_table btyp);
      *)
      let bsym =
        try Flx_bsym_table.find bsym_table i
        with Not_found ->
          failwith ("[gen_offset_tables:BTYP_inst] can't find index " ^
            string_of_bid i)
      in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_abs (vs,bquals,_,_) ->
        (*
        print_endline ("abstract type "^id^".. quals:");
        print_endline (string_of_bquals sym_table bquals);
        *)
        let handle_qual bqual = match bqual with
        | `Bound_needs_shape t ->
          (*
          print_endline ("Needs shape (uninstantiated) " ^ sbt bsym_table t);
          *)
          let varmap = mk_varmap vs ts in
          let t = varmap_subst varmap t in
          (*
          print_endline ("Needs shape (instantiated) " ^ sbt bsym_table t);
          *)
          begin try
            let index = Hashtbl.find syms.registry t in
            Hashtbl.replace allocable_types t index
          with
          | Not_found -> failwith "[gen_offset_tables] Woops, type isn't in registry?"
          end

        | _ -> ()
        in
        let rec aux quals = match quals with
        | [] -> ()
        | h :: t -> handle_qual h; aux t
        in aux bquals


      (* this routine assumes any use of a union component is
         allocable .. this is quite wrong but safe. This SHOULD
         be drived by detecting constructor expressions

         We don't need to worry about pattern matches .. if
         we didn't construct it, perhaps a foreigner did,
         in which case THEY needed to create the shape object
      *)
      | BBDCL_union (vs,args) ->
        let varmap = mk_varmap vs ts in
        let args = map (fun (_,_,t)->t) args in
        let args = map (varmap_subst varmap) args in
        iter begin fun t ->
          match t with
          | BTYP_tuple []
          | BTYP_void -> ()
          | _ ->
            try
              let index = Hashtbl.find syms.registry t in
              Hashtbl.replace allocable_types t index
            with Not_found -> ()
        end args

      | _ -> ()
      end
    | _ -> ()
  )
  syms.registry
  ;
  Hashtbl.iter
  (fun btyp index ->
    (*
    print_endline ("allocable type --> " ^ string_of_btypecode sym_table btyp);
    *)
    match unfold btyp with
    | BTYP_function _ -> ()

    | BTYP_tuple args ->
      let name = cpp_type_classname syms bsym_table btyp in
      let offsets = get_offsets syms bsym_table btyp in
      let n = length offsets in
      let classname = cpp_type_classname syms bsym_table btyp in
      bcat s ("\n//OFFSETS for tuple type " ^ string_of_bid index ^ "\n");
      gen_offset_data s n name offsets false [] None last_ptr_map

    (* This is a pointer, the offset data is in the system library *)
    | BTYP_pointer t -> ()

    (* for an array, we only have offsets for the first element *)
    | BTYP_array (t,i) ->
      let k =
        try int_of_unitsum i
        with Not_found -> failwith "Array index must be unitsum"
      in
      let name = cpp_typename syms bsym_table btyp in
      let tname = cpp_typename syms bsym_table t in
      let offsets = get_offsets syms bsym_table t in
      let is_pod =
        match t with
        | BTYP_inst (k,ts) ->
          begin match Flx_bsym_table.find_bbdcl bsym_table k with
          | BBDCL_abs (_,quals,_,_) -> mem `Pod quals
          | _ -> false
          end
        | _ -> false
      in
      let n = length offsets in
      bcat s ("\n//OFFSETS for array type " ^ string_of_bid index ^ "\n");
      if n <> 0 then begin
        bcat s ("static std::size_t " ^ name ^ "_offsets["^si n^"]={\n  ");
        bcat s ("  " ^ cat ",\n  " offsets);
        bcat s "};\n"
      end
      ;

      let this_ptr_map = name ^ "_ptr_map" in
      let old_ptr_map = !last_ptr_map in
      last_ptr_map := "&"^this_ptr_map;

      if not is_pod then begin
        bcat s ("static void " ^ name ^ "_finaliser(collector_t *, void *p){\n");
        bcat s ("  (("^ tname ^ "*)p)->~" ^ tname ^ "();\n");
        bcat s ("  p = (void*)((char*)p + sizeof("^tname^"));\n");
        bcat s ("}\n")
      end
      ;
      bcat s ("static gc_shape_t "^ name ^"_ptr_map = {\n");
      bcat s ("  " ^ old_ptr_map ^ ",\n");
      bcat s ("  \"" ^ name ^ "\",\n");
      bcat s ("  " ^ si k ^ ",\n");
      bcat s ("  sizeof("^name^"),\n");
      bcat s
      (
        if not is_pod then ("  "^name^"_finaliser,\n")
        else ("  0,\n")
      );
      bcat s
      (
        "  "^si n^
        (
          if n = 0 then ",0,\n"
          else ",\n  " ^name^"_offsets,\n"
        )
      );
      bcat s "  gc_flags_default\n";
      bcat s "};\n"

    | BTYP_inst (i,ts) ->
      let name = cpp_typename syms bsym_table btyp in
      let bsym =
        try Flx_bsym_table.find bsym_table i
        with Not_found ->
          failwith (
            "[gen_offset_tables:BTYP_inst:allocable_types] can't find index " ^
            string_of_bid i)
      in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_abs (_,quals,_,_) ->
        let complete = not (mem `Incomplete quals) in
        let pod = mem `Pod quals in
        if complete then
          if not (Hashtbl.mem primitive_shapes name) then
          begin
            Hashtbl.add primitive_shapes name true;
            bcat s ("\n//OFFSETS for complete abstract "^(if pod then "pod " else "finalisable ")^
              "type " ^ name ^ " instance\n"
            );

            let this_ptr_map = name ^ "_ptr_map" in
            let old_ptr_map = !last_ptr_map in
            last_ptr_map := "&"^this_ptr_map;

            if not pod then bcat s ("FLX_FINALISER("^name^")\n");
            bcat s ( "static gc_shape_t " ^ name ^ "_ptr_map = {\n") ;
            bcat s ("  " ^ old_ptr_map ^ ",\n");
            bcat s ("  \"" ^ name ^ "\",\n");
            if pod then
              bcat s ("  1,sizeof("^name^"),0,0,0,gc_flags_default\n")
            else
              bcat s ("  1,sizeof("^name^"),"^name^"_finaliser,0,0,gc_flags_default\n")
            ;
            bcat s "};\n"
          end else begin
            bcat s ("\n//OFFSETS for abstract type " ^ name ^ " instance\n");
            bcat s ("//Use "^name^"_ptr_map\n");
          end
        else
          clierr (Flx_bsym.sr bsym)
          ("[ogen] attempt to allocate an incomplete type: '" ^ Flx_bsym.id bsym ^"'")

      | BBDCL_union _ -> () (* handled by universal _uctor_ *)
      | BBDCL_cstruct (vs,cps) ->
        (* cstruct shouldn't have allocable stuff in it *)

        let this_ptr_map = name ^ "_ptr_map" in
        let old_ptr_map = !last_ptr_map in
        last_ptr_map := "&"^this_ptr_map;

        bcat s ("\n//OFFSETS for cstruct type " ^ name ^ " instance\n");

        (* HACK .. in fact, some C structs might have finalisers! *)
        let pod = true in
        if not pod then bcat s ("FLX_FINALISER("^name^")\n");
        bcat s ( "static gc_shape_t " ^ name ^ "_ptr_map ={\n") ;
        bcat s ("  " ^ old_ptr_map ^ ",\n");
        bcat s ("  \"" ^ name ^ "\",\n");
        if pod then
          bcat s ("  1,sizeof("^name^"),0,0,0,gc_flags_default\n")
        else
          bcat s ("  1,sizeof("^name^"),"^name^"_finaliser,0,0,gc_flags_default\n")
        ;
        bcat s "};\n"

      | BBDCL_struct (vs,cps) ->
        failwith
        (
          "[ogen]: can't handle struct offsets yet: type " ^
          sbt bsym_table btyp
        )
        (*
        bcat s ("\n//OFFSETS for struct type " ^ name ^ " instance\n");
        bcat s ("//CANT HANDLE YET!\n");
        *)
      | _ ->
        failwith
        (
          "[ogen]: can't handle instances of this kind yet: type " ^
          sbt bsym_table btyp
        )
    end

   | BTYP_unitsum _ ->
     let name = cpp_typename syms bsym_table btyp in
     bcat s ("static gc_shape_t &"^ name ^"_ptr_map = flx::rtl::_int_ptr_map;\n");

   | BTYP_sum _ ->
     let name = cpp_typename syms bsym_table btyp in
     bcat s ("static gc_shape_t &"^ name ^"_ptr_map = flx::rtl::_uctor_ptr_map;\n");

   | _ ->
     failwith
     (
       "[ogen]: Unknown kind of allocable type " ^
       sbt bsym_table btyp
     )
  )
  allocable_types
  ;
  bcat s ("\n");

  bcat s ("// Head of shape list\n");
  bcat s ("extern \"C\" FLX_EXPORT gc_shape_t *" ^ cid_of_flxid module_name ^ "_head_shape;\n");
  bcat s ("gc_shape_t *" ^ cid_of_flxid module_name ^ "_head_shape=" ^ !last_ptr_map ^ ";\n");
  Buffer.contents s
