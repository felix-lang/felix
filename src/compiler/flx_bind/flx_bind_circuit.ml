open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_lookup
open Flx_mbind
open Flx_unify
open Flx_exceptions
open List
open Flx_maps
open Flx_lookup_state
open Flx_bexe_state


type pin_descr_t = string * (string * int * Flx_btype.t)
type device_descr_t = string * pin_descr_t list

let bind_circuit bsym_table (state : Flx_bexe_state.bexe_state_t) sr be cs =
 
    let proc_t = Flx_btype.btyp_function (Flx_btype.btyp_unit (), Flx_btype.btyp_void ()) in  
(*
    print_endline ("Processihg circuit spec");
*)
    let lu name = lookup_name_in_env state.lookup_state bsym_table (state.env:env_t) sr name in
    let lun name = 
       match lu name with
       | NonFunctionEntry {base_sym=index} -> index
       | _ -> clierr sr ("Require channel type " ^ name ^ " to be in scope")
    in
    let luf name ts signs =
      Flx_lookup.lookup_name_with_sig' state.lookup_state
        bsym_table sr sr state.env state.env
        Flx_lookup_state.rsground name ts signs
    in
    let lus name = 
      let signs = [Flx_btype.btyp_function (Flx_btype.btyp_unit (), Flx_btype.btyp_void ())] in
      let ts = [] in
      luf name ts signs
    in
    let schannel = lun "schannel" in
    let ischannel = lun "ischannel" in
    let oschannel = lun "oschannel" in
    let spawn_fthread : Flx_bexpr.t = lus "spawn_fthread" in
    let mk_schannel t: Flx_bexpr.t = luf "mk_schannel" [t] [(Flx_btype.btyp_unit ())] in
(*
    print_endline ("Spawn_fthread = " ^ sbe bsym_table spawn_fthread);
*)
    let parent_ts = List.map (fun (s,i) -> btyp_type_var (i,btyp_type 0)) state.parent_vs in

    (* find all the devices *)
    let devices = 
      List.fold_left (fun acc ((ld,_),(rd,_)) ->
        let acc = if List.mem ld acc then acc else ld :: acc in
        let acc = if List.mem rd acc then acc else rd :: acc in
        acc
      ) [] cs
    in 
(*
    print_endline ("Device list");
    List.iter (fun s -> print_endline ("  device " ^ s)) devices;
*)
(*
    print_endline ("Device types");
*)
    let device_data : device_descr_t list =
      List.fold_left (fun acc s -> 
        let name = EXPR_name (sr,s,[]) in
        let (_,t) as bname = be name in
(*
        print_endline (" device " ^ s ^ ": " ^ sbt bsym_table t);
*)
        match t with
        | BTYP_function (BTYP_record pins, BTYP_function (BTYP_tuple [], BTYP_void)) -> 
          let pin_data :  pin_descr_t list =
            List.fold_left (fun acc (name,typ) ->
(*
              print_endline ("  pin " ^ name ^ ":" ^ sbt bsym_table typ);
*)
              match typ with
              | BTYP_inst (i,[t]) ->
                 let direction = match i with
                 | _ when i = schannel -> "io"
                 | _ when i = oschannel -> "output"
                 | _ when i = ischannel -> "input"
                 | _ -> clierr sr ("Invalid pin type " ^ sbt bsym_table typ) 
                 in
(*
                 print_endline ("      pin " ^ name ^ ":" ^ direction ^ " " ^ sbt bsym_table t);
*)
                 (name,(direction,i,t))::acc
              | _ -> clierr sr ("Invalid pin type " ^ sbt bsym_table typ)
            )
            [] pins 
          in
          (s,pin_data):: acc
        | _ -> clierr sr ("Invalid device type " ^ sbt bsym_table t)
      )
      []
      devices
    in
    let pin_list,pin_data = 
      let pin_list = ref [] in (* device.pin -> index *)
      let pin_data = ref [] in (* index -> info *)
      let cnt = ref 0 in
      List.iter (fun (device,pins) ->
        List.iter (fun (pin,(dir,index,vt)) -> 
         let x = (device,pin),!cnt in 
         pin_list := x :: !pin_list;
         pin_data := (!cnt,(device,pin,dir,index,vt)) :: !pin_data;
         incr cnt; 
         )
         pins
      )
      device_data;
      List.rev !pin_list, !pin_data
    in
    let find_index (device,pin) = List.assoc (device,pin) pin_list in

    let npins = List.length pin_list in
    let partition = Array.init npins (fun i->i) in
    let find_cand i = 
      let x = ref i in
      while !x <> Array.get partition i do x := Array.get partition i done;
      !x
    in
    let join i j = Array.set partition (find_cand i) (find_cand j) in
     
    (* do connections *)
    List.iter (fun ((ld,lp),(rd,rp)) ->
      let lindex = List.assoc (ld,lp) pin_list in
      let rindex = List.assoc (rd,rp) pin_list in
      join lindex rindex
      )
      cs 
    ;

    (* find partition *)
    let eqpins = Array.make npins (BidSet.empty) in
    for i = 0 to npins - 1 do
      let cand = find_cand i in
      Array.set eqpins cand (BidSet.add i (Array.get eqpins cand))
    done;

    (* create wires : map wire index to list of pin indices*)
    let wires = 
      let wires = ref [] in
      let cnt = ref 0 in
      for i = 0 to npins - 1 do
         let pinset = Array.get eqpins i in
         if not (BidSet.is_empty pinset) then 
         begin
           wires := (!cnt,(BidSet.fold (fun index acc -> index :: acc) pinset [])) :: !wires;
           incr cnt
         end
      done;
      !wires
    in  
    let str_of_pins pins =
      String.concat "," (List.map (fun pinindex ->
        let device,pin,dir,_,vt = List.assoc pinindex (pin_data) in
         device ^ "." ^ pin
       ) pins)
    in
    (* show wires *)
(*
    List.iter (fun (wireno,pins) -> 
      print_endline ("WIRE " ^ string_of_int wireno ^ " connects " ^ str_of_pins pins)
    )
    wires;
*)
    (* validate I/O directions *)
    List.iter (fun (wireno,pins) -> 
      match pins with
      | [] -> assert false
      | [pinindex] -> 
        let device,pin,dir,_,vt = List.assoc pinindex pin_data in
        print_endline ("WARNING: " ^ device ^ "." ^ pin ^ " is not connected")
      | pins ->
        let reads = ref 0 and writes = ref 0 and ios = ref 0 in
        List.iter (fun pinindex ->
          let device,pin,dir,_,vt = List.assoc pinindex (pin_data) in
          match dir with
          | "input" -> incr reads
          | "output" -> incr writes
          | "io" -> incr ios
          | _ -> assert false
        ) pins;
        if !ios = 0 && !reads = 0 then
          print_endline ("WARNING: pins " ^ str_of_pins pins ^ " are all output")
        else if !ios = 0 && !writes = 0 then
          print_endline ("WARNING: pins " ^ str_of_pins pins ^ " are all input")
    )
    wires;

    let pin_map =
      let pin_map = ref [] in
      List.iter (fun (wireno, pins) ->
        List.iter (fun pinindex ->
           assert (not (List.mem_assoc pinindex !pin_map));
           pin_map := (pinindex,wireno) :: !pin_map
        )
        pins
      )
      wires;
      !pin_map
    in
      
    (* assign symbol table indices to the wires *)
    let wire_map = 
      List.fold_left (fun acc (wireno,_) ->
        let index = !(state.counter) in
        incr state.counter;
        (wireno,index)::acc)
      []
      wires
    in

    let exes = ref [] in

    (* create variables *)
    List.iter (fun (wireno,index) ->
      let pins = List.assoc wireno wires in
      let vt =
        match pins with
        | [] -> assert false
        | pinindex :: _ -> let _,_,_,_,vt = List.assoc pinindex pin_data in vt
      in
      let name = "pin_" ^ string_of_int index in
      let stype = Flx_btype.btyp_inst (schannel, [vt]) in
      let bbdcl = Flx_bbdcl.bbdcl_val (state.parent_vs,stype,`Val) in
      let bsym = Flx_bsym.create ~sr name bbdcl in 
      Flx_bsym_table.add bsym_table index state.parent bsym;
(*
      print_endline ("Created channel variable " ^ name ^ " index " ^ 
        string_of_int index ^ " for wire " ^ string_of_int wireno ^ " value type " ^ sbt bsym_table vt);
*)
      let channel_value = Flx_bexpr.bexpr_apply stype (mk_schannel vt,Flx_bexpr.bexpr_unit) in
      let init = Flx_bexe.bexe_init (sr,index,channel_value) in
(*
      print_endline ("Init " ^ string_of_bexe bsym_table 0 init);
*)
      exes := init :: !exes
    )
    wire_map;

    (* spawn fthreads *)
    List.iter (fun device ->
      let pins = List.assoc device device_data in
(*
      print_endline ("Handling device " ^ device);
*)
      let fields = 
        List.fold_left (fun acc (pin,_) ->
          let pinindex = List.assoc (device,pin) pin_list in
          let pindata = List.assoc pinindex pin_data in
          let _,_,_,cast_schannelindex,vt = List.assoc pinindex pin_data in
          let wireno = List.assoc pinindex pin_map in
          let vindex = List.assoc wireno wire_map in
(*
          print_endline ("   pin " ^ pin ^ " index " ^ 
            string_of_int pinindex ^ " value type " ^ sbt bsym_table vt ^ 
            " wireno " ^ string_of_int wireno ^ " variable " ^ string_of_int vindex);
*)
          (* the type expected, ischannel,oschannel, or schannel, is NOT
            the actual variable type which is always just schannel
          *)
          let ct = Flx_btype.btyp_inst (cast_schannelindex,[vt]) in
          let component = pin,(Flx_bexpr.bexpr_varname ct (vindex, parent_ts)) in
          component::acc
        )
        []
        pins
      in 
      let record = Flx_bexpr.bexpr_record fields in
(*
      print_endline ("Record value is " ^ sbe bsym_table record);
*)
      (* waste, already done but didn't save, FIXME *)
      let bdev = 
        let name = EXPR_name (sr,device,[]) in
        be name 
      in
      let device_closure = Flx_bexpr.bexpr_apply proc_t (bdev,record) in
      let spawn_exe = Flx_bexe.bexe_call (sr,spawn_fthread,device_closure) in
(*
      print_endline ("SPAWN: " ^ string_of_bexe bsym_table 0 spawn_exe);
*)
      exes := spawn_exe :: !exes
    )
    devices;
    bexe_comment (sr, "create circuit") :: List.rev !exes


