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
open Flx_unify
open Flx_exceptions
open List
open Flx_maps
open Flx_lookup_state
open Flx_bexe_state
open Flx_name_map
open Flx_bid

type pin_descr_t = string * (string * int * Flx_btype.t)
type device_descr_t = string * pin_descr_t list

let cal_channel bsym_table (schannel,ischannel,oschannel) sr typ : int * string * Flx_btype.t =
  match typ with
  | BTYP_inst (i,[t],_) ->
     let direction = match i with
     | _ when i = schannel -> "io"
     | _ when i = oschannel -> "output"
     | _ when i = ischannel -> "input"
     | _ -> clierrx "[flx_bind/flx_bind_circuit.ml:108: E40] " sr ("Invalid pin type " ^ sbt bsym_table typ) 
     in
     i,direction,t
  | _ -> clierr sr ("Invalid pin type " ^ sbt bsym_table typ)


let bind_circuit bsym_table (state : Flx_bexe_state.bexe_state_t) sr be (cs:Flx_ast.connection_t list) =
    let print_flag = state.lookup_state.print_flag in 
    let proc_t = Flx_btype.btyp_function (Flx_btype.btyp_unit (), Flx_btype.btyp_void ()) in  
    if print_flag then
      print_endline ("Processihg circuit spec");
    let lu name = lookup_name_in_env state.lookup_state bsym_table (state.env:env_t) sr name in
    let lun name = 
       match lu name with
       | NonFunctionEntry {base_sym=index} -> index
       | _ -> clierrx "[flx_bind/flx_bind_circuit.ml:36: E39] " sr ("Require channel type " ^ name ^ " to be in scope")
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
    let parent_ts = List.map (fun (s,i,k) -> 
      btyp_type_var (i,k)) state.parent_vs 
    in

    (* find all the devices *)
    let devices = 
      List.fold_left (fun acc term ->
        match term with
        | Connect (pins)->
          List.fold_left (fun acc (d,_) ->
            if List.mem d acc then acc else d :: acc
          ) acc pins
        | Wire (e,(rd,rp))-> 
          let acc = if List.mem rd acc then acc else rd :: acc in
           acc
      ) [] cs
    in 
    if print_flag then
      begin 
        print_endline ("Device list");
        List.iter (fun s -> print_endline ("  device " ^ s)) devices;
      end;

    let named_wires = 
      List.fold_left (fun acc term ->
        match term with
        | Wire (e,_) -> e :: acc 
        | Connect _ -> acc
      ) [] cs
    in 

    if print_flag then
    begin
      print_endline ("Named wires: " ^ catmap "," (fun e -> string_of_expr e) named_wires);
    end;

    let device_data : device_descr_t list =
      List.fold_left (fun acc s -> 
        if print_flag then
        begin
          print_string ("Binding device " ^ s); flush stdout;
        end;

        let name = EXPR_name (sr,s,[]) in
        let (_,t) as bname = 
          try be name 
          with exn -> print_endline ("Cannot bind device name " ^ s); raise exn 
        in
        if print_flag then
          print_endline (" device " ^ s ^ ": " ^ sbt bsym_table t);

        match t with
        | BTYP_function (BTYP_record pins, BTYP_function (BTYP_tuple [], BTYP_void)) -> 
          let pin_data :  pin_descr_t list =
            List.fold_left (fun acc (name,typ) ->
(*
              print_endline ("  pin " ^ name ^ ":" ^ sbt bsym_table typ);
*)
              let i,direction,vt = cal_channel bsym_table (schannel,ischannel,oschannel) sr typ in
(*
                 print_endline ("      pin " ^ name ^ ":" ^ direction ^ " " ^ sbt bsym_table t);
*)
              (name,(direction,i,vt))::acc
            )
            [] pins 
          in
          (s,pin_data):: acc
        | _ -> clierrx "[flx_bind/flx_bind_circuit.ml:119: E42] " sr ("Invalid device type " ^ sbt bsym_table t)
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
    let find_index (device,pin) = 
      try List.assoc (device,pin) pin_list 
      with Not_found ->
        clierr sr ("Invalid pin name '" ^ pin ^ "' of device '" ^ device^"'");
    in

    let npins = List.length pin_list in
    let partition = Array.init npins (fun i->i) in
    let rec find_cand i = 
      let j = Array.get partition i in
      if i = j then i else find_cand j
    in
    let join i j = Array.set partition (find_cand i) (find_cand j) in
     
    (* do connections *)
    List.iter (fun term ->
      match term with
      | Connect pins ->
        begin match pins with
        | h::t -> 
          let index = find_index h in
          List.iter (fun pin -> join index (find_index pin)) t
        | [] -> assert false
        end
      | Wire _ -> ()
      )
      cs 
    ;

    (* find partition *)
    let eqpins = Array.make npins (BidSet.empty) in
    for i = 0 to npins - 1 do
      let cand = find_cand i in
      Array.set eqpins cand (BidSet.add i (Array.get eqpins cand))
    done;

    (* find wires connected to pin groups *)
    let named_wire_con = Array.make npins None in
    List.iter (fun term ->
      match term with
      | Connect _ -> ()
      | Wire (e,(rd,rp)) -> 
         let pinindex = find_index (rd,rp) in
         let canonical_rep = find_cand pinindex in
(*
         print_endline ("Wire " ^ string_of_expr e^ " connects to pin " ^
           rd ^ "." ^ rp ^ " = pinindex " ^ string_of_int pinindex ^
           " canonical rep = " ^ string_of_int canonical_rep
         );
*)
        match Array.get named_wire_con canonical_rep with
        | Some _ ->
          clierr sr ("Connect a named wire "^string_of_expr e ^"to same pin group twice!")
        | None ->
          let e = 
            try be e
            with exn ->
              print_endline ("Cannot bind named wire " ^ string_of_expr e); raise exn
          in
          Array.set named_wire_con canonical_rep (Some e);
      )
      cs 
    ;

    (* show named wires *)
    if print_flag then
    for pinno = 0 to npins - 1 do
      let term = Array.get named_wire_con pinno in
      match term with
      | None -> ()
      | Some e ->
        print_endline ("Pin" ^ string_of_int pinno ^  " is connected to wire " ^ sbe bsym_table e)
    done;

    let str_of_pins pins =
      String.concat "," (List.map (fun pinindex ->
        let device,pin,dir,_,vt = List.assoc pinindex (pin_data) in
         device ^ "." ^ pin
       ) pins)
    in

    (* create wires : map wire index to list of pin indices*)
    let wires = 
      let wires = ref [] in
      let cnt = ref 0 in
      for pinno = 0 to npins - 1 do
        if print_flag then
        print_endline ("Examining pin #" ^ string_of_int pinno);
        let pinset = Array.get eqpins pinno in
        let pinlist = BidSet.fold (fun index acc -> index :: acc) pinset [] in
        if pinlist <> [] then 
        begin
           assert (pinno = find_cand pinno); (* canonical pin *)
           wires := (!cnt,pinlist) :: !wires;
           if print_flag then
           print_endline ("      ** Created wire, index " ^ string_of_int !cnt ^ " for canonical pin " ^ string_of_int pinno);
           incr cnt
        end
        else 
          if print_flag then
          print_endline ("      ** Not canonical")
      done;
      !wires
    in  

    (* show wires *)
    if print_flag then
    List.iter (fun (wireno,pins) -> 
      print_endline ("WIRE " ^ string_of_int wireno ^ " connects " ^ str_of_pins pins)
    )
    wires;

    (* validate I/O directions *)
    List.iter (fun (wireno,pins) -> 
      let reads = ref 0 and writes = ref 0 and ios = ref 0 in
      let check_named_wire pinindex pc =
        let nw = Array.get named_wire_con pinindex in
        begin match nw with
        | None ->
          begin match pc with 
          | `Solo ->
            let device,pin,dir,_,vt = List.assoc pinindex pin_data in
            print_endline ("WARNING: " ^ device ^ "." ^ pin ^ " is not connected")
          | _ -> ()
          end
        | Some (_,typ) -> 
          let i,dir,vt = cal_channel bsym_table (schannel,ischannel,oschannel) sr typ in
          (* polarity is backwards for wires *)
          begin match dir with
          | "input" -> incr writes 
          | "output" -> incr reads
          | "io" -> incr ios
          | _ -> assert false
          end
        end
      in
      let handle_pin pinindex =
        let device,pin,dir,_,vt = List.assoc pinindex (pin_data) in
          match dir with
          | "input" -> incr reads
          | "output" -> incr writes
          | "io" -> incr ios
          | _ -> assert false
      in
      begin match pins with
      | [] -> assert false
      | [pinindex] -> 
        check_named_wire pinindex `Solo;
        handle_pin pinindex 

      | pins ->
        List.iter (fun pinindex ->
          check_named_wire pinindex `Multi;
          handle_pin pinindex 
        ) pins
      end;
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
      let vt,maybe_named_wire =
        match pins with
        | [] -> assert false
        | pinindex :: _ -> 
          let _,_,_,_,vt = List.assoc pinindex pin_data in 
          let nw = Array.get named_wire_con pinindex in
          vt,nw
      in
      let name = "pin_" ^ string_of_int index in
      let stype = Flx_btype.btyp_inst (schannel, [vt], Flx_kind.KIND_type) in
      let bbdcl = Flx_bbdcl.bbdcl_val (state.parent_vs,stype,`Val) in
      let bsym = Flx_bsym.create ~sr name bbdcl in 
      Flx_bsym_table.add bsym_table index state.parent bsym;
(*
      print_endline ("Created channel variable " ^ name ^ " index " ^ 
        string_of_int index ^ " for wire " ^ string_of_int wireno ^ " value type " ^ sbt bsym_table vt);
*)
      let channel_value = 
        match maybe_named_wire with
        | Some ((_,typ) as e) -> 
          begin 
            let i,dir,vt' = cal_channel bsym_table (schannel,ischannel,oschannel) sr typ in
            if vt <> vt' then
              clierr sr 
              (
                "Wire of value type "^ sbt bsym_table vt' ^ 
                " connected to pins of value type "^ sbt  bsym_table vt
              )
            else e 
          end
        | None -> Flx_bexpr.bexpr_apply stype (mk_schannel vt,Flx_bexpr.bexpr_unit) 
      in
      
      (* FIXME: initialise to named wire if one exists *)
      let init =  Flx_bexe.bexe_init (sr,index,channel_value) in
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
          let ct = Flx_btype.btyp_inst (cast_schannelindex,[vt], Flx_kind.KIND_type) in
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



