(** Flx_why: This module converts a bound Felix program into the Why
 * verification language. See this for more: http://why.lri.fr/. *)

open List

open Flx_ast
open Flx_bbdcl
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_maps
open Flx_mtypes2
open Flx_options
open Flx_types
open Flx_util

(** Searches the bound symbol table for all the symbols with the given name. *)
let find_name bsym_table root name =
  let rec search_root bsyms root =
    let bsyms =
      Flx_types.BidSet.fold begin fun child bsyms ->
        let bsym = Flx_bsym_table.find bsym_table child in
        if Flx_bsym.id bsym = name then (child, bsym) :: bsyms else bsyms
      end (Flx_bsym_table.find_children bsym_table root) bsyms
    in

    (* We didn't find it, so search up the parents. *)
    let parent =
      try Some (Flx_bsym_table.find_parent bsym_table root)
      with Not_found -> None
    in
    match parent with
    | Some (Some parent) -> search_root bsyms parent
    | _ -> bsyms
  in
  search_root [] root


(* Hackery to find logic functions in the library *)
let find_function syms bsym_table root name =
  let bsyms = find_name bsym_table root name in
  let bsyms =
    List.filter begin fun (_, bsym) ->
      match Flx_bsym.bbdcl bsym with
      | BBDCL_external_fun (_,_,args,res,ct,_,_) ->
        begin match Flx_bsym.id bsym, args, res with
        | "lnot", [BTYP_unitsum 2], BTYP_unitsum 2 -> true
        | _, [BTYP_unitsum 2; BTYP_unitsum 2], BTYP_unitsum 2 -> true
        | _ -> false
        end
      | _ -> false
    end bsyms
  in
  match bsyms with
  | [i, _] -> i
  | [] ->
      if syms.compiler_options.print_flag then
      print_endline ("WARNING: flx_why cannot find '" ^ name ^ "'");
      dummy_bid
  | _ ->
      print_endline ("WARNING: flx_why found too many '" ^ name ^ "'");
      dummy_bid

let find_logics syms bsym_table root =
  let ff x = find_function syms bsym_table root x in
  [
    ff "land", "and";
    ff "lor", "or";
    ff "implies", "->";
    ff "eq", "<->";
    ff "lnot", "not"
  ]

(** Converts a felix name into a why name. *)
let whyid_of_flxid =
  let why_keywords = [
    "absurd"; "and; array"; "as"; "assert";
    "axiom"; "begin"; "bool"; "do"; "done";
    "else"; "end"; "exception"; "exists"; "external";
    "false"; "for"; "forall"; "fun"; "function";
    "goal"; "if"; "in"; "int"; "invariant";
    "let"; "logic"; "not"; "of";
    "or"; "parameter"; "predicate"; "prop"; "raise";
    "raises"; "reads"; "real"; "rec"; "ref";
    "returns"; "then"; "true"; "try"; "type";
    "unit"; "variant"; "void"; "while"; "with";
    "writes"] in

  (* special names in thread frame *)
  let fixups = [
    "argc","_argc";
    "argv","_argv";
    "flx_stdin","_flx_stdin";
    "flx_stdout","_flx_stdout";
    "flx_stderr","_flx_stderr";
    "gc","_gc";
  ] @ List.map (fun k -> k, "_" ^ k) why_keywords in

  (* Now define the actual function. *)
  fun s ->
  let n = String.length s in
  let id = Buffer.create (n+10) in
  (* if the value is prefixed with a number, prepend an underscore *)
  if n > 1 && s.[0] >= '0' && s.[0] <= '9' then Buffer.add_char id '_';
  for i=0 to n - 1 do
    (* from http://www.w3.org/TR/html4/sgml/entities.html *)
    match s.[i] with
    | ' '  -> Buffer.add_string id "__sp_"
    | '!'  -> Buffer.add_string id "__excl_"
    | '"'  -> Buffer.add_string id "__quot_"
    | '#'  -> Buffer.add_string id "__num_"
    | '$'  -> Buffer.add_string id "__dollar_"
    | '%'  -> Buffer.add_string id "__percnt_"
    | '&'  -> Buffer.add_string id "__amp_"
    | '\'' -> Buffer.add_string id "__apos_"
    | '('  -> Buffer.add_string id "__lpar_"
    | ')'  -> Buffer.add_string id "__rpar_"
    | '*'  -> Buffer.add_string id "__ast_"
    | '+'  -> Buffer.add_string id "__plus_"
    | ','  -> Buffer.add_string id "__comma_"
    | '-'  -> Buffer.add_string id "__hyphen_"
    | '.'  -> Buffer.add_string id "__period_"
    | '/'  -> Buffer.add_string id "__sol_"
    | ':'  -> Buffer.add_string id "__colon_"
    | ';'  -> Buffer.add_string id "__semi_"
    | '<'  -> Buffer.add_string id "__lt_"
    | '='  -> Buffer.add_string id "__equals_"
    | '>'  -> Buffer.add_string id "__gt_"
    | '?'  -> Buffer.add_string id "__quest_"
    | '@'  -> Buffer.add_string id "__commat_"
    | '['  -> Buffer.add_string id "__lsqb_"
    | '\\' -> Buffer.add_string id "__bsol_"
    | ']'  -> Buffer.add_string id "__rsqb_"
    | '^'  -> Buffer.add_string id "__caret_"
    (* | '_'  -> Buffer.add_string id "__lowbar_" *)
    | '`'  -> Buffer.add_string id "__grave_"
    | '{'  -> Buffer.add_string id "__lcub_"
    | '|'  -> Buffer.add_string id "__verbar_"
    | '}'  -> Buffer.add_string id "__rcub_"
    | '~'  -> Buffer.add_string id "__tilde_"
    | x    -> Buffer.add_char id x
  done;
  let name = Buffer.contents id in
  try List.assoc name fixups with Not_found -> name

(** Convert a bid into a why identifier. *)
let whyid_of_bid i = string_of_int i

(** Look up the name of a felix symbol. *)
let getname syms bsym_table i =
  try
    whyid_of_flxid (Flx_bsym_table.find_id bsym_table i)
  with Not_found ->
    "index_" ^ whyid_of_bid i

let flx_bool = btyp_unitsum 2

let isbool2 t =
  t = btyp_array (flx_bool, flx_bool)

let rec why_expr syms bsym_table e =
  let ee e = why_expr syms bsym_table e in
  match e with
  | BEXPR_apply ((BEXPR_closure (i,ts),_),b),_ ->
    let id = getname syms bsym_table i in
    id ^ "_" ^ whyid_of_bid i ^ "(" ^
    (
      match b with
      | BEXPR_tuple [],_ -> "void"
      | BEXPR_tuple ls,_ -> catmap ", " ee ls
      | x -> ee x
    ) ^
    ")"


  | BEXPR_apply (a,b),_ ->
     "apply(" ^ ee a ^ "," ^ ee b ^")"

  (* this probably isn't right, ignoring ts *)
  | BEXPR_closure (i,ts),_ ->
    let id = getname syms bsym_table i in
    id ^ "_" ^ whyid_of_bid i

  (* this probably isn't right, ignoring ts *)
  | BEXPR_name (i,ts),_ ->
    let id = getname syms bsym_table i in
    id ^ "_" ^ whyid_of_bid i

  | BEXPR_tuple ls,_ ->
    "(" ^ catmap ", " ee ls ^ ")"

  | BEXPR_literal x,_ -> begin match x with
    | Flx_literal.Int (s,j) -> j
    | _ -> "UNKLIT"
    end
  | _ -> "UNKEXPR"


let rec why_prop syms bsym_table logics e =
  let ee e = why_expr syms bsym_table e in
  let ep e = why_prop syms bsym_table logics e in
  match e with
  | BEXPR_apply ((BEXPR_closure (i,ts),_),b),_ ->
    let op = try assoc i logics with Not_found -> "" in
    begin match op with
    | "and"
    | "or"
    | "->" ->
      begin match b with
      | BEXPR_tuple [x;y],t when isbool2 t ->
        ep x ^ " " ^ op ^ " " ^ ep y

      | _ -> failwith ("[flx_why] Wrong number or type of args to '" ^ op ^ "'")
      end

    | "<->" ->
      begin match b with
      | BEXPR_tuple [x;y],t when isbool2 t ->
        ep x ^ " " ^ op ^ " " ^ ep y

      | _ -> "true=" ^ ee e
      end


    | "not" -> op ^ " " ^ ep b

    | "" -> "true=" ^ ee e
    | _ -> assert false
    end
  | _ -> "true=" ^ ee e


let cal_bvs bvs =
  let tps = match bvs with
    | [] -> ""
    | [s,_] -> "'" ^ s ^ " "
    | ss -> "('" ^ catmap ", '" fst ss ^ ") "
  in tps

let emit_type syms bsym_table f index name sr bvs =
  let srt = Flx_srcref.short_string_of_src sr in
  output_string f ("(* type " ^ name ^ ", at "^srt^" *)\n");

  (* NOTE BUG: needs namespace qualifier mangled in! *)
  if name = "int" then
    output_string f ("(* type int" ^ " -- USE why's builtin *)\n\n")
  else
    let tps = cal_bvs bvs in
    output_string f ("type " ^ tps ^ name ^ "\n\n")

let rec cal_type syms bsym_table t =
  let ct t = cal_type syms bsym_table t in
  match t with
(*  | BTYP_lvalue t -> ct t ^ " lvalue " *)
  | BTYP_tuple [] -> "unit"
  | BTYP_void -> "unit" (* cheat *)
  | BTYP_unitsum 2 -> "bool"
  | BTYP_function (a,b) ->
    "(" ^ ct a ^ ", " ^ ct b ^ ") fn"

  | BTYP_inst (index,ts) ->
    (* HACK! *)
    let ts = match ts with
      | [] -> ""
      | [t] -> cal_type syms bsym_table t ^ " "
      | ts -> "(" ^ catmap ", " ct ts ^ ")"
    in
    ts ^ Flx_bsym_table.find_id bsym_table index
  | BTYP_type_var (index,_) ->
    begin
      try "'" ^ Flx_bsym_table.find_id bsym_table index with Not_found ->
        "'T" ^ whyid_of_bid index
    end

  | _ -> "dunno"

let emit_axiom syms bsym_table logics f (k:axiom_kind_t) (name,sr,parent,kind,bvs,bps,e) =
  if k <> kind then () else
  let srt = Flx_srcref.short_string_of_src sr in
  let tkind,ykind =
    match kind with
    | Axiom -> "axiom", "axiom"
    | Lemma -> "lemma", "goal"
  in
  output_string f ("(* "^tkind^" " ^ name ^ ", at "^srt^" *)\n\n");
  output_string f (ykind ^ " " ^ name ^ ":\n");
  List.iter (fun {pkind=pkind; pid=pid; pindex=pindex; ptyp=ptyp} ->
    output_string f
    ("  forall " ^ pid ^ "_" ^ whyid_of_bid pindex ^ ": " ^
      cal_type syms bsym_table ptyp ^ ".\n")
  )
  (fst bps)
  ;
  begin match e with
  | `BPredicate e ->
    output_string f ("    " ^ why_prop syms bsym_table logics e)

  | `BEquation (l,r) ->
    output_string f ("  " ^
      why_expr syms bsym_table l ^ " = " ^
      why_expr syms bsym_table r
    )

  end;
  output_string f "\n\n"

let emit_reduction syms bsym_table logics f (name,bvs,bps,el,er) =
  output_string f ("(* reduction " ^ name ^ " *)\n\n");
  output_string f ("axiom " ^ name ^ ":\n");
  List.iter (fun {pkind=pkind; pid=pid; pindex=pindex; ptyp=ptyp} ->
    output_string f
    ("  forall " ^ pid ^ "_" ^ Flx_print.string_of_bid pindex ^ ": " ^
      cal_type syms bsym_table ptyp ^ ".\n")
  )
  bps
  ;
  output_string f ("    " ^ why_expr syms bsym_table el);
  output_string f ("\n  = " ^ why_expr syms bsym_table er);
  output_string f "\n\n"


let emit_function syms bsym_table f index id sr bvs ps ret =
  let srt = Flx_srcref.short_string_of_src sr in
  output_string f ("(* function " ^ id ^ ", at "^srt^" *)\n");
  let name = whyid_of_flxid id ^ "_" ^ Flx_print.string_of_bid index in
  let dom = match ps with
    | [] -> "unit"
    | _ -> catmap ", " (cal_type syms bsym_table) ps
  in
  let cod = cal_type syms bsym_table ret in
  output_string f ("logic " ^ name ^ ": " ^ dom ^ " -> " ^ cod ^ "\n\n")

let calps ps =
  let ps = fst ps in (* elide constraint *)
  Flx_bparameter.get_btypes ps

let unitt = btyp_tuple []

let emit_whycode filename syms bsym_table root =
  let logics = find_logics syms bsym_table root in
  let f = open_out filename in
  output_string f "(****** HACKS *******)\n";
(*  output_string f "type 'a lvalue  (* Felix lvalues *) \n"; *)
  output_string f "type dunno      (* translation error *)\n";
  output_string f "type ('a,'b) fn (* functions *)\n";
  output_string f "logic apply: ('a,'b) fn, 'a -> 'b (* application *)\n";
  output_string f "\n";

  output_string f "(****** ABSTRACT TYPES *******)\n";
  Flx_bsym_table.iter begin fun index _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_external_type (bvs,qual,ct,breqs) ->
        emit_type syms bsym_table f index (Flx_bsym.id bsym) (Flx_bsym.sr bsym) bvs
    | _ -> ()
  end bsym_table;

  output_string f "(****** UNIONS *******)\n";
  Flx_bsym_table.iter begin fun index _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_union (bvs,variants) ->
        emit_type syms bsym_table f index (Flx_bsym.id bsym) (Flx_bsym.sr bsym) bvs
    | _ -> ()
  end bsym_table;

  output_string f "(****** STRUCTS *******)\n";
  Flx_bsym_table.iter begin fun index _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_cstruct (bvs,variants,_)
    | BBDCL_struct (bvs,variants) ->
        emit_type syms bsym_table f index (Flx_bsym.id bsym) (Flx_bsym.sr bsym) bvs
    | _ -> ()
  end bsym_table;

  output_string f "(******* FUNCTIONS ******)\n";
  Flx_bsym_table.iter begin fun index _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (_,bvs,ps,ret,_) ->
        let ps = calps ps in
        emit_function
          syms
          bsym_table
          f
          index
          (Flx_bsym.id bsym)
          (Flx_bsym.sr bsym)
          bvs
          ps
          (match ret with BTYP_void -> unitt | _ -> ret)

    | BBDCL_external_fun (_,bvs,ps,ret,_,_,_) ->
        emit_function
          syms
          bsym_table
          f
          index
          (Flx_bsym.id bsym)
          (Flx_bsym.sr bsym)
          bvs
          ps
          (match ret with BTYP_void -> unitt | _ -> ret)

    | _ -> ()
  end bsym_table;

  output_string f "(******* AXIOMS ******)\n";
  List.iter
    (emit_axiom syms bsym_table logics f Axiom)
    !(syms.axioms);

  output_string f "(******* REDUCTIONS ******)\n";
  List.iter
    (emit_reduction syms bsym_table logics f)
    !(syms.reductions);

  output_string f "(******* LEMMAS (goals) ******)\n";
  List.iter
    (emit_axiom syms bsym_table logics f Lemma)
    !(syms.axioms);

  close_out f
