open Flx_mtypes2
open Flx_ast
open Flx_set
open Flx_typing
open Flx_typing2
open Flx_print
open Flx_exceptions
open Flx_util
open Flx_list
open Ocs_types
open Sex_types
open Dyp
open Lexing
open Flx_string
open Flx_token
open Flx_parse_ebnf

let strip_us s =
  let n = String.length s in
  let x = Buffer.create n in
  for i=0 to n - 1 do
    match s.[i] with
    | '_' -> ()
    | c -> Buffer.add_char x c
  done;
  Buffer.contents x


type action_t = [`Scheme of string | `None]
type symbol_t = [`Atom of token | `Group of dyalt_t list]
and dyalt_t = symbol_t list * Flx_srcref.t * action_t * anote_t

let lexeme x = Lexing.lexeme (Dyp.std_lexbuf x)

let getsr dyp =
  let s = dyp.symbol_start_pos() and e = dyp.symbol_end_pos() in
  Flx_srcref.make (
    s.pos_fname,
    s.pos_lnum,
    s.pos_cnum - s.pos_bol + 1,
    e.pos_lnum,
    e.pos_cnum - e.pos_bol)

let incr_lineno lexbuf n = 
  let lexbuf = Dyp.std_lexbuf lexbuf in
  let n = ref n in
  while !n <> 0 do Lexing.new_line lexbuf; decr n done

let set_lineno lexbuf n =
  let lexbuf = Dyp.std_lexbuf lexbuf in
  let lcp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { lcp with
    pos_lnum = n;
    pos_bol = lcp.pos_cnum;
  }

let lfcount s =
  let n = ref 0 in
  for i = 0 to (String.length s) - 1 do
    if s.[i] = '\n' then incr n
  done;
  !n

(* string parsers *)
let decode_qstring s = let n = String.length s in unescape (String.sub s 0 (n-1))
let decode_dstring s = let n = String.length s in unescape (String.sub s 0 (n-1))
let decode_qqqstring s = let n = String.length s in unescape (String.sub s 0 (n-3))
let decode_dddstring s = let n = String.length s in unescape (String.sub s 0 (n-3))

let decode_raw_qstring s = let n = String.length s in String.sub s 0 (n-1)
let decode_raw_dstring s = let n = String.length s in String.sub s 0 (n-1)
let decode_raw_qqqstring s = let n = String.length s in String.sub s 0 (n-3)
let decode_raw_dddstring s = let n = String.length s in String.sub s 0 (n-3)


let silly_strtoken k = Flx_prelex.string_of_token k

let fresh_dssl = {
  prios = [];
  rules = [];
  deps = [];
  privacy = Drules.empty;
}

exception Scheme_error of sval

let giveup () = raise Giveup; Sunspec
let sraise s  = raise (Scheme_error s); Sunspec

let flx_ocs_init env =
  Ocs_env.set_pf0 env giveup "giveup";
  Ocs_env.set_pf1 env sraise "raise"

let init_env () =
  let env = Ocs_top.make_env () in
  flx_ocs_init env;

  let v1:Ocs_types.sval = Ocs_sym.get_symbol "_sr" in
  let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = Sunbound } in
  Ocs_env.bind_name env v1 g1;

  let v1:Ocs_types.sval = Ocs_sym.get_symbol "_arg" in
  let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = Sunbound } in
  Ocs_env.bind_name env v1 g1;

  for n = 1 to 20 do
    let v1:Ocs_types.sval = Ocs_sym.get_symbol ("_" ^ string_of_int n) in
    let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = Sunbound } in
    Ocs_env.bind_name env v1 g1;
  done;
  env

let debug = try ignore(Sys.getenv "FLX_DEBUG_PARSER"); true with Not_found -> false

let global_data = {
  pcounter = ref 1;
  env = init_env ();
  pdebug = ref debug;
}

let local_data = {
  Flx_token.dssls = Drules.empty;
  Flx_token.loaded_dssls = [];
  Flx_token.scm = [];
  Flx_token.rev_stmts = [];
}


let xsr sr =
  match Flx_srcref.to_tuple sr with f,fl,fc,ll,lc ->
  Ocs_misc.make_slist Snull ((Sint lc) :: (Sint ll) :: (Sint fc) :: (Sint ll) :: (Sstring f) :: [])

let buffer_add_ocs b r = Ocs_print.print_to_buffer b false r

let scheme_lex sr (s:string):sval =
  let sr = Flx_srcref.short_string_of_src sr in
  let inp = Ocs_port.string_input_port s in
  let lex = Ocs_lex.make_lexer inp sr in
  match Ocs_read.read_expr lex with
  | Ocs_types.Seof -> print_endline "END OF FILE?"; Snull
  | v ->  v

let scheme_compile env (s:sval):Ocs_types.code =
  Ocs_compile.compile env s

let scheme_eval (c:Ocs_types.code):sval =
  let th = Ocs_top.make_thread () in
  let term = ref None in
  Ocs_eval.eval th (fun (r:sval) -> term := Some r) c;
  match !term with
  | None -> failwith "Scheme term not returned!"
  | Some r -> r

let scheme_run sr env (s:string):sval =
  let l :sval = scheme_lex sr s in
  let c :code = scheme_compile env l in
  let r :sval = scheme_eval c in
  r

let cal_priority_relation p =
  match p with
  | `No_prio -> No_priority
  | `Eq_prio p -> Eq_priority p
  | `Less_prio p -> Less_priority p
  | `Lesseq_prio p -> Lesseq_priority p
  | `Greater_prio p -> Greater_priority p
  | `Greatereq_prio p -> Greatereq_priority p

(* Add a production to a dssl *)
let define_scheme sr dyp dssl_record dssl name prio rhs (scm:string) =
(*
print_endline ("define_scheme " ^ name);
*)
  let mapnt name = try Drules.find name dssl_record.privacy with Not_found -> name in
  let pr_age = !(dyp.global_data.pcounter) in
  incr (dyp.global_data.pcounter);
  let name = mapnt name in

  let cde =
    try
      let l = scheme_lex sr scm in
      let c = scheme_compile dyp.global_data.env l in
      c
    with
    | Ocs_error.Error err | Ocs_error.ErrorL (_,err) -> failwith ("Error " ^ err ^ " compiling " ^ scm)
  in

  let priority = match prio with
    | `Default -> "default_priority"
    | `Priority p -> p
  in

  (* Translate Felix production to Dypgen production *)
  let rule = 
    let f o = match o with
      | STRING s ->  (* Handle strings like "fun" in productions *)
        Dyp.Regexp (Dyp.RE_String s)

      | NONTERMINAL (s,p) -> (* handle identifiers like sexpr in productions *)
        let nt = mapnt s in
        let ntpri = cal_priority_relation p in
        Dyp.Non_ter (nt,ntpri)

      | NAME s -> assert false (* should have been converted to NONTERMINAL *)

      | s -> (* anything else is an actual keyword or symbol so make a Dypgen terminal *)
        let name = Flx_prelex.name_of_token s in
        Dyp.Ter name
    in
     name,(List.map f rhs),priority,[] 
  in
  if !(dyp.global_data.pdebug) then
    print_endline (
      "Rule " ^ string_of_int pr_age ^ " " ^ name ^ "["^priority^"] := " ^
      catmap " " silly_strtoken rhs ^
      " =># " ^ scm
    )
   ;
  (* this is the core routine which defines the action on reducing the production *)
  let action = fun dyp2 avl ->
    match avl,scm with
    (* optimise special case *)
    | [`Obj_sexpr (_,sr,s)],"_1" -> `Obj_sexpr (pr_age,sr,s),[]
    | _ ->
    let age = ref pr_age in
    let b = Buffer.create 200 in

    if !(dyp.global_data.pdebug) then begin
      Buffer.add_string b (
        "Reducing Rule " ^ string_of_int pr_age ^ " for " ^ name ^ "[" ^
        priority ^ "], scm=" ^ scm ^ "\n"
      );

      print_endline (
        "Reducing Rule " ^ string_of_int pr_age ^ " for " ^ name ^ "[" ^
        priority ^ "] := " ^ catmap " " silly_strtoken rhs ^ " #scm=" ^ scm ^
        "\n"
      );
    end;

    (* let env = Ocs_env.env_copy dyp.local_data.env in *)
    (* let env = dyp.local_data.env in *)
    let env = dyp.global_data.env in

    (* this is the core routine which compares the parsed attributes
     * with the translated production at parse time
     * objs: the symbols of the production
     * syms: the attributes parsed for those symbols 
     *)
    let rec aux objs syms n = match objs, syms with
      | [],[] -> ()
      | [],_ | _,[] -> assert false
      | h1::t1,h2::t2 ->
        let s =
          match h1,h2 with
          | _,`Obj_sexpr (seq,sr,s) ->
            (* age := max !age seq; *)
            s

          | k,`Obj_keyword -> Snull (* Sstring (Flx_prelex.string_of_token k) *)

          | STRING s1, `Obj_NAME s2 ->
            if s1 <> s2 then raise Giveup;
            Sstring s1

          | STRING _, `Lexeme_matched s -> (* print_endline ("Matched regexp to " ^ s); *) Sstring s

          | k , _ ->
          print_endline ("Woops, unhandled token=" ^ Flx_prelex.string_of_token k);
          Sstring (Flx_prelex.string_of_token k)
        in
        if !(dyp.global_data.pdebug) then begin
          Buffer.add_string b ("Arg " ^ string_of_int n ^ " = ");
          buffer_add_ocs b s; Buffer.add_string b "\n";
        end;
        let v1:Ocs_types.sval = Ocs_sym.get_symbol ("_" ^ string_of_int n) in
        Ocs_env.set_glob env v1 s;
        aux t1 t2 (n+1)
    in
    aux rhs avl 1;
    if !(dyp.global_data.pdebug) then
    Buffer.add_string b "End of arguments\n";
    let sr = getsr dyp2 in
(*
print_endline ("sr of reduction is " ^ Flx_srcref.short_string_of_src sr);
*)
    begin
      let v1:Ocs_types.sval = Ocs_sym.get_symbol ("_sr") in
      (*
      let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = ssr } in
      *)
      Ocs_env.set_glob env v1 (xsr sr)
    end
    ;
    let r =
      try scheme_eval cde
      with Ocs_error.Error err | Ocs_error.ErrorL (_,err) ->
        print_string (Buffer.contents b);
        print_string ("Error "^err^" evaluating " ^ scm);
        failwith "Error evaluating Scheme"
    in
    `Obj_sexpr (!age,sr,r),[]
  in
  rule,action,Bind_to_cons [(name, "Obj_sexpr")]

(* top routine to add a new production to a dssl *)
let extend_grammar dyp (dssl,(name,prio,prod,action,anote,sr)) =
  let m = dyp.local_data in
  let dssl_record = Drules.find dssl m.dssls in
  define_scheme sr dyp dssl_record dssl name prio prod action

(* ------------------------------------------------------ *)
let dflt_action kind prod =
  let rn = ref 1 in
  let action =
    match kind with
    | `Sval ->
      "`(" ^
      List.fold_left (fun acc _ -> let n = !rn in incr rn;
        (if acc = "" then "" else acc ^ " ") ^ ",_" ^ string_of_int n
      ) "" prod
      ^ ")"

    | `String ->
      "(strcat `(" ^
      List.fold_left (fun acc _ -> let n = !rn in incr rn;
        (if acc = "" then "" else acc ^ " ") ^ ",_" ^ string_of_int n
      ) "" prod
      ^ "))"
  in
  (*
  print_endline ("DEFAULT ACTION for " ^
    catmap " " silly_strtoken prod ^
    " =># " ^ action)
  ;
  *)
  action

let cal_action kind prod action =
  match action with
  | `None -> dflt_action kind prod
  | `Scheme scm -> scm

let rec flatten sr pcounter kind rhs =
  let rec aux inp out extras = match inp with
  | [] -> List.rev out,extras

  | `Group dyalts :: t ->
    let x = string_of_int (!pcounter) in incr pcounter;
    let sl = "__grp_" ^ x in
    let rules = fixup_alternatives pcounter kind sl `Default dyalts in
    aux t (NONTERMINAL (sl, `No_prio)::out) (rules@extras)

  | `Atom h :: t -> aux t (h::out) extras
  in aux rhs [] []

and fixup_rule sr pcounter kind rhs =
  let rhs,extras = flatten sr pcounter kind rhs in
  let rhs, extras =
    fixup_prio sr rhs,
    List.map (fun (name,prio,prod,action,anote,sr) -> name,prio,fixup_prio sr prod, action,anote,sr) extras
  in
  let rhs,extras' = fixup_suffix sr pcounter kind rhs in
  rhs,extras@extras'

and fixup_alternatives pcounter kind name prio dyalts =
  let rules =
    List.fold_left
      (fun rules (rhs,sr,action,anote) ->
        let prod,extras = fixup_rule sr pcounter kind rhs in
        let action : string = cal_action kind prod action in
        ((name,prio,prod,action,anote,sr) :: extras) @ rules
      )
      [] dyalts
  in
  List.rev rules

let add_rule global_data local_data dssl rule =
  let m = local_data in
  let d = try Drules.find dssl m.dssls with Not_found -> fresh_dssl in
  match rule with
  | `Scheme_rule (privacy,name,prio,kind,dyalts) ->
     let rules = fixup_alternatives global_data.pcounter kind name prio dyalts in
     let rules = List.fold_left (fun acc rule -> uniq_add rule acc) d.rules rules in
     let privacy =
       match privacy with
       | `Private ->
          let n = !(global_data.pcounter) in incr (global_data.pcounter);
          let secret = "_"^name^"_"^string_of_int n in
          Drules.add name secret d.privacy
       | `Public -> d.privacy
     in
     let d = { d with rules = rules; privacy = privacy } in
     let m = { m with dssls = Drules.add dssl d m.dssls } in
     global_data,m

  | `Requires ls ->
     let d = { d with deps = ls @ d.deps } in
     let m = { m with dssls = Drules.add dssl d m.dssls } in
     global_data,m

  | `Priorities p ->
    let d = { d with prios = p::d.prios } in
    let m = { m with dssls = Drules.add dssl d m.dssls } in
    global_data, m

let ocs2flx sr r =
  let sex = Ocs2sex.ocs2sex r in
  (*
  print_endline "OCS scheme term converted to s-expression:";
  Sex_print.sex_print sex;
  *)
  let flx = Flx_sex2flx.xstatement_t sr sex in
  (*
  print_endline "s-expression converted to Felix statement!";
  print_endline (string_of_statement 0 flx);
  *)
  flx


