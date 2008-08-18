open Flx_util
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_name
open Flx_exceptions
open Flx_display
open List
open Flx_maps
open Flx_egen
open Flx_pgen
open Flx_ctorgen

let gen_elk_lexer filebase module_name syms bbdfns this sr ((_,t') as e) n  =
  let lexer_name = "ElkLex_"^si n in
  let ge e = gen_expr syms bbdfns this e [] [] sr in
  let tn t = cpp_typename syms t in
  let get_token_fun_type = tn t' in

  let display = cal_display syms bbdfns (Some this) in
  let frame_dcls =
    "  FLX_FMEM_DECL\n"
  in
  let display_string =
    cat ""
    (
      map
      (fun (i, vslen) ->
       try
       let instname = cpp_instance_name syms bbdfns i [] in
       "  " ^ instname ^ " *ptr" ^ instname ^ ";\n"
       with _ -> failwith "Can't cal display name"
       )
      display
    )
  and ctor_dcl =
    "  "^lexer_name ^ "(\n" ^
    "    FLX_FPAR_DECL\n" ^
    cat ""
    (
      map
      (
        fun (i,vslen) ->
        let instname = cpp_instance_name syms bbdfns i [] in
        "    " ^ instname ^ "*,\n"
      )
      display
    )^
    "    "^get_token_fun_type ^"\n  );\n"
  in
  let filename = filebase ^ "_lexer_" ^ si n ^ ".hpp" in
  if syms.compiler_options.print_flag then
  print_endline ("Generating Elkhound lexer " ^ lexer_name ^ " in " ^ filename);

  let f = open_out filename in
  let pe s = output_string f (s ^ "\n") in

  let token_type, token_type_name, token_id, cts =
    match t' with
    | `BTYP_function (`BTYP_tuple [],`BTYP_inst(i,[])) ->
      let id,parent,sr',entry = Hashtbl.find bbdfns i in
      let token_type = `BTYP_inst(i,[]) in
      let token_type_name = tn token_type in
      begin match entry with
      | `BBDCL_union ([],cts) -> token_type, token_type_name, id, cts
      | _ -> assert false
      end
    | _ -> assert false
  in
  pe ("#ifndef ELKLEX_"^si n);
  pe ("#define ELKLEX_"^si n);
  pe "#include \"elk_lexerint.h\"";
  pe "";
  pe ("struct "^lexer_name^": public LexerInterface {");
  pe ("  //frame");
  pe frame_dcls;
  pe ("  //display");
  pe display_string;
  pe ("  // constructor");
  pe ctor_dcl;
  pe ("  " ^ get_token_fun_type ^ " get_token; // client token generator");
  pe ("  gc_profile_t &gcp; // Felix garbage collector");
  pe "  void setToken();  //fetch next token ";
  pe ("  "^lexer_name^" *init(); //prime the lexer");
  pe "";
  pe "  //Elkhound API";
  pe "  static void nextToken(LexerInterface *lex);";
  pe "  NextTokenFunc getTokenFunc() const { return &nextToken; }";
  pe "  sm_string tokenDesc() const;";
  pe "  sm_string tokenKindDesc(int kind) const;";
  pe "};";
  pe "#endif";
  close_out f;

  let filename = filebase ^ "_lexer_" ^ si n ^ ".cpp" in
  let f = open_out filename in
  let pe s = output_string f (s ^ "\n") in
  pe ("#include \""^module_name^"_lexer_"^si n^".hpp\"");
  pe ("//token type = " ^ token_type_name);
  pe ("static char const *"^token_id^"_desc["^si (length cts)^"]={");
  iter (fun (nm,_,_) -> pe ("   \""^nm^"\",")) cts;
  pe ("};");
  pe "";
  (* FUDGE PROPERTY LIST *)
  let props : property_t list = [`Uses_gc; `Requires_ptf] in
  pe (gen_ctor syms bbdfns lexer_name display [] [get_token_fun_type,"get_token"] ["gcp(*PTF gcp)"] [] props);
  pe ("sm_string " ^ lexer_name ^ "::tokenDesc() const { return tokenKindDesc(type); }");
  pe "";
  pe ("sm_string " ^ lexer_name ^ "::tokenKindDesc(int kind) const {");
  pe ("  return "^token_id^"_desc[kind];");
  pe ("}");
  pe "";
  pe ("void " ^ lexer_name ^ "::setToken() {");
  pe ("  _uctor_ token = get_token->apply();");
  pe ("  if(token.data) gcp.collector->add_root(token.data);");
  pe ("  type = token.variant;");
  pe ("  sval =  (SemanticValue)token.data;");
  pe ("}");
  pe "";
  pe ("void " ^ lexer_name ^ "::nextToken(LexerInterface *lex) {");
  pe ("  (("^lexer_name^"*)lex)->setToken();");
  pe ("}");
  pe "";
  pe (lexer_name^" *"^lexer_name^"::init(){");
  pe ("  nextToken(this);");
  pe ("  return this;");
  pe ("}");

  close_out f

let gen_elk_parser filebase module_name syms bbdfns this sr t' n ii =
  let filename = filebase ^ "_parser_" ^ si n ^ ".gr" in
  let parser_name = "_" ^ si n in
  if syms.compiler_options.print_flag then
  print_endline ("Generating Elkhound parser " ^ filename)
  ;
  let f = open_out filename in
  let pe s = output_string f (s ^ "\n") in
  let ps s = output_string f s in
  let ge_arg this ((x,t) as e) =
    match t with
    | `BTYP_tuple [] -> ""
    | _ -> gen_expr syms bbdfns this e [] [] sr
  in
  let tn t = cpp_typename syms (reduce_type t) in
  let string_of_bprod (n,g) =
    (match n with | None -> "" | Some n -> cid_of_flxid n ^ ":") ^
    (match g with
    | `Term k ->
      (match Hashtbl.find syms.dfns k with {id=id}->cid_of_flxid id)
    | `Nonterm (k::_) ->
      (match Hashtbl.find syms.dfns k with {id=id}->cid_of_flxid id)
    | _ -> assert false
    )
  in
  let print_production (this,p,xs) =
    match xs with
    | [`BEXE_fun_return (_,((_,t) as e))] ->
      let tname = tn t in
      ps ("  -> ");
      ps (catmap " " string_of_bprod p);
      pe "";
      pe "    {";
      pe ("       "^tname^" *_x = new(gcp,"^shape_of syms bbdfns tn t^",true)");
      pe ("       "^tname^"(" ^ ge_arg this e ^ ");");
      pe ("       gcp.collector->add_root(_x);");
      iter
      (function
        (* | Some n, `Nonterm _ -> pe ("       gcp.collector->remove_root(" ^ n^");") *)
        | Some n, _ -> pe ("       gcp.collector->remove_root(" ^ n^");")
        | _ -> ()
      )
      p;
      pe ("       return _x;");
      pe "    }";
    | _ -> assert false
  in
  let set_of_list ii : IntSet.t = fold_left (fun s elt ->IntSet.add elt s) IntSet.empty ii in
  let nts_of_prod p : IntSetSet.t =
    fold_left
    (fun x (_,k) -> match k with
      | `Nonterm ii -> IntSetSet.add (set_of_list ii) x
      | `Term _ -> x
    )
    IntSetSet.empty
    p
  in
  let prod_of_glr i =
    try
    match Hashtbl.find bbdfns i with
    | _,_,_,`BBDCL_glr (_,_,_,(p,_)) -> p
    | id,_,_,entry -> failwith
      ("Expected "^si i^"->BBDCL_glr, got " ^ string_of_bbdcl syms.dfns bbdfns entry i)

    with Not_found -> failwith ("Can't find BBDCL_glr " ^ si i)
  in
  let nts_of_glr i : IntSetSet.t = nts_of_prod (prod_of_glr i) in
  let nt_uses x : IntSetSet.t =
    IntSet.fold
    (fun i nts ->
      IntSetSet.union nts (nts_of_glr i)
    )
    x
    IntSetSet.empty
  in
  let make_closure ii =
    let been_done = ref (IntSetSet.singleton (set_of_list ii)) in
    let to_do = ref (nt_uses (set_of_list ii)) in
    while not (IntSetSet.is_empty !to_do) do
      let x = IntSetSet.choose !to_do in
      to_do := IntSetSet.remove x !to_do;
      if not (IntSetSet.mem x !been_done) then begin
        been_done := IntSetSet.add x !been_done;
        to_do := IntSetSet.union !to_do (nt_uses x)
      end
    done;
    !been_done
  in
  let print_nonterm x =
    let j = IntSet.choose x in
    let id,parent,sr'',entry = Hashtbl.find bbdfns j in
    begin match entry with
    | `BBDCL_glr (_,_,t,(p,xs)) ->
      let tt = tn t in
      pe ("nonterm("^tt^"*) "^cid_of_flxid id^" {");
      pe ("  fun dup(x) { "^tn t^" *d = new(gcp,"^shape_of syms bbdfns tn t^ ",true)" ^ tt ^ "(*x);");
      pe ("  gcp.collector->add_root(d); return d; }");
      pe ("  fun del(x) { gcp.collector->remove_root(x); }");
      IntSet.iter (fun i ->
        let id,parent,sr'',entry = Hashtbl.find bbdfns i in
        match entry with
        | `BBDCL_glr (_,_,t,(p,xs)) -> print_production (i,p,xs)
        | _ -> assert false
      )
      x;
      pe "}";
    | _ -> assert false
    end
  in
  let display = cal_display syms bbdfns (Some this) in
  let frame_dcls =
    "  FLX_FMEM_DECL"
  in
  let display_string =
    cat ""
    (
      map
      (fun (i,vslen) ->
       try
       let instname = cpp_instance_name syms bbdfns i [] in
       "  " ^ instname ^ " *ptr" ^ instname ^ ";\n"
       with _ -> failwith "Can't cal display name"
       )
      display
    )
  and ctor_dcl =
    "  Elk" ^parser_name^
    (if length display = 0
    then "(FLX_FPAR_DECL_ONLY);\n"
    else (
    "  (\n" ^
    "    FLX_FPAR_DECL\n " ^
    cat ",\n"
      (
        map
        (
          fun (i,vslen) ->
          let instname = cpp_instance_name syms bbdfns i [] in
          "    " ^ instname ^ "*"
        )
        display
      )^
      "\n  );\n"
    ))
  in
    begin match t' with
    | `BTYP_function (`BTYP_tuple [],`BTYP_inst(i,[])) ->
      let token_id,parent,sr',entry = Hashtbl.find bbdfns i in
      let token_type = `BTYP_inst(i,[]) in
      let token_type_name = tn token_type in
      begin match entry with
      | `BBDCL_union ([],cts) ->
        let j = hd ii in
        let id,parent,sr'',entry = Hashtbl.find bbdfns j in
        begin match entry with
        | `BBDCL_glr (props,_,t,(p,xs)) ->
          let result_type = tn t in
          pe ("//Elkhound parser Elk" ^ parser_name ^ " -> " ^ result_type);
          pe ("//Token type " ^ token_id ^ " -> " ^ token_type_name);
          pe "terminals {";
          let i = ref 0 in
          iter (fun (id,j,t) ->
            pe ("  " ^ si j^" : "^ cid_of_flxid id ^ ";")
          )
          cts;

        pe "";
        iter (fun (id,_,t) ->
          if t <> `BTYP_void then begin
            pe ("  token("^tn t^"*) " ^ cid_of_flxid id ^ "{");
            pe ("    fun dup(x) { return x; }");
            pe ("    fun del(x) {}");
            pe ("}");
          end
        )
        cts;

        pe "}";
        pe "";
        pe ("context_class Elk"^parser_name^": public UserActions {");
        pe ("public:");
        pe frame_dcls;
        ps display_string;
        pe ctor_dcl;
        pe ("  gc_profile_t &gcp;");
        pe
        (
          (if t = `BTYP_tuple [] then "int" else "_uctor_") ^
          " apply(LexerInterface *lex);"
        );
        pe "};";
        pe "";
        pe "impl_verbatim {";
        pe (gen_ctor syms bbdfns ("Elk"^parser_name) display [] [] ["gcp(*PTF gcp)"] [] props);
        pe "}";
        pe "";
        pe "impl_verbatim {";
        pe "// Felix function to apply the parser to a lexer";
        pe "// This returns a polymorphic option";
        pe "// case 0- Parse failed";
        pe "// case 1- Argument contains parser result";
        pe ("// Type of parser result is " ^ sbt syms.dfns t);

        pe
        (
          (if t = `BTYP_tuple [] then "int" else "_uctor_") ^
          " Elk"^parser_name^"::apply(LexerInterface *lex) {"
        );
        pe "  _uctor_ result(0,0);";
        pe "  SemanticValue p=(SemanticValue)(void*)0;";
        pe "  GLR glr(this,this->makeTables());";
        pe "  glr.noisyFailedParse = true;";
        pe "  result.variant = glr.glrParse(*lex,p);";
        pe "";
        pe "  if(result.variant==1)";

        if t = `BTYP_tuple [] then begin
        pe "  gcp.collector->remove_root((void*)p);";
        pe "  return result.variant;";
        end else begin
        pe ("    result.data =");
        pe ("      new(gcp,"^shape_of syms bbdfns tn t^",true)");
        pe ("     "^result_type^"(*("^result_type^"*)(void*)p)");
        pe ("  ;");
        pe ("  gcp.collector->remove_root((void*)p);");
        pe "  return result;";
        end;
        pe "}";
        pe "}";
        pe "";

        pe ("nonterm("^result_type^"*) elk"^parser_name^" {");
        print_production (j,p,xs);
        iter (fun i ->
          let id,parent,sr'',entry = Hashtbl.find bbdfns i in
          match entry with
          | `BBDCL_glr (_,vs,t,(p,xs)) -> print_production (i,p,xs)
          | _ -> assert false
        )
        (tl ii)
        ;
        pe "}";
        let cls = make_closure ii in
        IntSetSet.iter print_nonterm cls;
        pe "//End grammar"

      | _ -> assert false (* must be glr *)
      end

    | _ ->
      clierr sr
      "Parser function must have unit domain and return a non-polymorphic union"
    end
  | _ ->
    clierr sr
    "Parser function must have unit domain and return a non-polymorphic union"
  end
  ;
  close_out f
  ;
  let elkhound = syms.compiler_options.elkhound in
  let retval = Unix.system(elkhound ^ " -tr nolines " ^ filename) in
  begin match retval with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith "Error executing flx_elkhound"
  end
