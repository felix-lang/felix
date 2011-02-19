open Flx_token
open Flx_ast
open Flx_util
open List
open Flx_print

let tc_doc = Hashtbl.create 97
let module_doc = Hashtbl.create 97

let record_tc name (vs,sts) =
  Hashtbl.add tc_doc name (vs,sts)

let record_module name (vs,sts) =
  Hashtbl.add module_doc name (vs,sts)

let anchor s =
  "<A NAME=\""^s^"\">"

let href a s =
  "<A HREF=\""^a^"\">" ^ s ^ "</A>"

let href_page s = href (s^".html") s

let span cl s =
  "<SPAN CLASS=\""^cl^"\">"^s^"</SPAN>"

let begin_div cl =
  "<DIV CLASS=\""^cl^"\">"

let end_div = "</DIV>"

let st t = Flx_print.string_of_typecode t

let gen_tcdoc () =
    let _ = try Unix.mkdir "doc" 0o775 with _ -> () in
    let _ = try Unix.mkdir "doc/typeclassdoc" 0o775 with _ -> () in

    Hashtbl.iter (* all pages *)
    (fun name (vs,sts) ->
      let f = open_out ("doc/typeclassdoc/" ^ name ^ ".html") in
      let w s = output_string f s in
      w "<html><head>\n";
      w "<meta content=\"text/html; charset=utf8\" http-equiv=\"Content-Type\">\n";
      w "<link rel=stylesheet href=\"typeclassdoc.css\" type=\"text/css\">\n";
      w ("<title>" ^ name^ "</title>\n");
      w "</head>\n";
      w "<body>";
      w ("<h1>" ^ name ^ "</h1>\n");
      w (begin_div "rule" ^ ("typeclass " ^ name ^ Flx_print.string_of_vs vs) ^ end_div);
      iter (fun stmt -> match stmt with

      | STMT_inject_module (sr,qn) ->
        begin match qn with
        | `AST_name (_,name,ts) ->
          w (begin_div "element" ^
            "   inherit " ^ href_page name ^ "["^ catmap "," st ts^"]" ^
            end_div
          )
        | _ ->
          w (begin_div "element" ^
            "   inherit " ^ Flx_print.string_of_qualified_name qn ^
            end_div
          )
        end

      | STMT_fun_decl (sr,name,vs,args,result,code,reqs,prec)
        when code = Flx_code_spec.Virtual ->
        begin match result with
        | TYP_void _ ->
          w (begin_div "element" ^
            span "category" "   virtual proc " ^ span "ident" name ^ Flx_print.string_of_vs vs ^ ": " ^
            catmap "*" st args ^
            end_div
          )
        | _ ->
          w (begin_div "element" ^
            span "category" "   virtual fun " ^ span "ident" name ^ Flx_print.string_of_vs vs ^ ": " ^
            catmap "*" st args ^ " -> " ^ st result ^
            end_div
          )
        end

      | x -> w (begin_div "element" ^ (Flx_print.string_of_statement 4 x) ^ end_div)
      )
      sts
      ;
      w "</body></html>\n";
      close_out f
    )
    tc_doc
    ;
    let tc_list = Hashtbl.fold (fun k _ acc -> k::acc) tc_doc [] in
    let tc_list = List.sort compare tc_list in
    let f = open_out ("doc/typeclassdoc/index.html") in
    let w s = output_string f s in
    w "<html><head>\n";
    w "<meta content=\"text/html; charset=utf8\" http-equiv=\"Content-Type\">\n";
    w "<link rel=stylesheet href=\"typeclassdoc.css\" type=\"text/css\">";
    w ("<title>Felix Typeclasses</title>\n");
    w "</head>\n";
    w "<body>\n";
    w "<H1>Typeclass Index</H1>";
    w (begin_div "index");
    List.iter
    (fun name ->
      w (" " ^ href_page name ^ "\n");
    )
    tc_list
    ;
    w end_div;
    w "</body></html>\n";
    close_out f

let counter = ref 0

let gen_mdoc () =
    let _ = try Unix.mkdir "doc" 0o775 with _ -> () in
    let _ = try Unix.mkdir "doc/moduledoc" 0o775 with _ -> () in

    Hashtbl.iter (* all pages *)
    (fun name (vs,sts) ->
      let f = open_out ("doc/moduledoc/" ^ name ^ ".html") in
      let w s = output_string f s in
      w "<html><head>\n";
      w "<meta content=\"text/html; charset=utf8\" http-equiv=\"Content-Type\">\n";
      w "<link rel=stylesheet href=\"moduledoc.css\" type=\"text/css\">\n";
      w ("<title>" ^ name^ "</title>\n");
      w "<script src=\"toggle.js\"<script>";
      w "</head>\n";
      w "<body>";
      w ("<h1>" ^ name ^ "</h1>\n");
      w (begin_div "rule" ^ ("module " ^ name ^ Flx_print.string_of_vs vs) ^ end_div);
      iter (fun stmt -> match stmt with

      | STMT_inject_module (sr,qn) ->
        begin match qn with
        | `AST_name (_,name,ts) ->
          w (begin_div "element" ^
            "   inherit " ^ href_page name ^ "["^ catmap "," st ts^"]" ^
            end_div
          )
        | _ ->
          w (begin_div "element" ^
            "   inherit " ^ Flx_print.string_of_qualified_name qn ^
            end_div
          )
        end

      | STMT_const_decl (sr,name,vs,typ,code,reqs) ->
        let seq = !counter in incr counter;
        let id = "ref_"^string_of_int seq in
        w (begin_div "element" ^
          span "category" "const " ^ span "ident" name ^
          Flx_print.string_of_vs vs ^
          ": " ^ string_of_typecode typ
        );
        w ("&nbsp;&nbsp;&nbsp; <IMG SRC=\"plus.gif\""^id^
        " ONCLICK=\"toggle(this,'"^id^"')\" ALT=\"+\">" ^
        " <DIV CLASS=implementation ID="^id^" style=\"display:none\">");
        w (string_of_code_spec code ^ "\n");
        w (string_of_raw_reqs reqs ^ "\n");
        w "</DIV>";
        w end_div;

      | STMT_insert (_,name,vs,code, ikind, reqs) ->
        let seq = !counter in incr counter;
        let id = "ref_"^string_of_int seq in
        w (begin_div "element" ^
          span "category" (string_of_ikind ikind) ^
          span "ident" name ^
          Flx_print.string_of_vs vs
        );
        w ("&nbsp;&nbsp;&nbsp; <IMG SRC=\"plus.gif\""^id^
        " ONCLICK=\"toggle(this,'"^id^"')\" ALT=\"+\">" ^
        " <DIV CLASS=implementation ID="^id^" style=\"display:none\">");
        w (string_of_code_spec code ^ "\n");
        w (string_of_raw_reqs reqs ^ "\n");
        w "</DIV>";
        w end_div;

      | STMT_abs_decl (_,name,vs, quals, code, reqs) ->
        let seq = !counter in incr counter;
        let id = "ref_"^string_of_int seq in
        w (begin_div "element" ^
          (match quals with [] ->"" | _ -> string_of_quals quals ^ " ") ^
          span "category" "type " ^
          span "ident" name ^
          string_of_vs vs
        );
        w ("&nbsp;&nbsp;&nbsp; <IMG SRC=\"plus.gif\""^id^
        " ONCLICK=\"toggle(this,'"^id^"')\" ALT=\"+\">" ^
        " <DIV CLASS=implementation ID="^id^" style=\"display:none\">");
        w (string_of_code_spec code ^ "\n");
        w (string_of_raw_reqs reqs ^ "\n");
        w "</DIV>";
        w end_div;

      | STMT_fun_decl (sr,name,vs,args,result,code,reqs,prec)  ->
        let seq = !counter in incr counter;
        let id = "ref_"^string_of_int seq in
        begin match result with
        | TYP_void _ ->
          w (begin_div "element" ^
            span "category" "proc " ^ span "ident" name ^
            Flx_print.string_of_vs vs ^ ": " ^
            catmap "*" st args
          )
        | _ ->
          w (begin_div "element" ^
            span "category" "fun " ^ span "ident" name ^ Flx_print.string_of_vs vs ^ ": " ^
            catmap "*" st args ^ " -> " ^ st result
          )
        end
        ;
        w ("&nbsp;&nbsp;&nbsp; <IMG SRC=\"plus.gif\""^id^
        " ONCLICK=\"toggle(this,'"^id^"')\" ALT=\"+\">" ^
        " <DIV CLASS=implementation ID="^id^" style=\"display:none\">");
        w (string_of_code_spec code ^ "\n");
        w (string_of_raw_reqs reqs ^ "\n");
        w "</DIV>";
        w end_div;


      | STMT_curry (_,name, vs, pss, (res,traint) , kind, ss) ->
        let s =
          span "category"
          (match kind with
          | `Function -> (match res with TYP_void _ -> "proc " | _ -> "fun ")
          | `CFunction -> (match res with TYP_void _ -> "cproc " | _ -> "cfun ")
          | `InlineFunction -> (match res with TYP_void _ -> "inline proc " | _ -> "inline fun ")
          | `NoInlineFunction -> (match res with TYP_void _ -> "noinline proc " | _ -> "noinline fun ")
          | `Virtual -> (match res with TYP_void _ -> "virtual proc " | _ -> "virtual fun ")
          | `Ctor -> "ctor "
          | `Generator -> "generator "
          )
          ^
          span "ident" name ^ string_of_vs vs ^
          catmap " "
          (fun ps ->
            "("^string_of_parameters ps^")"
          )
          pss
          ^
          ": "^string_of_typecode res^
          (match traint with
          | None -> ""
          | Some x -> " when " ^ string_of_expr x
          )
        in

        w (begin_div "element");
        let seq = !counter in incr counter;
        let id = "ref_"^string_of_int seq in
        begin match ss with
        | [STMT_fun_return (_,e)] ->
          w s;
          w ("&nbsp;&nbsp;&nbsp; <IMG SRC=\"plus.gif\""^id^
          " ONCLICK=\"toggle(this,'"^id^"')\" ALT=\"+\">" ^
          " <DIV CLASS=implementation ID="^id^" style=\"display:none\">");
          w (" => " ^ string_of_expr e ^ ";\n");
          w "</DIV>"

        | _ ->
          w s;
          w ("&nbsp;&nbsp;&nbsp; <IMG SRC=\"plus.gif\""^id^
          " ONCLICK=\"toggle(this,'"^id^"')\" ALT=\"+\">" ^
          " <DIV CLASS=implementation ID="^id^" style=\"display:none\">");
          iter (fun s -> w (string_of_statement 0 s ^ "\n")) ss;
          w "</DIV>"
        end;
        w end_div;

      | STMT_instance (_,vs,name, sts) ->
        w (begin_div "element");
        let seq = !counter in incr counter;
        let id = "ref_"^string_of_int seq in
        w (span "category" "instance " ^ string_of_vs vs ^ " " ^
        span "ident" (string_of_qualified_name name) ^ " = ");
        w ("&nbsp;&nbsp;&nbsp; <IMG SRC=\"plus.gif\""^id^
        " ONCLICK=\"toggle(this,'"^id^"')\" ALT=\"+\">" ^
        " <DIV CLASS=implementation ID="^id^" style=\"display:none\">");
        iter (fun s -> w (string_of_statement 0 s ^ "\n")) sts;
        w "</DIV>";
        w end_div;

      | x -> w (begin_div "element" ^ (Flx_print.string_of_statement 0 x) ^ end_div)
      )
      sts
      ;
      w "</body></html>\n";
      close_out f
    )
    module_doc
    ;
    let tc_list = Hashtbl.fold (fun k _ acc -> k::acc) module_doc [] in
    let tc_list = List.sort compare tc_list in
    let f = open_out ("doc/moduledoc/index.html") in
    let w s = output_string f s in
    w "<html><head>\n";
    w "<meta content=\"text/html; charset=utf8\" http-equiv=\"Content-Type\">\n";
    w "<link rel=stylesheet href=\"moduledoc.css\" type=\"text/css\">";
    w ("<title>Felix Modules</title>\n");
    w "<script src=\"toggle.js\"<script>";
    w "</head>\n";
    w "<body>\n";
    w "<H1>Module Index</H1>";
    w (begin_div "index");
    List.iter
    (fun name ->
      w (" " ^ href_page name ^ "\n");
    )
    tc_list
    ;
    w end_div;
    w "</body></html>\n";
    close_out f

let gen_doc() =
  gen_tcdoc();
  gen_mdoc()
