Package: src/packages/flx_doc.fdoc


==========================
Felix documentation tools.
==========================

=================== ==================================
key                 file                               
=================== ==================================
flx_gramdoc.flx     $PWD/src/tools/flx_gramdoc.flx     
flx_libcontents.flx $PWD/src/tools/flx_libcontents.flx 
flx_libindex.flx    $PWD/src/tools/flx_libindex.flx    
flx_mktutindex.flx  $PWD/src/tools/flx_mktutindex.flx  
flx_fdoc2sphinx.flx $PWD/src/tools/flx_fdoc2sphinx.flx 
=================== ==================================


Documentation tools for Felix.
==============================

These tools are designed to extract and build
documentation from Felix libraries. Most no
longer work properly due to the move to packaging
technology and require upgrading.


Document the Grammar.
=====================

Generates an index of non-terminals used in
the grammar.

.. code-block:: felix

  //[flx_gramdoc.flx]
  var ishtml = System::argv 1 == "--html";
  var dir =  Filename::join ("src", "lib", "grammar");
  var fregex = ".*\\.flxh";
  
  open Regdef;
  regdef anychar = perl (".");
  
  regdef letter = charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  regdef digit = charset "0123456789";
  regdef id1 = letter | "_";
  regdef id2 = id1 | digit | "-" | "'";
  regdef id = id1 id2* "?"?;
  
  regdef spaces = " "*;
  regdef prio =  "[" id "]";
   
  regdef production = group(spaces ? id prio ? spaces ? ":=" spaces ? anychar*) "=>#" anychar*;
  regdef dssl = spaces group ("syntax" spaces id) anychar*;
  
  var lregex = (regexp (dssl | production)) . render;
  var lgrep = RE2 lregex;
  
  var n = NumberOfCapturingGroups(lgrep)+1;
  var v = varray[StringPiece] (n.size,StringPiece "");
  
  var scomment = RE2 " *//[$] (.*)";
  var vcomment = varray[StringPiece] (2.size, StringPiece "");
  
  if ishtml do
    println$ "<html><body>";
    println$ "<h1>Felix Syntax</h1>";
    println$ "<pre>";
  done
  
  for file in FileSystem::regfilesin (dir, fregex) do
    var href = "/share/lib/grammar/"+file; // URL always uses Unix filenames
    if ishtml do
      println$ '<hr/><a href="'+href+'">'+file+'</a>';
    else
      println$ "-" * 20;
      println$ file;
    done
    var lines = load (Filename::join dir file);
    var count = 0;
    var comments = Empty[string];
    for line in split (lines,char "\n") do
      ++count;
      var commentry = Match (scomment, StringPiece line, 0, ANCHOR_BOTH, vcomment.stl_begin, 2);
      if commentry do
         comments = Cons (vcomment . 1 . string.strip, comments);
      else
  
        var m = Match (lgrep, StringPiece line, 0, ANCHOR_BOTH, v.stl_begin,n); 
        if m do
          var syn = v.1.string.strip;
          var prod = v.2.string.strip;
          if ishtml do
            if syn != "" do
              println$ "";
              println$  f"%04d" count + ":  " + '<a href="'+href+'#'+f"%04d" count+'">'+syn+'</a>';
              for cline in rev comments do println$ "           "+cline; done
              comments = Empty[string];
            else
              println$ f"%04d" count + ":    " + '<a href="'+href+'#'+f"%04d" count+'">'+ prod +'</a>';
              for cline in rev comments do println$ "           "+ cline; done
              comments = Empty[string];
            done
          else
            if syn != "" do
              println$ "";
              println$ f"%04d" count + ":  " + syn;
              for cline in rev comments do println$ "           "+cline; done
              comments = Empty[string];
            else
              println$ f"%04d" count + ":    " + prod;
              for cline in rev comments do println$ "           "+ cline; done
              comments = Empty[string];
            done
          done // html
        done
      done
    done
  done
  
  if ishtml do
    println$ "</pre></body></html>";
  done
  


Library contents table.
-----------------------

Lists symbols per file.

.. code-block:: felix

  //[flx_libcontents.flx]
  var ishtml = System::argv 1 == "--html";
  var dir =  Filename::join ("src", "lib", "std");
  
  include "plugins/fdoc-interface";
  var  xlat_fdoc = Dynlink::load-plugin-func2 [fdoc_t, string, string] (
      dll-name="fdoc2html", setup-str="", entry-point="fdoc2html"
    );
  
  
  var fregex = ".*\\.(flx|fdoc)";
  open Regdef;
  regdef anychar = perl (".");
  
  regdef letter = charset "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  regdef digit = charset "0123456789";
  regdef id1 = letter | "_";
  regdef id2 = id1 | digit | "-" | "'";
  regdef id = id1 id2*;
  
  regdef tex = "\\" letter*;
  regdef symbol1 = "+-*/%^";
  regdef symbol = symbol1 | symbol1 symbol1 | symbol1 symbol1 symbol1;
  regdef name = id | symbol;
  regdef spaces = " "*;
  regdef vlist =  "[" spaces id (spaces "," spaces id)* spaces "]";
   
  regdef adjective = "pure" | "inline" | "noinline" | "pod" | "open" | "virtual";
  regdef binder = "fun" | "proc" | "gen" | "class" | "union" | "struct" | "type" | "typedef" | "ctor" (spaces vlist)?;
  
  regdef indent2 = "  ";
  
  regdef classbind= group ("class" | "open class");
  regdef otherbind= indent2 ? group (adjective* spaces binder);
  
  // Group 1 = class
  // Group 2 = other 
  // group 3 = identifier
  regdef decl = (classbind | otherbind) spaces group (name) anychar*;
  
  var emptystring = "";
  var emptystringpiece = StringPiece emptystring;
  
  var lregex = decl . render;
  var lgrep = RE2 lregex;
  var n = NumberOfCapturingGroups(lgrep)+1;
  var v = varray[StringPiece] (n.size,emptystringpiece);
  
  var extract = RE2 " *([^={]*) *(=|{|;).*";
  var n2 = NumberOfCapturingGroups(extract)+1;
  var v2 = varray[StringPiece] (n2.size,emptystringpiece);
  
  var scomment = RE2 " *//[$](.*)";
  var vcomment = varray[StringPiece] (2.size, emptystringpiece);
  
  if ishtml do
    println$ "<html><body>";
    println$ "<h1>Felix Library Contents</h1>";
  done
  
  var files = FileSystem::regfilesin (dir, fregex);
  files = files.sort;
  
  for file in files do
    var href = "/share/lib/std/"+file; // URL always uses Unix filenames
    if ishtml do
      println$ '<hr/><a href="'+href+'">'+file+'</a>';
    else
      println$ file;
    done
    var lines = load (Filename::join dir file);
    var count = 0;
    var comments = Empty[string];
    for line in split (lines,char "\n") do
      ++count;
      var spl = StringPiece line;
      var commentry = Match (scomment, spl, 0, ANCHOR_BOTH, vcomment.stl_begin, 2);
      if commentry do
         comments = Cons (vcomment . 1 . string, comments);
      else
  
        match lgrep line with
        | Some v =>
          var sym = v.3;
          var dfn = "";
          var m2 = Match (extract, spl, 0, ANCHOR_BOTH, v2.stl_begin, n2);
          if m2 do
            dfn = v2 . 1 . string . strip;
          else
            dfn = line . strip;
          done
          if ishtml do
            if prefix (dfn, "class") or prefix (dfn, "open class") do
              println$ "";
              println$  "<pre>"+ f"%04d" count + ":  " + '<a href="'+href+'#'+f"%04d" count+'">'+dfn +'</a></pre>';
              //for cline in rev comments do println$ "           "+cline; done
              var txt = "";
              for cline in rev comments do txt += cline+"\n"; done
              var result = xlat_fdoc (txt, "dummy");
              var html = #(result.html_raw);
              if txt != "" do 
                println$ "<div style='font-family:sans-serif; font-size:12pt; "+
                "margin-left:100; margin-right:100; top:5; color:#406040'>" + html + "</div>"; 
              done
              comments = Empty[string];
            else
              println$ "<pre>"+f"%04d" count + ":    " + '<a href="'+href+'#'+f"%04d" count+'">'+ dfn +'</a></pre>';
              //for cline in rev comments do println$ "           "+ cline; done
              txt = "";
              for cline in rev comments do txt += cline+"\n"; done
              result = xlat_fdoc (txt, "dummy");
              html = #(result.html_raw);
              if txt != "" do 
                println$ "<div style='font-family:sans-serif; font-size:10pt; " + 
                "margin-left:100; margin-right:100; top:2; color:#404040; '>" + html + "</div>"; 
              done
              comments = Empty[string];
            done
          else
            if prefix (dfn, "class") or prefix (dfn, "open class") do
              println$ "";
              println$ f"%04d" count + ":  " + dfn;
              for cline in rev comments do println$ "           "+cline; done
              comments = Empty[string];
            else
              println$ f"%04d" count + ":    " + dfn;
              for cline in rev comments do println$ "           "+ cline; done
              comments = Empty[string];
            done
          done
        | #None => ;
        endmatch; //d grexp
      done
    done
  done
  
  if ishtml do
    println$ "</body></html>";
  done
  


Library index table.
--------------------

Lists symbols alphabetically.

.. index:: str(fun)
.. code-block:: felix

  //[flx_libindex.flx]
  var ishtml = System::argv 1 == "--html";
  var dir =  Filename::join ("src", "lib", "std");
  var fregex = ".*\\.(flx|fdoc)";
  var lregex = "^ *(virtual|noinline)* *(proc|fun|class|ctor|gen) *(([A-Z]|[a-z])([A-Z]|[a-z]|[0-9]|-|_)*[?]?).*";
  var lgrep = RE2 lregex;
  var n = NumberOfCapturingGroups(lgrep)+1;
  var v = varray[StringPiece] (n.size,StringPiece "");
  
  var grexp = RE2 lregex;
  var extract = RE2 " *([^={]*) *(=|{|;).*";
  var n2 = NumberOfCapturingGroups(extract)+1;
  var v2 = varray[StringPiece] (n2.size,StringPiece "");
  var v2a = varray[StringPiece] (n2.size,StringPiece "");
  
  typedef data_t = (file:string, line:int, dfn:string);
  instance Str[data_t] {
    fun str (d:data_t) => d.file + "<"+d.line.str+">:"+d.dfn;
  }
  
  var index = #strdict[list[data_t]];
  
  for file in FileSystem::regfilesin (dir, fregex) do
    //println$ file;
    var text = load (Filename::join dir file);
    var count = 0;
    var lines = split (text, char "\n");
    for line in lines do
      ++count;
      if line != "" do
        var m = Match (grexp, StringPiece line, 0, ANCHOR_BOTH, v.stl_begin,n); 
        if m do
          var sym = v.3.string;
          var dfn = "";
          var m2 = Match (extract, StringPiece line, 0, ANCHOR_BOTH, v2.stl_begin, n2);
          if m2 do
            m2 = Match (extract, StringPiece line, 0, ANCHOR_BOTH, v2a.stl_begin, n2);
            if m2 do
              dfn = v2a . 1 . string . strip;
            else
              dfn = v2 . 1 . string . strip;
            done
          else
            dfn = line . strip;
          done
          //println$ file, count, sym,dfn;
          var data = (file=file, line=count, dfn=dfn);
          //val old_data =index.get_dflt(sym,Empty[data_t]);
          //val new_data = Cons (data, old_data);
          //val new_data =Cons (data,index.get_dflt(sym,Empty[data_t]));
          //index.add sym new_data;
          index.add sym (var Cons (data,index.get_dflt(sym,Empty[data_t])));
        done
      done
    done
  done
  
  //println$ "------------------";
  if ishtml do
    var ctrl = char " ";
    println$ "<html><body>";
    println$ "<h1>Felix library Index</h1>";
    println$ "<pre>";
    match key,value in index do
      var newctrl = char key;
      if ctrl != newctrl do
        println$ "<hr/>";
        ctrl = newctrl;
      done
      println$ key;
      match  (file=xfile,line=xline,dfn=xdfn) in value do
       var href = "/share/lib/std/" + xfile;
       println$ '  <a href="'+href+ "#"+f"%04d" xline + '">' + xfile + ":"+ str xline + "</a>: " + xdfn;
      done
    done 
    println$ "</pre></body></html>";
  else
    match key,value in index do
      println$ key;
      match  (file=xfile,line=xline,dfn=xdfn) in value do
       println$ "  " + xfile + ":"+ str xline + ": " + xdfn;
      done
    done 
  done


Make tutorial index pages.
--------------------------

Synthesises an index page for tutorial groups
with specified heading and pattern match.


.. index:: mkentry(fun)
.. code-block:: felix

  //[flx_mktutindex.flx]
  var dirname = System::argv_dflt 1 "src/web/tut";
  var homepage = System::argv_dflt 2 "";
  
  if dirname == "--help" do
    println "Usage flx_mktutindex directory homepage";
    println "  Makes src/web/tutname_index.fdoc for files in src/web/tutname_\\d*\\.fdoc";
    System::exit 0;
  done
  
  proc make_index (prefix:string)
  {
    re := RE2(prefix+"_\\d*\\.fdoc");
    var docs = FileSystem::regfilesin(dirname, re);
    docs = sort docs;
    iter println of (string) docs;
    f := fopen_output(Filename::join (dirname,prefix+"_index.fdoc"));
    if homepage != "" do
      writeln$ f,
       "<p><a href='"+homepage+"'>Up</a></p>"
      ; 
    done
  
    writeln$ f,"@h1 "+prefix +" Index";
    var abstract = load (Filename::join (dirname, prefix + "_abstract.html"));
    if abstract != "" do
      writeln$ f,abstract;
    done
    writeln$ f,"<ul>";
    iter (proc (x:string) { writeln$ f, mkentry x; }) docs;
    writeln$ f,"</ul>";
    fclose f;
  
    fun mkentry(x:string):string = 
    {
      var hline = "\n";
      begin // find first non-blank line
        f := fopen_input(Filename::join (dirname,x));
        while hline == "\n" do
          hline = f.readln;
        done
        fclose f;
      end
      scan:for var i in 0uz upto hline.len - 1uz do
        if hline.[i]== char ' ' do break scan; done
      done
      title := hline.[i to].strip;
      html := '<li><a href="' + Filename::basename x + '">' + title + '</a></li>';
      return html;
    }
  }
  
  var re = RE2(".*_01.fdoc");
  var samples = FileSystem::regfilesin(dirname, re);
  for name in samples do
    var prefix = name.[0 to -8];
    make_index prefix;
  done
  



.. index:: emit_code(proc)
.. index:: println(proc)
.. code-block:: felix

  //[flx_fdoc2sphinx.flx]
  open Regdef;
  
  // command translation
  regdef ident_r = perl("[A-Za-z_][A-Za-z_0-9]*");
  regdef fkey_r = ident_r "." ident_r;
  regdef cmd_name_r = perl("[A-Za-z_][A-Za-z_0-9]*| *");
  regdef spc_r = " " *;
  regdef any_r = perl(".*"); 
  regdef cmd_r = "@" group(cmd_name_r) spc_r group(any_r);
  regdef tangler_r = "@tangler" spc_r group(fkey_r) spc_r  "=" spc_r group(any_r);
  regdef url_r = group(any_r) '<a href="' group(any_r) '">' group(any_r) "</a>" group(any_r);
  
  // top level class
  regdef class_r = ("open" spc_r)? ("class"|"module") spc_r group(ident_r) any_r;
  
  // nested in class, exactltly 2 spaces in
  regdef def_r ="ctor"|"fun"|"proc"|"gen"|"type"|"union"|"struct"|"cstruct"|"const"|"header"|"typedef";
  regdef adj_r = "virtual" | "inline";
  regdef fun_r = "  " (adj_r spc_r)? group(def_r) spc_r group(ident_r) any_r;
  
  var cmd_R = RE2 (render cmd_r);
  var tangler_R = RE2 (render tangler_r);
  var url_R = RE2 (render url_r);
  var fun_R = RE2 (render fun_r);
  var class_R = RE2 (render class_r);
  
  typedef markup_t = (`Txt | `At | `Code | `Slosh | `Math | `MathSlosh);
  fun code_fixer (a:string): string =
  {
    var out = "";
    var mode = (#`Txt) :>> markup_t;
    for ch in a do
      match mode with
      | `Txt =>
        if ch == char "@" do 
          mode = (#`At) :>> markup_t;
        elif ch == char "\\" do
          mode = (#`Slosh) :>> markup_t;
        else
          out += ch;
        done
  
      | `Slosh =>
        if ch == char "(" do
          mode = (#`Math) :>> markup_t;
          out += ":math:`";
        else
          out += "\\" + ch;
          mode = (#`Txt) :>> markup_t;
        done
  
      | `Math =>
        if ch == char "\\" do
          mode = (#`MathSlosh) :>> markup_t;
        else
          out+= ch;
        done
  
      | `MathSlosh =>
         if ch == ")" do
           out+="` ";
           mode = (#`Txt) :>> markup_t;
         else
           out+="\\" + ch;
           mode = (#`Math) :>> markup_t;
         done
  
      | `At =>
        if ch == char "{" do
          out += " :code:`";
          mode = (#`Code) :>> markup_t;
        else
         out += "@"+ch;
        done
  
      | `Code =>
        if ch == char "}" do
          out += "`";
          mode = (#`Txt) :>> markup_t;
        else
          out += ch;
        done
      endmatch;
    done
    return out;
  }
  
  
  fun url_fixer (a:string) =>
    match Match (url_R, a) with
    | None => a
    | Some grp => grp.1 + "`" + grp.3 + " <" + grp.2 + ">`_" + grp.4
  ;
  
  fun code_markup(a:string) => code_fixer (url_fixer a);
  
  fun lexer_from_filename (var s:string) : string =
  {
    s = strip s;
    var lexer = 
      match s.Filename::get_extension with
      | (".cpp" | ".cxx" | ".hpp")  =>  "cpp"
      | (".flx" | ".fdoc" | ".fsyn")  =>  "felix"
      | (".fpc") => "fpc"
      | (".c" | ".h") => "c"
      | (".py") => "python"
      | _ => "text"
      endmatch
    ;
    return lexer;
  }
  
  
  typedef mode_t = (`Doc | `Code | `Tangler);
  
  fun process_file (f: string): string =
  {
    var tanglers = Empty[string * string];
  
    var code_buf = Empty[string];
    var prefix = "";
    var out = "";
    proc emit_code () { 
      var b = rev code_buf;
      for l in b do
        var rc = Match (class_R, l);
        var rf = Match (fun_R, l);
        chainmatch rc with
        | Some grp =>
          out+= ".. index:: " + grp.1+"(class)" + "\n";
        ormatch rf with 
        | Some grp =>
          out+= ".. index:: " + grp.2+"("+grp.1+")" + "\n";
        | None => ;
        endmatch;
      done
      out += prefix;
      for l in b perform out += "  " + l + "\n";
      code_buf = Empty[string];
      mode = (#`Doc) :>> mode_t;
    }
  
    proc println[T with Str[T]] (x:T) => out += x.str + "\n"; 
  
    var mode : mode_t = (#`Doc) :>> mode_t;
    nextline: for line in split (f, char "\n") do
      var cmd = Match (tangler_R, line);
      match cmd with
      | Some grp => 
        mode = (#`Tangler) :>> mode_t;
        tanglers = (grp.1,grp.2) ! tanglers;
        continue nextline;
  
      | None =>
        match mode with
        | `Tangler =>
          var tab = rev tanglers;
          tanglers = Empty[string * string];
          var lkey,lfile = fold_left 
            (fun (lkey:int,lfile:int) (key:string,file:string) =>
               max (lkey, key.len.int), max (lfile, file.len.int)
            )
            (10,20)
            tab
          ;
          var tabline = "=" * lkey + " " + "=" * lfile;
          println$ tabline;
          println$ 
            ("key" + " " * lkey).[0..lkey] + 
            ("file" + " " * lfile).[0..lfile]
          ;
          println$ tabline;
          for item in tab do
            var key,file = item;
            println$ 
              (key + " " * lkey).[0..lkey] + 
              (file + " " * lfile).[0..lfile]
            ;
          done
          println$ tabline;
          mode = (#`Doc) :>> mode_t;
        | _ => ;
        endmatch;
      endmatch;
  
      cmd = Match (cmd_R, line);
      match cmd with
      | Some grp =>
        var c = grp.1;
        var a = grp.2;
        if c == "title" do
          println$ "";
          match mode with
          | `Code () => emit_code(); 
          | _ => ;
          endmatch;
          a = code_markup a;
          println$ "=" * a.len.int;
          println$ a;
          println$ "=" * a.len.int;
          println$ "";
  
        elif c == "h1" do
          println$ "";
          match mode with
          | `Code () => emit_code();
          | _ => ;
          endmatch;
          a = code_markup a;
          println$ a;
          println$ "=" * a.len.int;
          println$ "";
  
        elif c == "h2" do
          a = code_markup a;
          println$ "";
          match mode with
          | `Code => emit_code();
          | _ => ;
          endmatch;
          println$ a;
          println$ "-" * a.len.int;
          println$ "";
  
        elif c == "tangle" do
          println$ "";
          var lexer = lexer_from_filename a;
          prefix = ".. code-block:: "+lexer + "\n\n";
          prefix += "";
          if lexer in ("c","cpp","felix","fpc") do
            prefix += "  //[" + a + "]\n";
          elif lexer == "python" do
            prefix += "  #["+a+"]\n";
          done
          mode = (#`Code) :>> mode_t;
        else 
          match mode with
          | `Code => emit_code();
          | _ => ;
          endmatch;
        done
  
  
      | None =>
        match mode with
        | `Doc => 
           println$ code_markup line;
        | `Code => code_buf = line ! code_buf;
        endmatch;
      endmatch;
    done
    return out;
  }
  
  
  include "std/felix/flx_cp";
  
  var dir = "src/packages";
  var regex = "(.*).fdoc";
  var target = "doc/packages/${1}.rst";
  var live = true;
  var verbose = true;
  
  gen sandr (src: string, dst:string) =
  {
    var text = load src;
    var result = process_file (text);
    result = "Package: " + src + "\n\n"+result;
    save (dst, result);
    return true;
  }
  
  var filere = Re2::RE2 regex;
  CopyFiles::processfiles sandr (dir, filere, target, live, verbose);
  System::exit(0);


