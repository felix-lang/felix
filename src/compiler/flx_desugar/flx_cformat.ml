open String
open List
open Flx_ast
open Flx_exceptions
open Flx_util
open Flx_print

let fmts = [
  ("hhd","tiny");
  ("hhi","tiny");
  ("hho","utiny");
  ("hhx","utiny");
  ("hhX", "utiny");

  ("hd","short");
  ("hi","short");
  ("hu","ushort");
  ("ho","ushort");
  ("hx","ushort");
  ("hX", "ushort");

  ("d","int");
  ("i","int");
  ("u","uint");
  ("o","uint");
  ("x","uint");
  ("X", "uint");

  ("ld","long");
  ("li","long");
  ("lu","ulong");
  ("lo","ulong");
  ("lx","ulong");
  ("lX","ulong");

  ("lld","vlong");
  ("lli","vlong");
  ("llu","uvlong");
  ("llo","uvlong");
  ("llx","uvlong");
  ("llX","uvlong");

  ("zd","ssize");
  ("zi","ssize");
  ("zu","size");
  ("zo","size");
  ("zx","size");
  ("zX","size");

  ("jd","intmax");
  ("ji","intmax");
  ("ju","uintmax");
  ("jo","uintmax");
  ("jx","uintmax");
  ("jX","uintmax");


  ("td","ptrdiff");
  ("ti","ptrdiff");
  ("tu","uptrdiff");
  ("to","uptrdiff");
  ("tx","uptrdiff");
  ("tX","uptrdiff");

  ("e","double");
  ("E","double");
  ("f","double");
  ("F","double");
  ("g","double");
  ("G","double");
  ("a","double");
  ("A","double");

  ("Le","ldouble");
  ("LE","ldouble");
  ("Lf","ldouble");
  ("LF","ldouble");
  ("Lg","ldouble");
  ("LG","ldouble");
  ("La","ldouble");
  ("LA","ldouble");

  ("c","int");

  ("S","string");
  ("s","&char");
  ("p","address");
  ("P","address");
]


let is_final ch =
  try ignore(index "udioxXeEfFgGaAcsSpPn" ch); true
  with Not_found -> false

let is_alpha ch =
  try ignore(index "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" ch); true
  with Not_found -> false

let is_num ch =
  try ignore(index "0123456789" ch); true
  with Not_found -> false



type mode_t = [
  | `Skip
  | `Scan
]

let strchr ch = String.make 1 ch

let ast i =
  let s = Bytes.make (i+1) ' ' in
  Bytes.set s i '*';
  (Bytes.to_string s)

let numval ch = index "0123456789" ch

let types_of_cformat_string sr s =
  let err i msg = clierrx "[flx_desugar/flx_cformat.ml:121: E313] " sr ("In format, pos="^si i^"\n"^string_of_string s^"\n "^ast i^"\n"^msg) in
  let outfmt = ref "" in
  let tent = ref "" in
  let types = ref [] in
  let mode = ref `Skip in
  let fmt = ref "" in
  let space_used = ref false in
  let sign = ref ' ' in
  let dp = ref ' ' in
  let argno = ref 1 in

  let width = ref None in
  let prec = ref None in
  let pos = ref None in
  let acc = ref None in

  let drop () = tent := "" in
  let commit () = outfmt := !outfmt ^ !tent; drop () in
  let app ch = commit(); outfmt := !outfmt ^ String.make 1 ch in
  let ten ch = tent := !tent ^ String.make 1 ch in

  for i = 0 to String.length s - 1 do
    match !mode with
    (* look for leading % sign *)
    | `Skip ->
      app s.[i];
      if s.[i]='%' then mode := `Scan

    | `Scan ->
      let ch = s.[i] in

      (* just emit % sign *)
      if ch = '%' then
      begin
        mode := `Skip;
        space_used := false;
        fmt := "";
        app ch;
      end

      (* last char of format spec *)
      else if is_final ch then
      begin
        app (if ch = 'S' then 's' else ch); (* convert string to &char *)
        let xfmt = !fmt ^ strchr ch in
        begin
          match !acc with | None -> () | Some j ->
          match !width with | None -> width := !acc | Some _ -> prec := !acc
        end
        ;
        try
          let arg = match !pos with None -> !argno | Some j -> j in
          types := (arg,assoc xfmt fmts) :: !types;
          mode := `Skip;
          acc := None;
          width := None;
          prec := None;
          sign := ' ';
          dp := ' ';
          fmt := "";
          begin match !pos with None -> incr argno | Some _ -> () end;
          pos := None;
        with Not_found ->
          err i ("Unsupported format '" ^ xfmt ^ "'")
      end

      (* some other alpha char *)
      else if is_alpha ch then begin
        fmt := !fmt ^ strchr ch;
        app ch;
      end

      (* an * spec, add a new format immediately *)
      (* hacked: you can't do *99$ at the moment! *)
      else if ch = '*' then begin
        let arg = !argno in incr argno;
        types := (arg,"int") :: !types;
        app ch;
      end

      (* sign *)
      else if ch = '+' || ch = '-' then begin
        if !sign <> ' ' then err i "Extra sign"
        else
          sign := ch;
          app ch;
      end

      (* decimal point *)
      else if ch = '.' then begin
        if !dp <> ' ' then err i "Duplicate decimal point"
        else begin
          width := !acc;
          acc := None;
          dp := '.';
          app ch;
        end
      end

      (* digit *)
      else if is_num ch then begin
        ten ch;
        match !acc with
        | None -> acc := Some (numval ch)
        | Some j -> acc := Some (10 * j + numval ch)
      end

      (* dollar sign *)
      else if ch = '$' then begin
        drop();
        pos := !acc;
        acc := None;
      end

      (* one space is allowed after the % *)
      else if ch = ' ' && !fmt = "" && not !space_used then begin
        space_used := true;
        app ch
      end

      else
        clierrx "[flx_desugar/flx_cformat.ml:242: E314] " sr ("unsupported format '" ^ !fmt ^ strchr ch ^ "'")
  done;
  commit();
  !outfmt,
  rev_map (fun (i,s) -> i,TYP_name (sr,s,[])) !types

