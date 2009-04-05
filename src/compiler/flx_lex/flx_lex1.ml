open Flx_token
open Flx_exceptions
open List
open Flx_srcref

(*

NOTES on token filters
----------------------

These routine are all meant to be short and simple
so its easy to see what they do -- the result
of composition is inefficient!

The results of filter application may depend on
ordering! In other words, each filter has
a postcondition, but also may have several
stronger postconditions given a pre-condition.

Composition of these routines is inefficient!
*)

let dummysr = ("<eol>",0,0,0)

(* throw out C comments; replace newline terminated C++
   comment with a newline
*)
let filter_comments x =
  let rec filter x' result  =
    match x' with
    | COMMENT_NEWLINE _ :: t -> filter t (NEWLINE::result)
    | COMMENT _ :: t  -> filter t result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out all white space *)
let filter_white x =
  let rec filter x' result  =
    match x' with
    | WHITE _ :: t -> filter t result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out all newlines *)
let filter_newline x =
  let rec filter x' result  =
    match x' with
    | NEWLINE :: t -> filter t result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out white space not immediately following a newline *)
let filter_inner_white x =
  let rec filter x' result  =
    match x' with
    | NEWLINE :: (WHITE _ as w) :: t -> filter t (w :: NEWLINE :: result)
    | WHITE _ :: t -> filter t result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out whitespace immediately preceding a NEWLINE *)
let filter_trailing_white x =
  let rec filter x' result  =
    match x' with
    | WHITE _ :: NEWLINE :: t -> filter t (NEWLINE::result)
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out duplicate NEWLINE, elides blanks lines if they're cleared
   of white space etc
*)
let filter_dup_newline x =
  let rec filter x' result  =
    match x' with
    | NEWLINE :: NEWLINE :: t -> filter (NEWLINE :: t) result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out duplicate SEMI *)
let filter_dup_semi x =
  let rec filter x' result  =
    match x' with
    | SEMI _ as s :: SEMI _ :: t -> filter (s :: t) result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

let check_nowhite x =
  let rec filter x' = match x' with
    | WHITE _ :: t -> failwith "UNEXPECTED WHITESPACE"
    | h :: t -> filter t
    | [] -> x
  in filter x

let check_nonewline x =
  let rec filter x' = match x' with
    | NEWLINE :: t -> failwith "UNEXPECTED NEWLINE"
    | h :: t -> filter t
    | [] -> x
  in filter x

let check_dent x =
  let rec filter x' = match x' with
    | NEWLINE :: WHITE _  :: t -> filter t
    | WHITE _ :: t -> failwith "WHITESPACE NOT AFTER NEWLINE"
    | h :: t -> filter t
    | [] -> x
  in filter x

(* this checks canonical form, every newline has white space
   after it, even width 0 if necessary
*)
let check_full_dent x =
  let rec filter x' = match x' with
    | NEWLINE :: WHITE _  :: t -> filter t
    | WHITE _ :: t -> failwith "WHITESPACE NOT AFTER NEWLINE"
    | NEWLINE :: t -> failwith "NEWLINE NOT FOLLOWED BY WHITESPACE"
    | h :: t -> filter t
    | [] -> x
  in filter x

let check_dollardollar x =
  let rec filter x' = match x' with
    | DOLLARDOLLAR _ :: NEWLINE :: WHITE _  :: t -> filter t
    | DOLLARDOLLAR _ :: t -> failwith "$$ not followed by INDENT"
    | h :: t -> filter t
    | [] -> x
  in filter x

(* canonicalise indents, by adding whitespace width 0 if necessary
  Afterwards all newlines should be followed by white space
*)
let canonical_dent x =
  let rec filter x' result  =
    match x' with
    | NEWLINE :: (WHITE _ as w) :: t -> filter t (w::NEWLINE::result)
    | NEWLINE :: t -> filter t (WHITE 0::NEWLINE::result)
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []


let dentit x =
  let rec filter x' dent result = match x' with
    (* New block start *)
    | DOLLARDOLLAR sr :: NEWLINE :: WHITE n :: t ->
      filter t (n::dent) (LBRACE sr::result)

    (* continuation line *)
    | NEWLINE :: WHITE n :: t when n > hd dent ->
      filter t dent result

    (* new statement *)
    | NEWLINE :: WHITE n :: t when n = hd dent ->
      filter t dent (SEMI dummysr :: result)

    (* dedent *)
    | NEWLINE :: WHITE n :: t when n < hd dent ->
      filter x' (tl dent) (RBRACE dummysr :: SEMI dummysr :: result)

    | ENDMARKER :: t when length dent > 1 ->
      filter x' (tl dent) (RBRACE dummysr :: SEMI dummysr :: result)

    | ENDMARKER :: t when length dent = 1 ->
      rev (ENDMARKER :: SEMI dummysr :: result)

    | h :: t -> filter t dent (h::result)
    | [] -> rev result
  in
  filter x [-1] []

(* remove comments, whitespace, newlines, trailing sloshes,
  and a trailing hash on the first line
*)
let filter_preprocessor x =
  let rec filter first_line x' result  =
    match x' with
    | WHITE _ :: t
    | COMMENT _ :: t
      -> filter first_line t result

    | COMMENT_NEWLINE _ :: t
    | NEWLINE :: t
    | SLOSH :: NEWLINE :: t
    | SLOSH :: WHITE _ :: NEWLINE :: t
      -> filter false t result

    | HASH _ :: NEWLINE :: t
    | HASH _ :: WHITE _ :: NEWLINE :: t
      when first_line  -> filter false t result

    | h :: t -> filter first_line t (h::result)
    | [] -> rev result
  in filter true x []


let compress_ctypes x =
  let rec filter x' result =
    match x' with
| NAME(sr,"unsigned") :: NAME(_,"long") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "uvlong") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"unsigned") :: NAME(_,"long") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "uvlong") :: result)
| NAME(sr,"unsigned") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "ulong") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "long") :: result)
| NAME(sr,"long") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"long") :: NAME(_,"double") :: NAME(_,"float") :: t -> 
    filter t (NAME (sr, "ldouble") :: result)
| NAME(sr,"unsigned") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "ulong") :: result)
| NAME(sr,"unsigned") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "uint") :: result)
| NAME(sr,"unsigned") :: NAME(_,"char") :: t -> 
    filter t (NAME (sr, "utiny") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "long") :: result)
| NAME(sr,"signed") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "int") :: result)
| NAME(sr,"signed") :: NAME(_,"char") :: t -> 
    filter t (NAME (sr, "tiny") :: result)
| NAME(sr,"long") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "long") :: result)
| NAME(sr,"float") :: NAME(_,"double") :: t -> 
    filter t (NAME (sr, "double") :: result)
| NAME(sr,"double") :: NAME(_,"float") :: t -> 
    filter t (NAME (sr, "double") :: result)
| NAME(sr,"unsigned") :: t -> 
    filter t (NAME (sr, "uint") :: result)
| NAME(sr,"long") :: t -> 
    filter t (NAME (sr, "long") :: result)
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

let unkeyword ts =
  let rec filter inp out = match inp with
| (COLONCOLON _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (DOT _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (RIGHTARROW _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (STRUCT _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (UNION _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (CLASS _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (FUNCTION _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (PROCEDURE _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
| (GENERATOR _ as cc) :: (USER_KEYWORD (sr,s) as u) :: tail 
  ->
    let sr = Flx_prelex.src_of_token  u in
    let s = Flx_prelex.string_of_token u in
    let u = NAME (sr,s) in
    filter tail (u :: cc :: out)
  | h :: t -> filter t (h::out)
  | [] -> rev out
  in filter ts []

let token_packer ts =
  let rec aux n o i = match i with
    | [] ->
      if n = 0 then rev o,[]
      else failwith "At end of file, unterminated token group"

    | NAME (sr,"_tok") :: t ->
      let h,t = aux (n+1) [] t in
      aux n (TOKEN_LIST h :: o) t

    | NAME (sr,"_etok") :: t ->
      if n = 0 then failwith "Unmatched _etok"
      else rev o,t

    | h :: t -> aux n (h::o) t
  in
  fst (aux 0 [] ts)

type state = {
  macs: (string * (string list * token list)) list ;
  cstack : bool list;
  cond : bool;
}

let cond ls = fold_left (fun x y -> x && y) true ls

let token_expander ts =
  let rec aux s o i = match i with
  | TOKEN_LIST ts :: t -> aux s o (ts @ t)

  | NAME (sr,name) as h :: t ->
    let err x = clierr (slift sr) x in
    begin match name with
    | "_ifdef" ->
       begin match t with
       | NAME (sr2,name) :: NAME(_,"_then" ) :: t ->
         let cs = mem_assoc name s.macs :: s.cstack in
         aux { s with cond=cond cs; cstack=cs} o t
       | _ -> err "usage: _ifdef token _then .. _endif"
       end

    | "_elifdef" ->
       begin match t with
       | NAME (sr2,name) :: NAME(_,"_then" ) :: t ->
         if length s.cstack > 0 then
           let cs = mem_assoc name s.macs :: tl s.cstack in
           aux { s with cond = cond cs; cstack=cs} o t
         else
          err "Unmatch _elif"

       | _ -> err "usage: _elifdef token _then .. _endif"
       end

    | "_endif" ->
      if length s.cstack > 0 then
        let cs = tl s.cstack in
        aux { s with cond = cond cs; cstack=cs} o t
      else
        err "Unmatch _endif"

    | "_else" ->
      if length s.cstack > 0 then
        let cs = not (hd s.cstack) :: tl s.cstack in
        aux { s with cond = cond cs; cstack=cs} o t
      else
        err "Unmatch _else"

    | _ when not (s.cond) -> aux s o t

    | "_tokdef" ->
      let rec grabdef n o i = match i with
        | NAME (sr,"_tokdef") as  h :: t ->
          grabdef (n+1) (h::o) t

        | NAME (sr,"_edef") as h :: t ->
          if n = 0 then rev o,t
          else grabdef (n-1) (h::o) t

        | NAME (sr,"_quote") :: h :: t ->
          grabdef n (h::o) t

        | h::t -> grabdef n (h::o) t
        | [] -> err "unterminated token macro substream"
      in
      begin match t with
      | NAME (sr2,name) :: t ->
        let rec grabp n o i : string list * token list  =
          if n = 0 then err "too many macro args, runaway?";
          match i with
          | [] -> err "unterminated macro definition"
          | EQUAL _ :: t -> rev o, t
          | NAME (_,s) :: t -> grabp (n-1) (s::o) t
          | _ -> err "macro arg must be identifier"
        in
        let params,t = grabp 10 [] t in
        let mac,t = grabdef 0 [] t in
         aux {s with macs=(name,(params,mac))::s.macs} o t
      | _ -> err "usage: _tokdef name = stream"
      end

    | "_undef" ->
       begin match t with
       | NAME (sr2,name) :: t ->
         let rec strip flag inp out = match inp with
         | [] -> rev out
         | (n,_) :: t when flag && n = name ->
           strip false t out
         | h :: t -> strip flag t (h::out)
         in
         let macs = strip true s.macs [] in
         aux {s with macs=macs} o t
       | _ -> err "usage: _undef name"
       end

    | "_popto" ->
       begin match t with
       | NAME (sr2,name) :: t ->
         let rec strip inp = match inp with
         | [] -> err ("_popto can't find macro " ^ name);
         | (n,_) :: t when n = name -> t
         | h :: t -> strip t
         in
         let macs = strip s.macs in
         aux {s with macs=macs} o t
       | _ -> err "usage: _popto name"
       end

    | _ when mem_assoc name s.macs ->
      let rec graba n o i =
        if n = 0 then rev o,i else
        match i with
        | [] -> err ("Not enough args for macro " ^ name)
        | h :: t -> graba (n-1) (h::o) t
      in
      let params,body = assoc name s.macs in
      let args, t = graba (length params) [] t in
      let pas =
        fold_left2
        (fun m p a -> (p,a) :: m)
        [] params args
      in
      let body =
        map
        (fun t -> match t with
          | NAME(_,s) ->
            (try assoc s pas with Not_found -> t)
          | _ -> t
        )
        body
      in
      aux s o (body @ t)
    | _ -> aux s (h::o) t
    end (* name handling *)

  | h :: t when not s.cond -> aux s o t
  | h :: t -> aux s (h::o) t
  | [] -> rev o
  in aux {macs=[]; cond=true; cstack=[]} [] ts


let translate ts =
  let filters = [
    filter_comments ;
    filter_inner_white;
    filter_trailing_white;
    filter_dup_newline;
    check_dent;
    canonical_dent;
    check_full_dent;
    check_dollardollar;
    dentit ;
    filter_dup_semi;
    check_nowhite;
    check_nonewline;
    compress_ctypes ;
    unkeyword ;
    token_packer;
    token_expander;
    ]
  and reverse_apply dat fn = fn dat
  in
  let result = List.fold_left reverse_apply ts filters in
  result

let translate_preprocessor ts =
  let filters = [
    (* 1 *) filter_preprocessor ;
    (* 2 *) compress_ctypes ;
    ]
  and reverse_apply dat fn = fn dat
  in List.fold_left reverse_apply ts filters
