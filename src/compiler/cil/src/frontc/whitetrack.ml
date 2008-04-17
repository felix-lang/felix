
open Cabs
open Cabshelper

(* This isn't the most efficient way to do things.
 * It would probably be better to not reparse rather
 * than keep the tokens in memory *)
 
(* In particular, most of the tokens we hold will be
   header files that we don't need *)

(* map cabslocs to token indexes *)

(* TODO: gather until end of line, then decide where to split *)

let tokenmap : ((string * int),int) Hashtbl.t = Hashtbl.create 1000
let nextidx = ref 0

(* array of tokens and whitespace *)
let tokens = GrowArray.make 0 (GrowArray.Elem  ("",""))

let cabsloc_to_str cabsloc =
    cabsloc.filename ^ ":" ^ string_of_int cabsloc.lineno ^ ":" ^ 
    string_of_int cabsloc.byteno ^ ":" ^ 
    string_of_int cabsloc.ident

let lastline = ref 0

let wraplexer_enabled lexer lexbuf = 
    let white,lexeme,token,cabsloc = lexer lexbuf in
    GrowArray.setg tokens !nextidx (white,lexeme);
    Hashtbl.add tokenmap (cabsloc.filename,cabsloc.byteno) !nextidx;
    nextidx := !nextidx + 1;
    token

let wraplexer_disabled lexer lexbuf = 
    let white,lexeme,token,cabsloc = lexer lexbuf in
    token

let enabled = ref false

let wraplexer lexer =
    if !enabled then wraplexer_enabled lexer 
    else wraplexer_disabled lexer
    
let finalwhite = ref "\n"    
    
let setFinalWhite w = finalwhite := w 
    
let curidx = ref 0  
let noidx = -1  
let out = ref stdout
    
let setLoc cabsloc =
    if cabsloc != cabslu && !enabled then begin
        try 
            curidx := Hashtbl.find tokenmap (cabsloc.filename,cabsloc.byteno)
        with
            Not_found -> Errormsg.s 
                (Errormsg.error "setLoc with location for non-lexed token: %s"
                    (cabsloc_to_str cabsloc)) 
    end else begin curidx := noidx; () end
    
let setOutput out_chan = 
    out := out_chan

(* TODO: do this properly *)
let invent_white () = " "

let rec chopwhite str =
    if String.length str = 0 then str 
    else if String.get str (String.length str - 1) = ' ' then
        chopwhite (String.sub str 0 (String.length str - 1))
    else if String.get str 0 = ' ' then
        chopwhite (String.sub str 1 (String.length str - 1)) 
    else str
    
let last_was_maybe = ref false    
let last_str = ref ""
    
let print str =
    let str = chopwhite str in
    if str = "" then ()
    else if !curidx == noidx || not !enabled then 
        output_string !out (invent_white() ^ str) 
    else begin
        let srcwhite,srctok = GrowArray.getg tokens !curidx in
        let white = if str = srctok 
            then srcwhite
            else begin
                ignore (Errormsg.warnOpt "%s" ("nomatch:["^String.escaped str^"] expected:["^String.escaped srctok ^ 
                    "] - NOTE: cpp not supported"));
                invent_white ()
            end in
        if !last_was_maybe && str = !last_str then () else begin
            output_string !out (white ^ str);
            curidx := !curidx + 1
        end
    end;
    last_was_maybe := false

let printl strs = 
    List.iter print strs   
    
let printu str =
    let srcwhite,srctok = GrowArray.getg tokens !curidx in
    if chopwhite str = "" then () 
    else if srctok = str 
        || srctok = str ^ "__" 
        || srctok = "__" ^ str
        || srctok = "__" ^ str ^ "__"
        then
        print srctok
    else (print_endline ("u-nomatch:["^str^"]"); print str)
                
let print_maybe str =
    let srcwhite,srctok = GrowArray.getg tokens !curidx in
    if str = srctok then begin 
        print str;
        last_was_maybe := true;
        last_str := str
        end else ()


let printEOF () = output_string !out !finalwhite


