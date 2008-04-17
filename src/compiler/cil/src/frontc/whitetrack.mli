
(* wrapped version of the lexer that remembers all its tokens *)
val wraplexer : (Lexing.lexbuf -> (string * string * 'a * Cabs.cabsloc)) 
                    -> Lexing.lexbuf -> 'a
val setFinalWhite : string -> unit

(* print a string, with correct whitespace *)
val print : string -> unit
val printl : string list -> unit
val printu : string -> unit
val print_maybe : string -> unit

val printEOF : unit -> unit

(* look for whitespace around here *)
val setLoc : Cabs.cabsloc -> unit

(* where we write the file to *)
val setOutput : out_channel -> unit

(* is whitespace tracking enabled *)
val enabled : bool ref
