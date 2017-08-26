(** {6 Routines to extract source reference from terms} *)

(** type of a span between two positions in one file *)
type t

(** Construct a srcref from a tuple (filename, startline, startcol, endline, endcol) 
Values are one origin.
*)
val make:
  string * (* filename *)
  int * (* starting line number, 1 origin *)
  int * (* starting column, 1 origin *)
  int * (* ending line number, 1 origin *)
  int   (* ending column, 1 origin *)
  -> t

(** Convert a srcref to a tuple *)
val to_tuple: t -> string * int * int * int * int

(** make a dummy srcref, for use in generated code *)
val make_dummy: string -> t

(** calculate the span between two srcrefs *)
val rsrange: t -> t -> t

(** Convert a srcref to a readable form *)
val short_string_of_src: t -> string

(** Display a srcref along with lines of the file *)
val long_string_of_src: t -> string

(** A dummy srceref for use in generated code *)
val dummy_sr: t

(** Get the file of a srcref *)
val file : t -> string

val first_line_no : t -> int
val first_line : t -> string

