open Flx_ast
open Flx_string
open Flx_token

exception Duplicate_macro of string

class comment_control :
  object
    val mutable nesting_level : int
    val mutable text : string
    method append : string -> unit
    method decr : unit
    method get_comment : string
    method get_nesting_level : int
    method incr : unit
    method set_text : string -> unit
  end

type condition_t = [
 | `Processing
 | `Skip_to_endif
 | `Skip_to_else
 | `Subscan
]

type location = {
    mutable buf_pos : int;
    mutable last_buf_pos : int;
    mutable line_no : int;
    mutable original_line_no : int;
}


class file_control :
  string ->
  string ->
  string list ->
  object
    val mutable loc: location
    val filename : string
    val mutable condition : condition_t list
    val macros : (string,string list * token list) Hashtbl.t

    method get_loc : location
    method set_loc : location -> unit

    method get_buf_pos : int
    method get_srcref : Lexing.lexbuf -> srcref
    method get_physical_srcref : Lexing.lexbuf -> srcref
    method incr : int -> unit
    method incr_lex_counters : Lexing.lexbuf -> unit
    method set_buf_pos : int -> unit
    method set_line : int -> Lexing.lexbuf -> unit
    method set_filename : string -> unit
    method get_relative : string -> string
    method get_incdirs : string list
    method get_absolute : string -> string

    method get_condition : condition_t
    method push_condition : condition_t -> unit
    method pop_condition : unit
    method set_condition : condition_t -> unit
    method condition_stack_length : int

    method store_macro : string -> string list -> token list -> unit
    method undef_macro : string -> unit
    method get_macro : string -> (string list * token list) option
    method get_macros : (string,string list * token list) Hashtbl.t
  end

class lexer_state :
  string ->
  string ->
  string list ->
  string option ->
  (string -> expr_t->expr_t) ->
  object
    method get_cache_dir : string option
    val expand_expr : string -> expr_t -> expr_t
    val comment_ctrl : comment_control
    val file_ctrl : file_control

    val mutable symbols :
      (string * (srcref * string -> token)) list array
    val mutable keywords:
      (string * (srcref * string -> token)) list array
    val nonterminals: (string, (token list *ast_term_t) list) Hashtbl.t
    val mutable include_files : string list

    method get_expand_expr : string -> expr_t -> expr_t

    method add_include_file : string -> unit
    method get_include_files : string list

    method append_comment : string -> unit
    method comment_level : int
    method decode : (string -> string) -> string -> string
    method decr_comment : unit
    method get_comment : string
    method get_srcref : Lexing.lexbuf -> srcref
    method get_physical_srcref : Lexing.lexbuf -> srcref
    method incr_comment : unit
    method newline : Lexing.lexbuf -> unit
    method set_comment : string -> unit
    method is_at_line_start : bool
    method inbody: unit
    method string_of_srcref : Lexing.lexbuf -> string
    method set_line : int -> Lexing.lexbuf-> unit
    method set_filename : string -> unit
    method get_incdirs : string list
    method get_relative : string -> string
    method get_absolute : string -> string

    method get_condition : condition_t
    method push_condition : condition_t -> unit
    method pop_condition : unit
    method set_condition : condition_t -> unit
    method condition_stack_length : int
    method get_loc : location
    method set_loc : location -> unit

    method store_macro : string -> string list -> token list -> unit
    method undef_macro : string -> unit
    method get_macro : string -> (string list * token list) option
    method get_macros : (string,string list * token list) Hashtbl.t
    method add_macros : lexer_state -> unit
    method adjust_symbol_array : int -> unit

    method get_keywords:
      (string * (srcref * string -> token)) list array

    method adjust_keyword_array : int -> unit

    method add_keyword:
      string -> unit

    method get_nonterminals:
      (string, (token list *ast_term_t) list) Hashtbl.t

    method get_symbols:
      (string * (srcref * string -> token)) list array

    method add_nonterminal:
      string -> range_srcref -> token list -> ast_term_t -> unit

    method tokenise_symbols : Lexing.lexbuf -> string -> token list
end
