type parser_state_t

(** Create a new parser state. *)
val make_parser_state: unit -> parser_state_t

(** Extract the current compliation unit from the parser state. *)
val compilation_unit: parser_state_t -> Flx_ast.compilation_unit_t

(** Parse a channel and return the new parser state. *)
val parse_channel:
  ?name:string ->   (** The optional name for this channel. *)
  parser_state_t -> (** The state for the felix parser. *)
  in_channel ->     (** The channel to parse. *)
  parser_state_t

(** Parse a file and return the new parser state. *)
val parse_file:
  parser_state_t -> (** The state for the felix parser. *)
  string ->         (** The filename to parse. *)
  parser_state_t

(** Parse a string and return the new parser state. *)
val parse_string:
  ?name:string ->   (** The optional name for this string. *)
  parser_state_t -> (** The state for the felix parser. *)
  string ->         (** The string to parse. *)
  parser_state_t

(** Parse a function and return the new parser state. *)
val parse_function:
  ?name:string ->           (** The optional name for this string. *)
  parser_state_t ->         (** The state for the felix parser. *)
  (string -> int -> int) -> (** The lexer function generator. *)
  parser_state_t
