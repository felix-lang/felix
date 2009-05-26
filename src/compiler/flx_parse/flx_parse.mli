type 'a parser_state_t

(** Create a new parser state. *)
val make_parser_state:
  (Flx_ast.statement_t -> 'a -> 'a) ->  (** A function that folds statements. *)
  'a ->                                 (** Initial data to fold. *)
  'a parser_state_t

(** Extract the current compliation unit from the parser state. *)
val parser_data: 'a parser_state_t -> 'a

(** Parse a channel and return the new parser state. *)
val parse_channel:
  ?name:string ->       (** The optional name for this channel. *)
  'a parser_state_t ->  (** The state for the felix parser. *)
  in_channel ->         (** The channel to parse. *)
  'a parser_state_t

(** Parse a file and return the new parser state. *)
val parse_file:
  'a parser_state_t ->  (** The state for the felix parser. *)
  string ->             (** The filename to parse. *)
  'a parser_state_t

(** Parse a string and return the new parser state. *)
val parse_string:
  ?name:string ->       (** The optional name for this string. *)
  'a parser_state_t ->  (** The state for the felix parser. *)
  string ->             (** The string to parse. *)
  'a parser_state_t

(** Parse a function and return the new parser state. *)
val parse_function:
  ?name:string ->           (** The optional name for this string. *)
  'a parser_state_t ->      (** The state for the felix parser. *)
  (string -> int -> int) -> (** The lexer function generator. *)
  'a parser_state_t
