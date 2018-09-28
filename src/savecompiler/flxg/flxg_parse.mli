(** Load the syntax into the state. *)
val load_syntax: Flxg_state.t -> Flx_token.local_data_t

(** Parse the file and return the list of statements it contains. *)
val parse_file:
  Flxg_state.t ->
  Flx_token.local_data_t ->
  string ->
  Flx_ast.statement_t list
