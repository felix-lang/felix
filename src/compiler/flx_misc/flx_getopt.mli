(** Get options *)

val parse_option: string -> (string * string) list
val parse_options: string array -> (string * string) list

val check_key_value :
  (string * string) list ->
  string-> string ->
  bool

val check_key:
  (string * string) list ->
  string ->
  bool

val check_keys:
  (string * string) list ->
  string list ->
  bool

val get_key_value :
  (string * string) list ->
  string ->
  string option

val get_key_values :
  (string * string) list ->
  string ->
  string list

val get_keys_values :
  (string * string) list ->
  string list ->
  string list
