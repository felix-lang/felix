(** Felix specific utilities. *)

(** Terminal error handler
 *
 * terminate the top level program in case of error *)
val terminate:
  bool ->
  exn ->
  unit
