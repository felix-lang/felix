(* Functor for creating wrap/unwrap functions *)

open Ocs_types
open Ocs_error

module Make(T: sig type t end) =
  struct
    type t = T.t

    exception E of t

    let wrap v =
      Swrapped (fun () -> raise (E v))

    let unwrap =
      function
	Swrapped f ->
	  (try
	    f ();
	    raise (Error "unwrap: internal error")
	   with E v -> v
	   | _ -> raise (Error "unwrap: wrong wrapped type"))
      | _ -> raise (Error "unwrap: not a wrapped type")

    let try_unwrap =
      function
	Swrapped f ->
	  (try
	    f ();
	    None
	   with E v -> Some v
	   | _ -> None)
      | _ -> None
  end

