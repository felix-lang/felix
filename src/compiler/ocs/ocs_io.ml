(* I/O primitives.  *)

open Ocs_types
open Ocs_error
open Ocs_env
open Ocs_eval
open Ocs_print

let get_stdin th =
  match th.th_stdin with
    Sport p -> p
  | _ -> assert false
;;

let get_stdout th =
  match th.th_stdout with
    Sport p -> p
  | _ -> assert false
;;

let read th cc =
  function
    [| |] ->
      cc (Ocs_read.read_from_port (get_stdin th))
  | [| Sport port |] -> cc (Ocs_read.read_from_port port)
  | _ -> raise (Error "read: bad args")
;;

let rdchr p cc =
  match Ocs_port.getc p with
    Some c -> cc (Schar c)
  | None -> cc Seof
;;

let read_char th cc =
  function
    [| |] -> rdchr (get_stdin th) cc
  | [| Sport port |] -> rdchr port cc
  | _ -> raise (Error "read-char: bad args")
;;

let peekchr p cc =
  match Ocs_port.getc p with
    Some c ->
      Ocs_port.ungetc p c;
      cc (Schar c)
  | None -> cc Seof
;;

let peek_char th cc =
  function
    [| |] -> peekchr (get_stdin th) cc
  | [| Sport port |] -> peekchr port cc
  | _ -> raise (Error "peek-char: bad args")
;;

let eof_object =
  function
    Seof -> Strue
  | _ -> Sfalse
;;

let chrdy p cc =
  cc (if Ocs_port.char_ready p then Strue else Sfalse)
;;

let char_ready th cc =
  function
    [| |] -> chrdy (get_stdin th) cc
  | [| Sport port |] -> chrdy port cc
  | _ -> raise (Error "char-ready?: bad args")
;;

let display th cc =
  function
    [| obj |] ->
      let p = get_stdout th in print p true obj; Ocs_port.flush p; cc Sunspec
  | [| obj; Sport p |] -> print p true obj; Ocs_port.flush p; cc Sunspec
  | _ -> raise (Error "display: bad args")
;;

let write th cc =
  function
    [| obj |] ->
      let p = get_stdout th in print p false obj; Ocs_port.flush p; cc Sunspec
  | [| obj; Sport p |] -> print p false obj; Ocs_port.flush p; cc Sunspec
  | _ -> raise (Error "write: bad args")
;;

let write_char th cc =
  function
    [| Schar c |] ->
      let p = get_stdout th in Ocs_port.putc p c; Ocs_port.flush p; cc Sunspec
  | [| Schar c; Sport p |] -> Ocs_port.putc p c; Ocs_port.flush p; cc Sunspec
  | _ -> raise (Error "write-char: bad args")
;;

let newline th cc =
  function
    [| |] ->
      let p = get_stdout th in Ocs_port.putc p '\n'; Ocs_port.flush p; cc Sunspec
  | [| Sport p |] -> Ocs_port.putc p '\n'; Ocs_port.flush p; cc Sunspec
  | _ -> raise (Error "newline: bad args")
;;

let current_input th cc =
  function
    [| |] -> cc th.th_stdin
  | _ -> raise (Error "current-input-port: bad args")
;;

let current_output th cc =
  function
    [| |] -> cc th.th_stdout
  | _ -> raise (Error "current-output-port: bad args")
;;

let is_input =
  function
    Sport p -> if Ocs_port.is_input p then Strue else Sfalse
  | _ -> Sfalse
;;

let is_output =
  function
    Sport p -> if Ocs_port.is_output p then Strue else Sfalse
  | _ -> Sfalse
;;

let open_input_file =
  function
    Sstring s -> Sport (Ocs_port.open_input_port s)
  | _ -> raise (Error "expected string as input file name")
;;

let open_output_file =
  function
    Sstring s -> Sport (Ocs_port.open_output_port s)
  | _ -> raise (Error "expected string as output file name")
;;

let close_port =
  function
    Sport p -> Ocs_port.close p
  | _ -> raise (Error "close-port: invalid argument")
;;

let scm_close_port p =
  close_port p; Sunspec
;;

(* Note that the call-with-*-file functions close the port if the
   procedure exits, so they must not be re-called using a captured
   continuation after they exit once.  Dynamic extents can't be used
   for this because closing and reopening the file would be an even
   bigger problem.  *)

let call_w_in th cc =
  function
    [| name; proc |] ->
      let p = open_input_file name in
	eval th (fun x -> close_port p; cc x) (Capply1 (Cval proc, Cval p))
  | _ -> raise (Error "call-with-input-file: bad args")
;;

let call_w_out th cc =
  function
    [| name; proc |] ->
      let p = open_output_file name in
	eval th (fun x -> close_port p; cc x) (Capply1 (Cval proc, Cval p))
  | _ -> raise (Error "call-with-output-file: bad args")
;;

let w_in th cc =
  function
    [| name; thunk |] ->
      let p = open_input_file name in
	eval { th with th_stdin = p }
	     (fun x -> close_port p; cc x)
	     (Capply0 (Cval thunk))
  | _ -> raise (Error "with-input-from-file: bad args")
;;

let w_out th cc =
  function
    [| name; thunk |] ->
      let p = open_output_file name in
	eval { th with th_stdout = p }
	     (fun x -> close_port p; cc x)
	     (Capply0 (Cval thunk))
  | _ -> raise (Error "with-output-to-file: bad args")
;;

let init e =
  set_pfcn e read "read";
  set_pfcn e read_char "read-char";
  set_pfcn e peek_char "peek-char";
  set_pfcn e char_ready "char-ready?";

  set_pf1 e eof_object "eof-object?";

  set_pfcn e display "display";
  set_pfcn e newline "newline";
  set_pfcn e write "write";
  set_pfcn e write_char "write-char";

  set_pfcn e current_input "current-input-port";
  set_pfcn e current_output "current-output-port";

  set_pf1 e is_input "input-port?";
  set_pf1 e is_output "output-port?";

  set_pf1 e open_input_file "open-input-file";
  set_pf1 e open_output_file "open-output-file";

  set_pf1 e scm_close_port "close-input-port";
  set_pf1 e scm_close_port "close-output-port";

  set_pfcn e call_w_in "call-with-input-file";
  set_pfcn e call_w_out "call-with-output-file";

  set_pfcn e w_in "with-input-from-file";
  set_pfcn e w_out "with-output-to-file";
;;
