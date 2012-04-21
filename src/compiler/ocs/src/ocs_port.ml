(* Buffered I/O, Scheme ports.  *)

open Ocs_error

(* Ports can either be file descriptors or string buffers.  File
   descriptors may be valid for both input and output, but when
   switching between the two modes, the file offset may not work
   as expected.

   For unbuffered or asynchronous I/O, from Scheme or otherwise,
   the port can simply be used as a reference to the file
   descriptor.  *)

type port = {
  mutable p_buf : string;
  mutable p_pos : int;
  mutable p_wend : int;
  mutable p_rend : int;
  mutable p_ugc : char option;
  mutable p_fd : Unix.file_descr option;
  mutable p_input : bool;
  mutable p_output : bool;
  p_close : bool
}

type port_flag =
    Pf_input
  | Pf_output
  | Pf_close

let mkbuf () =
  String.create 1024
;;

let mkport buf fd inf outf cl =
  { p_buf = buf;
    p_pos = 0;
    p_wend = 0;
    p_rend = 0;
    p_ugc = None;
    p_fd = fd;
    p_input = inf;
    p_output = outf;
    p_close = cl }
;;

let is_input p =
  p.p_input
;;

let is_output p =
  p.p_output
;;

let wrflush p =
  if not p.p_output then
    raise (Error "not a valid output port");
  match p.p_fd with
    Some fd ->
      if p.p_wend > 0 && p.p_pos > 0 then
	begin
	  try
	    let n = Unix.write fd p.p_buf 0 p.p_pos in
	      if n <> p.p_pos then
		raise (Error "write error: incomplete write")
	  with
	    Unix.Unix_error (e, _, _) ->
	      raise (Error ("write error: " ^ Unix.error_message e))
	end;
      p.p_pos <- 0;
      p.p_wend <- String.length p.p_buf
  | None ->
      if p.p_pos = p.p_wend then
	let n = String.length p.p_buf in
	let nbuf = String.create (n * 2) in
	  String.blit p.p_buf 0 nbuf 0 n;
	  p.p_buf <- nbuf;
	  p.p_wend <- String.length p.p_buf
;;

let rdfill p =
  if not p.p_input then
    raise (Error "not a valid input port");
  if p.p_wend > 0 then
    wrflush p;
  p.p_pos <- 0;
  p.p_rend <- 0;
  p.p_wend <- 0;
  match p.p_fd with
    Some fd ->
      begin
	try
	  p.p_rend <- Unix.read fd p.p_buf 0 (String.length p.p_buf)
	with
	  Unix.Unix_error (e, _, _) ->
	    raise (Error ("read error: " ^ Unix.error_message e))
      end
  | None -> ()
;;

let getc p =
  match p.p_ugc with
    Some _ as c -> p.p_ugc <- None; c
  | None ->
      if p.p_rend = 0 || p.p_pos >= p.p_rend then rdfill p;
      if p.p_rend = 0 then None
      else
	begin
	  assert (p.p_pos < p.p_rend);
	  let c = p.p_buf.[p.p_pos] in
	    p.p_pos <- p.p_pos + 1;
	    Some c
	end
;;

let get_fd p =
  p.p_fd
;;

let flush p =
  if p.p_wend > 0 then
    wrflush p
;;

let close p =
  if p.p_input || p.p_output then
    begin
      flush p;
      p.p_input <- false;
      p.p_output <- false;
      match p.p_fd with
	Some fd ->
	  if p.p_close then Unix.close fd;
	  p.p_fd <- None
      | None -> ()
    end
;;

let ungetc p c =
  p.p_ugc <- Some c
;;

let char_ready p =
  if p.p_ugc <> None || p.p_pos < p.p_rend then true
  else if not p.p_input then false
  else
    match p.p_fd with
      Some fd ->
	let (r, _, _) = Unix.select [ fd ] [] [] 0.0 in
	  List.length r > 0
    | None -> false
;;

let putc p c =
  if p.p_wend = 0 || p.p_pos >= p.p_wend then
    wrflush p;
  assert (p.p_pos < p.p_wend);
  p.p_buf.[p.p_pos] <- c;
  p.p_pos <- p.p_pos + 1
;;

let puts p s =
  let n = String.length s in
    if n > 0 && p.p_rend - p.p_pos >= n then
      begin
	String.blit s 0 p.p_buf p.p_pos n;
	p.p_pos <- p.p_pos + n
      end
    else
      String.iter (fun c -> putc p c) s
;;

let fd_port fd flags =
  let inf = ref false
  and outf = ref false
  and clf = ref false in
    List.iter (function
		Pf_input -> inf := true
	      | Pf_output -> outf := true
	      | Pf_close -> clf := true) flags;
    let p = mkport (mkbuf ()) (Some fd) !inf !outf !clf in
      if !clf then Gc.finalise close p;
      p
;;

let input_port ch =
  fd_port (Unix.descr_of_in_channel ch) [ Pf_input ]
;;

let output_port ch =
  fd_port (Unix.descr_of_out_channel ch) [ Pf_output ]
;;

let open_input_port name =
  try
    let fd = Unix.openfile name [ Unix.O_RDONLY ] 0 in
      fd_port fd [ Pf_input; Pf_close ]
  with
    Unix.Unix_error (e, _, _) ->
      let err = Unix.error_message e in
	raise (Error ("unable to open '" ^ name ^ "' for input: " ^ err))
;;

let open_output_port name =
  try
    let fd = Unix.openfile name [ Unix.O_WRONLY; Unix.O_APPEND;
				  Unix.O_CREAT; Unix.O_TRUNC ] 0o666 in
      fd_port fd [ Pf_output; Pf_close ]
  with
    Unix.Unix_error (e, _, _) ->
      let err = Unix.error_message e in
	raise (Error ("unable to open '" ^ name ^ "' for output: " ^ err))
;;

let string_input_port s =
  let p = mkport s None true false false in
    p.p_rend <- String.length s;
    p
;;

let string_output_port () =
  mkport (mkbuf ()) None false true false
;;
