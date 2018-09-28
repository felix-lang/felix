(* Print Scheme values *)

open Ocs_types
open Ocs_sym
open Ocs_numstr

let store_string putc puts s =
  putc '\"';
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '\n' -> puts "\\n"
    | '\r' -> puts "\\r"
    | '\t' -> puts "\\t"
    | '\\' -> puts "\\\\"
    | '\"' -> puts "\\\""
    | '\032' .. '\126' as c -> putc c
    | c -> puts (Printf.sprintf "\\x%02x" (int_of_char c))
  done;
  putc '\"'
;;

let store_char putc puts c =
  puts "#\\";
  match c with
    '\033' .. '\126' -> putc c
  | _ -> puts (Ocs_char.char_to_name c)
;;

let rec store_vector putc puts disp v =
  puts "#(";
  for i = 0 to Array.length v - 1 do
    if i <> 0 then putc ' ';
    store putc puts disp v.(i)
  done;
  putc ')'

and store_list putc puts disp l =
  putc '(';
  let rec pit l =
    store putc puts disp l.car;
    match l.cdr with
      Snull -> ()
    | Spair t -> putc ' '; pit t
    | x -> puts " . "; store putc puts disp x
  in
    pit l;
    putc ')'

and store putc puts disp =
  function
    Snull -> puts "()"
  | Seof -> puts "#<eof>"
  | Strue -> puts "#t"
  | Sfalse -> puts "#f"
  | Sstring s -> if disp then puts (Bytes.to_string s) else store_string putc puts (Bytes.to_string s)
  | Ssymbol s -> puts s
  | Sint i -> puts (string_of_int i)
  | Sreal r -> puts (string_of_real r)
  | Schar c -> if disp then putc c else store_char putc puts c
  | Spair l -> store_list putc puts disp l
  | Svector v -> store_vector putc puts disp v
  | Sport _ -> puts "#<port>"
  | Sproc _ -> puts "#<procedure>"
  | Sprim { prim_fun = _; prim_name = n } ->
      puts "#<primitive:"; puts n; putc '>'
  | Spromise _ -> puts "#<promise>"
  | Sesym (_, s) -> store putc puts disp s
  | Swrapped _ -> puts "#<wrapped>"
  | Sunspec -> puts "#<unspecified>"
  | _ -> puts "#<unknown>"
;;

let write_string p s =
  store_string (Ocs_port.putc p) (Ocs_port.puts p) s

let write_char p c =
  store_char (Ocs_port.putc p) (Ocs_port.puts p) c

let print p b v =
  store (Ocs_port.putc p) (Ocs_port.puts p) b v

let write_string_to_buffer p s =
  store_string (Buffer.add_char p) (Buffer.add_string p) s

let write_char_to_buffer p c =
  store_char (Buffer.add_char p) (Buffer.add_string p) c

let print_to_buffer p b v =
  store (Buffer.add_char p) (Buffer.add_string p) b v

let string_of_ocs v =
  let b = Buffer.create 200 in
  print_to_buffer b false v;
  Buffer.contents b

