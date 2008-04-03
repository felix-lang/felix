(* Print Scheme values *)

open Ocs_types
open Ocs_sym
open Ocs_numstr

let write_string p s =
  Ocs_port.putc p '\"';
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '\n' -> Ocs_port.puts p "\\n"
    | '\r' -> Ocs_port.puts p "\\r"
    | '\t' -> Ocs_port.puts p "\\t"
    | '\\' -> Ocs_port.puts p "\\\\"
    | '\"' -> Ocs_port.puts p "\\\""
    | '\032' .. '\126' as c -> Ocs_port.putc p c
    | c -> Ocs_port.puts p (Printf.sprintf "\\x%02x" (int_of_char c))
  done;
  Ocs_port.putc p '\"'
;;

let write_char p c =
  Ocs_port.puts p "#\\";
  match c with
    '\033' .. '\126' -> Ocs_port.putc p c
  | _ -> Ocs_port.puts p (Ocs_char.char_to_name c)
;;

let rec write_vector p disp v =
  Ocs_port.puts p "#(";
  for i = 0 to Array.length v - 1 do
    if i <> 0 then Ocs_port.putc p ' ';
    print p disp v.(i)
  done;
  Ocs_port.putc p ')'

and write_list p disp l =
  Ocs_port.putc p '(';
  let rec pit l =
    print p disp l.car;
    match l.cdr with
      Snull -> ()
    | Spair t -> Ocs_port.putc p ' '; pit t
    | x -> Ocs_port.puts p " . "; print p disp x
  in
    pit l;
    Ocs_port.putc p ')'

and print p disp =
  function
    Snull -> Ocs_port.puts p "()"
  | Seof -> Ocs_port.puts p "#<eof>"
  | Strue -> Ocs_port.puts p "#t"
  | Sfalse -> Ocs_port.puts p "#f"
  | Sstring s -> if disp then Ocs_port.puts p s else write_string p s
  | Ssymbol s -> Ocs_port.puts p s
  | Sint i -> Ocs_port.puts p (string_of_int i)
  | Sreal r -> Ocs_port.puts p (string_of_real r)
  | Scomplex z -> Ocs_port.puts p (string_of_complex z)
  | Sbigint b -> Ocs_port.puts p (Big_int.string_of_big_int b)
  | Srational r -> Ocs_port.puts p (Ratio.string_of_ratio r)
  | Schar c -> if disp then Ocs_port.putc p c else write_char p c
  | Spair l -> write_list p disp l
  | Svector v -> write_vector p disp v
  | Sport _ -> Ocs_port.puts p "#<port>"
  | Sproc _ -> Ocs_port.puts p "#<procedure>"
  | Sprim { prim_fun = _; prim_name = n } ->
      Ocs_port.puts p "#<primitive:"; Ocs_port.puts p n; Ocs_port.putc p '>'
  | Spromise _ -> Ocs_port.puts p "#<promise>"
  | Sesym (_, s) -> print p disp s
  | Swrapped _ -> Ocs_port.puts p "#<wrapped>"
  | Sunspec -> Ocs_port.puts p "#<unspecified>"
  | _ -> Ocs_port.puts p "#<unknown>"
;;

