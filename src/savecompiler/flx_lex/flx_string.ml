exception StringError of string

let hexchar_of_int i =
  if i < 10
  then char_of_int (i + (int_of_char '0'))
  else char_of_int (i- 10 + (int_of_char 'A'))

let hex8 i : string =
  let j = ref i in
  let s = Bytes.create 8 in
  for k = 0 to 7 do
    Bytes.set s (7-k) (hexchar_of_int (!j mod 16));
    j := !j / 16
  done;
  Bytes.to_string s

let hex4 i : string =
  let j = ref i in
  let s = Bytes.create 4 in
  for k = 0 to 3 do
    Bytes.set s (3-k) (hexchar_of_int (!j mod 16));
    j := !j / 16
  done;
  Bytes.to_string s

let hex2 i : string =
  let j = ref i in
  let s = Bytes.create 2 in
  for k = 0 to 1 do
    Bytes.set s (1-k) (hexchar_of_int (!j mod 16));
    j := !j / 16
  done;
  Bytes.to_string s

let escape_of_char quote ch : string =
  if ch = '\\' then "\\\\"
  else if ch = quote then "\\" ^ (String.make 1 quote)
  else if ch = '\n' then "\\n"
  else if ch < ' '
  || ch > char_of_int 126
  then "\\x" ^ (hex2 (Char.code ch))
  else String.make 1 ch

let escape_of_string quote x : string =
  let esc = escape_of_char quote in
  let res = ref "" in
  for i = 0 to (String.length x -1) do
    res := !res ^ (esc x.[i])
  done;
  (String.make 1) quote ^ !res ^ (String.make 1 quote)

let py_dquote_of_string : string -> string = escape_of_string '"';;
let c_quote_of_string : string  -> string = escape_of_string '"';;
let py_quote_of_string : string  -> string = escape_of_string '\'';;

let string_of_char c : string = String.make 1 c;;

let bin_char2int s =
  let c = Char.code s in
  match s with
  | '0' -> 0
  | '1' -> 1
  | _ -> raise (StringError ("'" ^ (string_of_char s) ^ "' not binary digit"))

let oct_char2int s =
  let c = Char.code s in
  match s with
    _ when (s >= '0' && s <= '7') ->
      c - (Char.code '0')
  | _ -> raise (StringError ("'" ^ (string_of_char s) ^ "' not octal digit"))

let dec_char2int s =
  let c = Char.code s in
  match s with
    _ when (s >= '0' && s <= '9') ->
      c - (Char.code '0')
  | _ -> raise (StringError ("'" ^ (string_of_char s) ^ "' not decimal digit"))

let hex_char2int s =
  let c = Char.code s in
  match s with
    _ when (s >= '0' && s <= '9') ->
      c - (Char.code '0')
  | _ when (s >= 'a' && s <= 'f') ->
      (c - (Char.code 'a')) + 10
  | _ when (s >= 'A' && s <= 'F') ->
      (c - (Char.code 'A')) + 10
  | _ -> raise (StringError ("'" ^ (string_of_char s) ^ "' not hexadecimal digit"))


let binint_of_string s =
  let len = String.length s in
  let value = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] <> '_'
    then value := !value * 2 + (bin_char2int s.[i])
  done;
  !value

let octint_of_string s =
  let len = String.length s in
  let value = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] <> '_'
    then value := !value * 8 + (oct_char2int s.[i])
  done;
  !value

let decint_of_string s =
  let len = String.length s in
  let value = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] <> '_'
    then value := !value * 10 + (dec_char2int s.[i])
  done;
  !value

let hexint_of_string s =
  let len = String.length s in
  let value = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] <> '_'
    then value := !value * 16 + (hex_char2int s.[i])
  done;
  !value

let floating_of_string s' =
  let dst = ref 0 in
  let s = Bytes.of_string s' in
  for src = 0 to String.length s' - 1 do
    if s'.[src] <> '_'
    then begin
      Bytes.set s (!dst) (s'.[src]);
      incr dst
    end
  done;
  float_of_string (Bytes.to_string (Bytes.sub s 0 !dst))

(* WARNING: THIS CODE WILL NOT WORK FOR THE HIGHER PLANES
  BECAUSE OCAML ONLY SUPPORTS 31 bit signed integers;
  THIS CODE REQUIRES 32 bits [This can be fixed by using
  negative codes but hasn't been done]

  HAPPINESS: Since the above note was posted,
  ISO10646/Unicode has agreed on a 20 bit address
  space for code points.
*)

(* parse the first utf8 encoded character of a string s
  starting at index position i, return a pair
  consisting of the decoded integers, and the position
  of the first character not decoded.

  If the first character is bad, it is returned,
  otherwise if the encoding is bad, the result is
  an unspecified value.

  Fails if the index is past or at
  the end of the string.

  COMPATIBILITY NOTE: if this function is called
  with a SINGLE character string, it will return
  the usual value for the character, in range
  0 .. 255
*)

let parse_utf8 (s : string)  (i : int) : int * int =
  let ord = int_of_char
  and n = (String.length s)  - i
  in
  if n <= 0 then
    failwith
    (
      "parse_utf8: index "^ string_of_int i^
      " >= "^string_of_int (String.length s)^
      " = length of '" ^ s ^ "'"
    )
  else let lead = ord (s.[i]) in
    if (lead land 0x80) = 0 then
      lead land 0x7F,i+1 (* ASCII *)
    else if lead land 0xE0 = 0xC0 && n > 1 then
      ((lead land 0x1F)  lsl  6) lor
        (ord(s.[i+1]) land 0x3F),i+2
    else if lead land 0xF0 = 0xE0 && n > 2 then
      ((lead land 0x1F) lsl 12) lor
        ((ord(s.[i+1]) land 0x3F)  lsl 6) lor
        (ord(s.[i+2]) land 0x3F),i+3
    else if lead land 0xF8 = 0xF0 && n > 3 then
      ((lead land 0x1F) lsl 18) lor
        ((ord(s.[i+1]) land 0x3F)  lsl 12) lor
        ((ord(s.[i+2]) land 0x3F)  lsl 6) lor
        (ord(s.[i+3]) land 0x3F),i+4
    else if lead land 0xFC = 0xF8 && n > 4 then
      ((lead land 0x1F) lsl 24) lor
        ((ord(s.[i+1]) land 0x3F)  lsl 18) lor
        ((ord(s.[i+2]) land 0x3F)  lsl 12) lor
        ((ord(s.[i+3]) land 0x3F)  lsl 6) lor
        (ord(s.[i+4]) land 0x3F),i+5
    else if lead land 0xFE = 0xFC && n > 5 then
      ((lead land 0x1F) lsl 30) lor
        ((ord(s.[i+1]) land 0x3F)  lsl 24) lor
        ((ord(s.[i+2]) land 0x3F)  lsl 18) lor
        ((ord(s.[i+3]) land 0x3F)  lsl 12) lor
        ((ord(s.[i+4]) land 0x3F)  lsl 6) lor
        (ord(s.[i+5]) land 0x3F),i+6
    else lead, i+1  (* error, just use bad character *)

(* convert an integer into a utf-8 encoded string of bytes *)
let utf8_of_int i =
  let chr x = String.make 1 (Char.chr x) in
  if i < 0x80 then
     chr(i)
  else if i < 0x800 then
     chr(0xC0 lor ((i lsr 6) land 0x1F))  ^
      chr(0x80 lor (i land 0x3F))
  else if i < 0x10000 then
     chr(0xE0 lor ((i lsr 12) land 0xF)) ^
      chr(0x80 lor ((i lsr 6) land 0x3F)) ^
      chr(0x80 lor (i land 0x3F))
  else if i < 0x200000 then
     chr(0xF0 lor ((i lsr 18) land 0x7)) ^
      chr(0x80 lor ((i lsr 12) land 0x3F)) ^
      chr(0x80 lor ((i lsr 6) land 0x3F)) ^
      chr(0x80 lor (i land 0x3F))
  else if i < 0x4000000 then
     chr(0xF8 lor ((i lsr 24) land 0x3)) ^
      chr(0x80 lor ((i lsr 18) land 0x3F)) ^
      chr(0x80 lor ((i lsr 12) land 0x3F)) ^
      chr(0x80 lor ((i lsr 6) land 0x3F)) ^
      chr(0x80 lor (i land 0x3F))
  else chr(0xFC lor ((i lsr 30) land 0x1)) ^
    chr(0x80 lor ((i lsr 24) land 0x3F)) ^
    chr(0x80 lor ((i lsr 18) land 0x3F)) ^
    chr(0x80 lor ((i lsr 12) land 0x3F)) ^
    chr(0x80 lor ((i lsr 6) land 0x3F)) ^
    chr(0x80 lor (i land 0x3F))

let unescape (s: string)  : string =
  let hex_limit = 2 in
  let n = String.length s in
  let s' = Buffer.create 1000 in
  let deferred = ref 0 in

  (* tack char deferres tacking spaces until
     the next non-space is received
  *)
  let tack_char ch =
    if ch = ' ' then incr deferred
    else begin
      if !deferred<>0 then begin
        Buffer.add_string s' (String.make !deferred ' ');
        deferred := 0
      end;
      Buffer.add_char s' ch
    end
  in

  (* tack string always flushes deferred characters *)
  let tack_string ss =
    if !deferred<> 0 then begin
       Buffer.add_string s' (String.make !deferred ' ');
       deferred := 0
     end;
     Buffer.add_string s' ss
  in
  let tack_utf8 code = tack_string (utf8_of_int code) in
  let i= ref 0 in
  while !i< n do let ch = s.[!i] in
    if ch = '\\' then begin
      tack_string ""; (* flush spaces before any slosh *)
      incr i;
      if !i = n then tack_char '\\'
      else match s.[!i] with
      | 'a'  -> tack_char  '\007'; incr i   (* 7 : bell *)
      | 'b'  -> tack_char  '\008'; incr i   (* 8 : backspace *)
      | 't'  -> tack_char  '\t'; incr i     (* 9 : horizontal tab *)

      (* Note that \n flushes deferred spaces! *)
      | 'n'  -> tack_char  '\n'; incr i     (* 10 : linefeed *)
      | 'r'  -> tack_char  '\r'; incr i     (* 13 : return *)
      | 'v'  -> tack_char  '\011'; incr i   (* vertical tab *)
      | 'f'  -> tack_char  '\012'; incr i   (* form feed *)
      | 'e'  -> tack_char  '\033'; incr i   (* 27: x1b: escape *)

      | '\\' -> tack_char  '\\'; incr i
      | '"'  -> tack_char  '"'; incr i (* NOTE OCAMLLEX BUG: TWO SPACES REQUIRED *)
      | '\'' -> tack_char  '\''; incr i

      (* this is the special case of \ spaces:
         if the spaces are followed by a newline,
         discard the spaces (and the newline!)
         otherwise we keep the spaces
      *)
      | ' ' ->
        while !i<n && s.[!i]=' ' do
           incr deferred;
           incr i
        done;
        if !i<n && s.[!i]='\n' then begin
          deferred :=0;
          incr i
        end

      (* \newline is thrown out, but defered spaces are output *)
      | '\n' -> incr i
      | 'x' ->
        begin
          incr i;
          let j = ref 0 and value = ref 0 in
          while
            (!i < n) &&
            (!j < hex_limit) &&
            (String.contains "0123456789ABCDEFabcdef" s.[!i]) do
            value := !value * 16 + (hex_char2int s.[!i]);
            incr i;
            incr j
          done;
          tack_utf8 !value
        end
      | 'u' ->
        begin
          incr i;
          let j = ref 0 and value = ref 0 in
          while
            (!i < n) &&
            (!j < 4) &&
            (String.contains "0123456789ABCDEFabcdef" s.[!i]) do
            value := !value * 16 + (hex_char2int s.[!i]);
            incr i;
            incr j
          done;
          tack_utf8 !value
        end
      | 'U' ->
        begin
          incr i;
          let j = ref 0 and value = ref 0 in
          while
            (!i < n) &&
            (!j < 8) &&
            (String.contains "0123456789ABCDEFabcdef" s.[!i]) do
            value := !value * 16 + (hex_char2int s.[!i]);
            incr i;
            incr j
          done;
          tack_utf8 !value
        end
      | 'd' ->
        begin
          incr i;
          let j = ref 0 and value = ref 0 in
          while
            (!i < n) &&
            (!j < 3) &&
            (String.contains "0123456789" s.[!i]) do
            value := !value * 10 + (dec_char2int s.[!i]);
            incr i;
            incr j
          done;
          tack_utf8 !value
        end
      | 'o' ->
        begin
          incr i;
          let j = ref 0 and value = ref 0 in
          while
            (!i < n) &&
            (!j < 3) &&
            (String.contains "01234567" s.[!i]) do
            value := !value * 8 + (oct_char2int s.[!i]);
            incr i;
            incr j
          done;
          tack_utf8 !value
        end

      | x -> tack_char '\\'; tack_char x;
        incr i;
    end else begin

      (* if we get a newline character, emit it
         without preceding spaces
      *)
      if s.[!i]='\n' then deferred :=0;
      tack_char s.[!i];
      incr i
    end
  done;
  tack_string "";  (* flush any deferred spaces *)
  Buffer.contents s'

(* this routine converts strings containing
   utf8 and/or \U \u escapes to a normalised
   ASCII form using \U and \u escapes
   for all codes in the range 0-1F, and >80
*)

