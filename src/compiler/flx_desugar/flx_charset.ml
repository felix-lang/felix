type charset_t = bool array

let charset_of_string s =
  let x = Array.make 256 false in
  for i  = 0 to String.length s - 1 do
    x.(Char.code s.[i]) <- true
  done;
  x


let charset_of_int_range x1 x2 =
  let x = Array.make 256 false in
  for i = x1 to x2 do
    x.(i) <- true
  done
  ;
  x

let charset_of_range s1 s2 =
  if String.length s1 <> 1
  then
    failwith "Charset range(first) requires string length 1"
  ;
  if String.length s2 <> 1
  then
    failwith "Charset range(last) requires string length 1"
  ;
  let x1 = Char.code (s1.[0])
  and x2 = Char.code (s2.[0])
  in
    charset_of_int_range x1 x2

let charset_union x1 x2 =
  let x = Array.make 256 false in
  for i = 0 to 255 do
    x.(i) <- x1.(i) || x2.(i)
  done;
  x

let charset_inv y =
  let x = Array.make 256 false in
  for i = 0 to 255 do
    x.(i) <- not y.(i)
  done;
  x

let regexp_of_charset y =
  let res = ref `REGEXP_epsilon in
  for i = 0 to 255 do
    if y.(i) then res :=
      let r = `REGEXP_string (String.make 1 (Char.chr i)) in
      if !res = `REGEXP_epsilon
      then r
      else `REGEXP_alt ( !res, r)
  done
  ;
  !res

let regexp_underscore =
  regexp_of_charset (charset_of_int_range 0 255)

let eol = Char.code '\n'

let regexp_dot =
  regexp_of_charset
  (
    charset_union
      (charset_of_int_range 0 (eol - 1))
      (charset_of_int_range (eol + 1) 255)
  )

