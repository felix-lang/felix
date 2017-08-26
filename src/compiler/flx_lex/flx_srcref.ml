(** {6 Generic source reference manipulation}
 *
 * Note the special hack of forgetting the second filename when creating a
 * range: the alternative would be to record a complete list of lines. *)

(** type of a span between two positions in one file*)
type t =
  string * (* filename *)
  int * (* starting line number, 1 origin *)
  int * (* starting column, 1 origin *)
  int * (* ending line number, 1 origin *)
  int   (* ending column, 1 origin *)

let make srcref = srcref

let make_dummy name = make (name, 0, 0, 0, 0)

let to_tuple srcref = srcref

let dummy_sr = make_dummy "[flx_srcref] generated"

(** get range from first and last ranges *)
let rsrange (f1,sl1,sc1,el1,ec1) (f2,sl2,sc2,el2,ec2) =
  (f1,sl1,sc1,el2,ec2)

(** {6 Type specific operations} *)

let short_string_of_src (f,l1,c1,l2,c2) =
  if l1 = l2
  then
    f ^ ": line " ^ string_of_int l1 ^
    ", cols " ^ string_of_int c1 ^ " to " ^ string_of_int c2
  else
    f ^ ": line " ^ string_of_int l1 ^
    " col " ^ string_of_int c1 ^ " to " ^
    " line " ^ string_of_int l2 ^ " col " ^ string_of_int c2

let get_lines f context l1' l2' c1 c2 = (* first line is line 1 *)
  let l1 = max 1 (l1'-context) in
  let l2 = l2' + context in
  let n = String.length (string_of_int l2) in
  let fmt i =
    let s ="    " ^ string_of_int i in
    let m = String.length s in
    String.sub s (m-n) n
  in
  try
    let zlen = l1' = l2' && c1 = c2 in
    let buf = Buffer.create ((l2-l1+4) * 80) in
    let spc () = Buffer.add_char buf ' ' in
    let star() = Buffer.add_char buf (if zlen then '^' else '*') in
    let nl() = Buffer.add_char buf '\n' in
    let f = open_in f in
    for i = 1 to l1-1 do ignore(input_line f) done;
    let too_long = l2'-l1' > 20 in
    begin
      try
        for i = l1 to l2 do
          let s = input_line f in
          if too_long && i = l1'+3 then
            Buffer.add_string buf ("...\n")
          else if too_long && i > l1'+3 && i< l2'-3 then () else
          begin
            Buffer.add_string buf (fmt i ^": ");
            Buffer.add_string buf s;
            nl();
            if i = l1' && l1' = l2' then
            begin
              for i = 1 to n + 2 do spc() done;
              for i = 1 to c1 - 1 do spc() done;
              for i = c1 to c2 do star() done;
              nl()
            end
          end
        done
      with _ -> Buffer.add_string buf "<eof>\n"
    end
    ;
    close_in f;
    Buffer.contents buf
  with _ ->
    "*** Can't read file " ^ f ^ " lines " ^ fmt l1 ^ " thru " ^ fmt l2 ^ "\n"

let long_string_of_src (f,l1,c1,l2,c2) =
  short_string_of_src (f,l1,c1,l2,c2) ^
  "\n" ^
  let c2 = if l1 = l2 && c2 < c1 then c1 else c2 in
  get_lines f 1 l1 l2 c1 c2

let file (f,_,_,_,_) = f

let first_line_no (_,l,_,_,_) = l 

let first_line (f,l,_,_,_) =
  try
    let f = open_in f in
    for i = 1 to l-1 do ignore(input_line f) done;
    let line = string_of_int l ^ ": " ^ input_line f in
    close_in f;
    line
  with _ ->
    "line " ^ string_of_int l

