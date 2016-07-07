open Flx_util
open Flx_list

type reg_kind_t =
[
  | `regmatch of string * string
  | `reglex of string * string * string
]

type regular_args_t =
    int list *                      (* alphabet *)
    int *                           (* state count *)
    (int, Flx_bexpr.t) Hashtbl.t *  (* state->expression map *)
    (int * int, int) Hashtbl.t      (* transition matrix *)

let make_dtran nchars nstates matrix =
  (* transition matrix *)
  let d = Array.make_matrix nchars nstates (-1) in
  Hashtbl.iter
  (fun (c,s1) s2 ->
    d.(c).(s1) <- s2
  )
  matrix
  ;
  d

let emit_matrix tack nchars nstates d =
  (* find equivalent chars *)
  tack "  // state->state transition vectors for canonical characters\n";
  let canon = Array.make nchars 0 in
  for i = 0 to nchars - 1 do
    try
      for j = 0 to i do
        if d.(i) = d.(j) then
        begin
          canon.(i) <- j;
          if i = j then
          begin
            tack("  static int s"^si i^"["^si (nstates+1)^"]=\n");
            tack "  {0, // error\n"
            ;
            for state = 0 to nstates - 1 do
              if state mod 16 = 0 then tack "      ";
              let s = "     " ^ si (d.(i).(state)+1) in
              let n = String.length s in
              let s = String.sub s (n-3) 3 in
              tack s;
              if state <> nstates - 1 then tack ", ";
              if state mod 16 = 15 then tack "\n"
            done
            ;
            tack "\n";
            tack "  };\n"
          end;
          raise Not_found;
        end
      done (* j *)
    with Not_found -> ()
  done (* i *)
  ;
  tack ("  //char -> (state->state) lookup\n");
  tack ("  static int *matrix["^si nchars^"] =\n");
  tack "  {\n";
  for i = 0 to nchars - 1  do
    if i mod 16 = 0 then tack "      ";
    let s = "     s" ^ si canon.(i) in
    let n = String.length s in
    let s = String.sub s (n-4) 4 in
    tack s;
    if i <> nchars - 1 then tack ", ";
    if i mod 16 = 15 then tack "\n"
  done;
  tack "  };\n\n"


let regen b sr (alpha, nstates, cases, matrix) kind ge =
  let nchars = 256 in
  let tack s = bcat b s in

  tack ("  // regmatch/lex: error state=0, start state=1, valid states=1 to "^si nstates^"\n");

  let d = make_dtran nchars nstates matrix in
  begin match kind with | `reglex _ ->
  tack ("  // accepting states\n");
  tack ("  static int accept["^si (nstates+1)^"]={\n");
  tack (
    "    0," ^
    catmap ","
    (fun i -> match Hashtbl.mem cases i with
    | true -> "1" | false -> "0"
    )
    (nlist nstates)
  );
  tack "\n  };\n";
  | _ -> ()
  end
  ;
  emit_matrix tack nchars nstates d
  ;
  begin match kind with
  | `regmatch (lexeme_start,buffer_end) ->
    tack ("  char const*p = "^lexeme_start^";\n");
    tack ("  int state = 1;\n");
    tack ("  while(state && p != "^buffer_end^")\n");
    tack ("    state = matrix[int(*p++)][state];\n");
    tack ("  switch (state)\n")
  | `reglex (lexeme_start, buffer_end, lexeme_end) ->
    tack ("  char const *p= "^lexeme_start^";\n");
    tack ("  int state = 1;\n");
    tack ("  int last_state = 0;\n");
    tack ("  "^lexeme_end^" = NULL;\n");
    tack ("  if(accept[state]){\n");
    tack ("    last_state = state;\n");
    tack ("    "^lexeme_end^" = p;\n");
    tack ("  }\n");
    tack ("  while(state && p != "^buffer_end^") {\n");
    tack ("    state = matrix[int(*p++)][state];\n");
    tack ("    if(accept[state]){\n");
    tack ("      last_state = state;\n");
    tack ("      "^lexeme_end^" = p;\n");
    tack ("    }\n");
    tack ("  }\n");
    tack ("  switch (last_state)\n")
  | _ -> assert false
  end
  ;
  tack "  {\n";
  Hashtbl.iter
  (fun state expr ->
    tack
    (
      "    case " ^ si (state + 1)^ ":"^
      " return " ^ ge expr ^ ";\n"
    )
  )
  cases
  ;
  let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
  let s = Flx_print.string_of_string f ^ "," ^
    si sl ^ "," ^ si sc ^ "," ^
    si el ^ "," ^ si ec
  in
  tack ("    case 0: FLX_MATCH_FAILURE("^s^");\n");
  tack ("    default: FLX_MATCH_FAILURE("^s^");\n");
  tack "  }\n";

