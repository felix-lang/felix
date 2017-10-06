let si x = string_of_int x
let catmap x = Flx_util.catmap x
let sbt x = Flx_print.sbt x

let show bsym_table i = 
  try 
    Flx_bsym_table.find_id bsym_table i ^ "<" ^ si i ^ ">"
   with Not_found -> "index_" ^ si i

let showts bsym_table i ts =
  show bsym_table i ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]"

let showvars bsym_table vars = 
  catmap "," (fun (i,t)-> si i ^ " |-> " ^ sbt bsym_table t) vars


