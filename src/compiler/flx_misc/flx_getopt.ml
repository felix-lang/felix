open List
open Flx_util

let parse_option s =
  let n = String.length s in
  if n > 1 && s.[0]='-' then
    if n > 2 && s.[1]='-' then
    begin
      let j = ref 2 in
      while !j < n && s.[!j]<>'=' do incr j done;
      let key = String.sub s 2 (!j - 2) in
      let value =
        if !j<n && s.[!j]='=' then
          String.sub s (!j+1) (n - !j - 1)
        else
          ""
      in
        [key,value]
    end
    else
      [String.sub s 1 1, String.sub s 2 (n-2)]
  else ["",s]

let parse_options argv =
  concat (map parse_option (List.tl (Array.to_list argv)))

let get_key_value options key =
  catch_all (assoc key) options

let check_key options key =
  is_some (get_key_value options key)

let check_keys options keys =
  fold_left
    (fun b key -> b || (check_key options key) )
    false keys

let check_key_value options key value =
  let keyval = key,value in
  let rec aux = function
  | [] -> false
  | h :: t -> if keyval = h then true else aux t
  in aux options

let get_key_values options key =
  let values = ref [] in
  let rec aux  = function
  | [] ->  !values
  | (key',value) :: t ->
    if key=key' then values := value :: !values;
    aux t
  in rev (aux options)

let get_keys_values options keys =
  concat (map (get_key_values options) keys)

