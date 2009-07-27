let find table key =
  try
    Some (Hashtbl.find table key)
  with Not_found ->
    None
