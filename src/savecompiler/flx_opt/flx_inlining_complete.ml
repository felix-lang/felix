
let inlining_complete bsym_table i =
  match Flx_bsym_table.find_bbdcl bsym_table i with
  | Flx_bbdcl.BBDCL_fun (props,_,_,_,_,_) -> List.mem `Inlining_complete props
  | Flx_bbdcl.BBDCL_external_fun _ -> true

  | _ -> assert false


