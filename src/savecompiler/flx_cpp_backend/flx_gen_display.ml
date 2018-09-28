let strd the_display props =
  if the_display = [] then
  (if List.mem `Requires_ptf props then "(FLX_FPAR_PASS_ONLY)" else "()")
  else
  (if List.mem `Requires_ptf props then "(FLX_FPAR_PASS " else "(") ^
    Flx_util.cat ", " the_display ^ ")"

