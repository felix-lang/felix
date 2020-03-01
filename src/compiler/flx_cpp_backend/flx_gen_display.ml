let strd the_display props =
  if the_display = [] then
  (if List.mem `Requires_ptf props then "(ptf)" else "()")
  else
  (if List.mem `Requires_ptf props then "(ptf, " else "(") ^
    Flx_util.cat ", " the_display ^ ")"

