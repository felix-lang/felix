type version_data_t =
{
  version_string : string;
  build_time_float : float;
  build_time : string;
}

let version_data = ref
{
  version_string = "no version";
  build_time_float = -1.0;
  build_time = "0000-00-00";
}



