val print_string : Format.formatter -> string -> unit

val print_big_int : Format.formatter -> Big_int.big_int -> unit

val print_opt :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a option ->
  unit

val print_tuple0 : Format.formatter -> unit -> unit

val print_tuple1 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  unit

val print_tuple2 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  unit

val print_tuple3 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  unit

val print_tuple4 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  unit

val print_tuple5 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  unit

val print_tuple6 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  (Format.formatter -> 'f -> unit) -> 'f ->
  unit

val print_tuple7 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  (Format.formatter -> 'f -> unit) -> 'f ->
  (Format.formatter -> 'g -> unit) -> 'g ->
  unit

val print_tuple8 :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  (Format.formatter -> 'f -> unit) -> 'f ->
  (Format.formatter -> 'g -> unit) -> 'g ->
  (Format.formatter -> 'h -> unit) -> 'h ->
  unit


val print_variant0 : Format.formatter -> string -> unit

val print_variant1 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  unit

val print_variant2 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  unit

val print_variant3 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  unit

val print_variant4 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  unit

val print_variant5 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  unit

val print_variant6 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  (Format.formatter -> 'f -> unit) -> 'f ->
  unit

val print_variant7 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  (Format.formatter -> 'f -> unit) -> 'f ->
  (Format.formatter -> 'g -> unit) -> 'g ->
  unit

val print_variant8 :
  Format.formatter -> string ->
  (Format.formatter -> 'a -> unit) -> 'a ->
  (Format.formatter -> 'b -> unit) -> 'b ->
  (Format.formatter -> 'c -> unit) -> 'c ->
  (Format.formatter -> 'd -> unit) -> 'd ->
  (Format.formatter -> 'e -> unit) -> 'e ->
  (Format.formatter -> 'f -> unit) -> 'f ->
  (Format.formatter -> 'g -> unit) -> 'g ->
  (Format.formatter -> 'h -> unit) -> 'h ->
  unit

val print_record1 :
  Format.formatter ->
  string -> (Format.formatter -> 'a -> unit) -> 'a ->
  unit

val print_record2 :
  Format.formatter ->
  string -> (Format.formatter -> 'a -> unit) -> 'a ->
  string -> (Format.formatter -> 'b -> unit) -> 'b ->
  unit

val print_record3 :
  Format.formatter ->
  string -> (Format.formatter -> 'a -> unit) -> 'a ->
  string -> (Format.formatter -> 'b -> unit) -> 'b ->
  string -> (Format.formatter -> 'c -> unit) -> 'c ->
  unit

val print_record4 :
  Format.formatter ->
  string -> (Format.formatter -> 'a -> unit) -> 'a ->
  string -> (Format.formatter -> 'b -> unit) -> 'b ->
  string -> (Format.formatter -> 'c -> unit) -> 'c ->
  string -> (Format.formatter -> 'd -> unit) -> 'd ->
  unit

val print_record5 :
  Format.formatter ->
  string -> (Format.formatter -> 'a -> unit) -> 'a ->
  string -> (Format.formatter -> 'b -> unit) -> 'b ->
  string -> (Format.formatter -> 'c -> unit) -> 'c ->
  string -> (Format.formatter -> 'd -> unit) -> 'd ->
  string -> (Format.formatter -> 'e -> unit) -> 'e ->
  unit

val print_record6 :
  Format.formatter ->
  string -> (Format.formatter -> 'a -> unit) -> 'a ->
  string -> (Format.formatter -> 'b -> unit) -> 'b ->
  string -> (Format.formatter -> 'c -> unit) -> 'c ->
  string -> (Format.formatter -> 'd -> unit) -> 'd ->
  string -> (Format.formatter -> 'e -> unit) -> 'e ->
  string -> (Format.formatter -> 'f -> unit) -> 'f ->
  unit

val print_record7 :
  Format.formatter ->
  string -> (Format.formatter -> 'a -> unit) -> 'a ->
  string -> (Format.formatter -> 'b -> unit) -> 'b ->
  string -> (Format.formatter -> 'c -> unit) -> 'c ->
  string -> (Format.formatter -> 'd -> unit) -> 'd ->
  string -> (Format.formatter -> 'e -> unit) -> 'e ->
  string -> (Format.formatter -> 'f -> unit) -> 'f ->
  string -> (Format.formatter -> 'g -> unit) -> 'g ->
  unit
