module Int_kind :
  sig
    type t =
      | Tiny
      | Short
      | Int
      | Long
      | Vlong
      | Utiny
      | Ushort
      | Uint
      | Ulong
      | Uvlong
      | Int8
      | Int16
      | Int32
      | Int64
      | Uint8
      | Uint16
      | Uint32
      | Uint64

    (** Convert the integer kind to a string. *)
    val to_string : t -> string

    (** Prints out a integer kind to a formatter. *)
    val print : Format.formatter -> t -> unit
  end

module Float_kind :
  sig
    type t =
      | Float
      | Double
      | Ldouble

    (** Convert the integer kind to a string. *)
    val to_string : t -> string

    (** Prints out a floating point kind to a formatter. *)
    val print : Format.formatter -> t -> unit
  end

(** Literals recognized by the lexer. *)
type t =
  | Int of Int_kind.t * string
  | Float of Float_kind.t * string
  | String of string
  | Cstring of string
  | Wstring of string
  | Ustring of string

(** Return if two literals are equivalent. *)
val eq : t -> t -> bool

(** Prints out a literal to a formatter. *)
val print : Format.formatter -> t -> unit
