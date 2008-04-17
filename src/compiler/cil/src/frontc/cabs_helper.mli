val cabslu : Cabs.cabsloc
val missingFieldDecl :
  string * Cabs.decl_type * 'a list * Cabs.cabsloc
val isStatic : Cabs.spec_elem list -> bool
val isExtern : Cabs.spec_elem list -> bool
val isInline : Cabs.spec_elem list -> bool
val isTypedef : Cabs.spec_elem list -> bool
val get_definitionloc : Cabs.definition -> Cabs.cabsloc
val get_statementloc : Cabs.statement -> Cabs.cabsloc
val explodeStringToInts : string -> int64 list
val valueOfDigit : char -> int64
val d_cabsloc : unit -> Cabs.cabsloc -> Pretty.doc
