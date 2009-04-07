open Cil
val ptname: typeinfo -> string
val ciname: compinfo -> string
val einame: enuminfo -> string
val viname: varinfo -> string

val pci: compinfo -> string
val pcci: compinfo -> string

val pei: enuminfo -> string
val pcei: enuminfo -> string

val pcomp: compinfo -> string


val soi: ikind -> string
val sof: fkind -> string

val cvqual: attributes -> string

val attrof: typ -> attributes

val c_name: global -> string option

val achk: string -> bool

val isanont: typ -> bool
val isanon: global -> bool

val is_cstruct_field: typ -> bool
val ispublic: string -> bool

val flx_name': global -> string option
