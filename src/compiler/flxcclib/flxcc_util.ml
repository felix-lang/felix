open Cil
open Flx_ctypes
open List

let ptname {tname=tname} = tname
and ciname {cname=cname} = cname
and einame {ename=ename} = ename
and viname {vname=vname} = vname

let pci ci = match ci with
{cname=cname; cstruct=cstruct} ->
(if cstruct then "_struct_" else "_union_") ^ cname

let pcci ci = match ci with
{cname=cname; cstruct=cstruct} ->
(if cstruct then "struct " else " union ") ^ cname

let pei ei = match ei with
{ename=ename} -> "_enum_" ^ ename

let pcei ei = match ei with
{ename=ename} -> "enum " ^ ename

let pcomp pi = match pi with
{cname=name} -> name


let soi = function
| IBool -> "bool"
| IChar -> "char"
| ISChar -> "tiny"
| IUChar -> "utiny"
| IInt -> "int"
| IUInt -> "uint"
| IShort -> "short"
| IUShort -> "ushort"
| ILong -> "long"
| IULong -> "ulong"
| ILongLong -> "vlong"
| IULongLong -> "uvlong"

let sof = function
| FFloat -> "float"
| FDouble -> "double"
| FLongDouble -> "ldouble"

| IFloat -> "imaginary"
| IDouble -> "dimaginary"
| ILongDouble -> "limaginary"

| CFloat -> "complex"
| CDouble -> "dcomplex"
| CLongDouble -> "lcomplex"

let cvqual a =
  let const = ref false
  and volatile = ref false
  in
  List.iter
  (fun (Attr (s,_)) ->
    if s = "const" then const := true
    else if s = "volatile" then volatile := true
  )
  a
  ;
  if !const && !volatile then "cv"
  else if !const then "c"
  else if !volatile then "v"
  else ""

let attrof = function
| TVoid a
| TInt (_,a)
| TFloat (_,a)
| TPtr (_,a)
| TArray (_,_,a)
| TFun (_,_,_,a)
| TNamed (_,a)
| TComp (_,a)
| TEnum (_,a)
| TBuiltin_va_list a
-> a

let c_name = function
  | GType ({tname=tname},_) -> Some tname
  | GCompTag (ci,sr) -> Some (pcci ci)
  | GCompTagDecl (ci,_) -> Some (pcci ci)
  | GEnumTag (ei,_) -> Some "int"
  | GEnumTagDecl (ei,_) -> Some "int"
  | GVarDecl ({vname=vname},_) -> Some vname
  | GVar ({vname=vname},_,_) -> Some vname
  | GFun ({svar={vname=vname}},sr) -> Some vname
  | GAsm _ -> None
  | GPragma _ -> None
  | GText _ -> None
;;

let achk x =
  let a = "__anon" in
  let n = String.length a in
  String.length x > n &&
  a = String.sub x 0 n


let rec isanont t =  match t with
| TVoid _
| TInt _
| TFloat _ -> false
| TPtr (t,_) -> isanont t
| TArray (t,_,_) -> isanont t
| TFun (t,Some ps,_,_) ->
  fold_left (fun b (_,t,_)-> b || isanont t ) (isanont t) ps

| TFun (t,None,_,_) -> isanont t
| TNamed ({tname=tname},_) -> achk tname
| TComp ({cname=cname},_) -> achk cname
| TEnum _ -> false
| TBuiltin_va_list _ -> false

let isanon = function
  | GType ({tname=tname},_) -> achk tname
  | GCompTag ({cname=cname},_) -> achk cname
  | GCompTagDecl ({cname=cname},_) -> achk cname
  | GEnumTag ({ename=ename},_) -> achk ename
  | GEnumTagDecl ({ename=ename},_) -> achk ename
  | GVarDecl ({vname=vname},_) -> false
  | GVar ({vname=vname},_,_) -> false
  | GFun (fd,sr) -> false
  | GAsm _ -> true
  | GPragma _ -> true
  | GText _ -> true


(* got to be a named non function type *)
let is_cstruct_field t =  match t with
| TVoid _
| TInt _
| TFloat _
| TNamed _
| TPtr _
| TArray _
| TComp _
| TEnum _
  -> true

| TFun _
| TBuiltin_va_list _
  -> false

let ispublic s =
 String.length s < 2 || String.sub s 0 2 <> "__"

(* pure name *)
let flx_name' = function
  | GType ({tname=tname},_) -> Some tname
  | GCompTag (ci,sr) -> Some (pci ci)
  | GCompTagDecl (ci,_) -> Some (pci ci)
  | GEnumTag ({ename=ename},_) -> Some ename
  | GEnumTagDecl ({ename=ename},_) -> Some ename
  | GVarDecl ({vname=vname},_) -> Some vname
  | GVar ({vname=vname},_,_) -> Some vname
  | GFun ({svar={vname=vname}},sr) -> Some vname
  | GAsm _ -> None
  | GPragma _ -> None
  | GText _ -> None
