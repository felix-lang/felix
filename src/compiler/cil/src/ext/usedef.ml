
open Cil
open Pretty

module E = Errormsg

(** compute use/def information *)

module VS = Set.Make (struct 
                        type t = Cil.varinfo
                        (* Subtraction is safe since vids are always positive*)
                        let compare v1 v2 = v1.vid - v2.vid
                      end)

(** Set this global to how you want to handle function calls.
    This also returns a modified argument list which will be used for the
    purpose of Use analysis, in case you have a function that needs special
    treatment of its args. *)
let getUseDefFunctionRef: (exp -> exp list -> VS.t * VS.t * exp list) ref = 
  ref (fun func args -> (VS.empty, VS.empty, args))

(** Say if you want to consider a variable use.  This applies to
  variable reads only; see also considerVariableAddrOfAsUse *)
let considerVariableUse: (varinfo -> bool) ref = 
  ref (fun _ -> true)


(** Say if you want to consider a variable def *)
let considerVariableDef: (varinfo -> bool) ref = 
  ref (fun _ -> true)

(** Say if you want to consider a variable addrof as a use *)
let considerVariableAddrOfAsUse: (varinfo -> bool) ref = 
  ref (fun _ -> true)

(** Return any vars that should be considered "used" by an expression,
    other than the ones it refers to directly.  Deputy uses this for
    variables in Cast annotations. *)
let extraUsesOfExpr: (exp -> VS.t) ref =
  ref (fun _ -> VS.empty)

(* When this is true, only definitions of a variable without
   an offset are counted as definitions. So:
   a = 5; would be a definition, but
   a[1] = 5; would not.
   Exception: writing to a union field is considered to be a definition of
   the union even if this is set to true.*)
let onlyNoOffsetsAreDefs: bool ref = ref false

(** Should we ignore the contents of sizeof and alignof? *)
let ignoreSizeof: bool ref = ref true

let varUsed: VS.t ref = ref VS.empty
let varDefs: VS.t ref = ref VS.empty

class useDefVisitorClass : cilVisitor = object (self)
  inherit nopCilVisitor
      
  (** this will be invoked on variable definitions only because we intercept 
   * all uses of variables in expressions ! *)
  method vvrbl (v: varinfo) = 
    if (!considerVariableDef) v &&
      not(!onlyNoOffsetsAreDefs) then 
      varDefs := VS.add v !varDefs;
    SkipChildren

  (** If onlyNoOffsetsAreDefs is true, then we need to see the
   *  varinfo in an lval along with the offset. Otherwise just
   *  DoChildren *)
  method vlval (l: lval) =
    if !onlyNoOffsetsAreDefs then
      match l with
	(Var vi, NoOffset) ->
	  if (!considerVariableDef) vi then
	    varDefs := VS.add vi !varDefs;
	  SkipChildren
      | (Var vi, Field(fi, NoOffset)) when not fi.fcomp.cstruct ->
          (* If we are writing to a union field, treat that the same
             as a write to a union. *)
	  if (!considerVariableDef) vi then
	    varDefs := VS.add vi !varDefs;
	  SkipChildren
      | _ -> DoChildren
    else DoChildren

  method vexpr (e:exp) =
    let extra = (!extraUsesOfExpr) e in
    if not (VS.is_empty extra) then 
      varUsed := VS.union extra !varUsed;
    match e with
      Lval (Var v, off) -> 
        ignore (visitCilOffset (self :> cilVisitor) off);
        if (!considerVariableUse) v then begin
          varUsed := VS.add v !varUsed
	end;
        SkipChildren (* So that we do not see the v *)

    | AddrOf (Var v, off) 
    | StartOf (Var v, off) -> 
        ignore (visitCilOffset (self :> cilVisitor) off);
        if (!considerVariableAddrOfAsUse) v then 
          varUsed := VS.add v !varUsed;
        SkipChildren

    | SizeOfE _
    | AlignOfE _ when !ignoreSizeof -> SkipChildren

    | _ -> DoChildren

  (* For function calls, do the transitive variable read/defs *)
  method vinst i = match i with
      Call (lvo, f, args, _) -> begin
        (* we will compute the use and def that appear in 
         * this instruction. We also add in the stuff computed by 
         * getUseDefFunctionRef *)
        let use, def, args' = !getUseDefFunctionRef f args in
        varUsed := VS.union !varUsed use;
        varDefs := VS.union !varDefs def;
        
        (* Now visit the children of  "Call (lvo, f, args', _)" *)
        let self: cilVisitor = (self :> cilVisitor) in
        (match lvo with None -> ()
        | Some lv -> ignore (visitCilLval self lv));
        ignore (visitCilExpr self f);
        List.iter (fun arg -> ignore (visitCilExpr self arg)) args';
        SkipChildren
      end
    | Asm(_,_,slvl,_,_,_) -> List.iter (fun (_,s,lv) ->
	match lv with (Var v, off) ->
	  if s.[0] = '+' then
	    varUsed := VS.add v !varUsed;
	| _ -> ()) slvl;
	DoChildren
    | _ -> DoChildren
        
end

let useDefVisitor = new useDefVisitorClass 

(** Compute the use information for an expression (accumulate to an existing 
 * set) *)
let computeUseExp ?(acc=VS.empty) (e: exp) : VS.t = 
  varUsed := acc;
  ignore (visitCilExpr useDefVisitor e);
  !varUsed


(** Compute the use/def information for an instruction *)
let computeUseDefInstr ?(acc_used=VS.empty)
                       ?(acc_defs=VS.empty) 
                       (i: instr) : VS.t * VS.t = 
  varUsed := acc_used;
  varDefs := acc_defs;
  ignore (visitCilInstr useDefVisitor i);
  !varUsed, !varDefs


(** Compute the use/def information for a statement kind. Do not descend into 
 * the nested blocks. *)
let computeUseDefStmtKind ?(acc_used=VS.empty)
                          ?(acc_defs=VS.empty) 
                          (sk: stmtkind) : VS.t * VS.t =
  varUsed := acc_used;
  varDefs := acc_defs;
  let ve e = ignore (visitCilExpr useDefVisitor e) in 
  let _ = 
    match sk with 
      Return (None, _) -> ()
    | Return (Some e, _) -> ve e
    | If (e, _, _, _) -> ve e
    | Break _ | Goto _ | Continue _ -> ()
    | Loop (_, _, _, _) -> ()
    | Switch (e, _, _, _) -> ve e
    | Instr il -> 
        List.iter (fun i -> ignore (visitCilInstr useDefVisitor i)) il
    | TryExcept _ | TryFinally _ -> ()
    | Block _ -> ()
  in
  !varUsed, !varDefs

(* Compute the use/def information for a statement kind.
   DO descend into nested blocks *)
let rec computeDeepUseDefStmtKind ?(acc_used=VS.empty)
                                  ?(acc_defs=VS.empty) 
                                   (sk: stmtkind) : VS.t * VS.t =
  let handle_block b =
    List.fold_left (fun (u,d) s ->
      let u',d' = computeDeepUseDefStmtKind s.skind in
      (VS.union u u', VS.union d d')) (VS.empty, VS.empty)
      b.bstmts
  in
  varUsed := acc_used;
  varDefs := acc_defs;
  let ve e = ignore (visitCilExpr useDefVisitor e) in  
  match sk with 
    Return (None, _) -> !varUsed, !varDefs
  | Return (Some e, _) -> 
      let _ = ve e in
      !varUsed, !varDefs
  | If (e, tb, fb, _) ->
      let _ = ve e in
      let u, d = !varUsed, !varDefs in
      let u', d' = handle_block tb in
      let u'', d'' = handle_block fb in
      (VS.union (VS.union u u') u'', VS.union (VS.union d d') d'')
  | Break _ | Goto _ | Continue _ -> !varUsed, !varDefs
  | Loop (b, _, _, _) -> handle_block b
  | Switch (e, b, _, _) -> 
      let _ = ve e in
      let u, d = !varUsed, !varDefs in
      let u', d' = handle_block b in
      (VS.union u u', VS.union d d')
  | Instr il -> 
      List.iter (fun i -> ignore (visitCilInstr useDefVisitor i)) il;
      !varUsed, !varDefs
  | TryExcept _ | TryFinally _ -> !varUsed, !varDefs
  | Block b -> handle_block b

let computeUseLocalTypes ?(acc_used=VS.empty)
                         (fd : fundec)
    =
  List.fold_left (fun u vi ->
    ignore(visitCilType useDefVisitor vi.vtype);
    VS.union u (!varUsed)) acc_used fd.slocals
