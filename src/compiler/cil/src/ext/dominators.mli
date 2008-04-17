

(** Compute dominators using data flow analysis *)
(** Author: George Necula   
      5/28/2004 
 **)

(** Invoke on a code after filling in the CFG info and it computes the 
 * immediate dominator information. We map each statement to its immediate 
 * dominator (None for the start statement, and for the unreachable 
 * statements). *)
val computeIDom: ?doCFG:bool -> Cil.fundec -> Cil.stmt option Inthash.t

type tree

(** returns the IDoms and a map from statement ids to 
 *  the set of statements that are dominated *)
val computeDomTree: ?doCFG:bool -> 
  Cil.fundec -> 
  Cil.stmt option Inthash.t * tree

(** This is like Inthash.find but gives an error if the information is 
 * Not_found *)
val getIdom:  Cil.stmt option Inthash.t -> Cil.stmt -> Cil.stmt option

(** Check whether one statement dominates another. *)
val dominates: Cil.stmt option Inthash.t -> Cil.stmt -> Cil.stmt -> bool

(** Return a list of statements dominated by the argument *)
val children: tree -> Cil.stmt -> Cil.stmt list

type order = PreOrder | PostOrder

(** Iterate over a dominator tree *)
val domTreeIter: (Cil.stmt -> unit) -> order -> tree -> unit

(** Compute the start of the natural loops. This assumes that the "idom" 
 * field has been computed. For each start, keep a list of origin of a back 
 * edge. The loop consists of the loop start and all predecessors of the 
 * origins of back edges, up to and including the loop start *)
val findNaturalLoops: Cil.fundec -> 
                      Cil.stmt option Inthash.t -> 
                      (Cil.stmt * Cil.stmt list) list
