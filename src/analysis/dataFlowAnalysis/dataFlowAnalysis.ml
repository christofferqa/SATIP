open Structures
module CFG = ControlFlowGraph

(* JOIN(v) := Union of [[w]] for w in succ(v): *)
let join_backwards_may node nodes cfg =
  List.fold_left
    (fun acc node_succ -> IdentifierSet.union acc (CFGNodeMap.find node_succ nodes))
    IdentifierSet.empty (CFG.succ node cfg)

(* JOIN(v) := Union of [[w]] for w in pred(v): *)
let join_forwards_may node nodes cfg =
  List.fold_left
    (fun acc node_succ -> StmSet.union acc (CFGNodeMap.find node_succ nodes))
    StmSet.empty (CFG.pred node cfg)