open Structures
module CFG = ControlFlowGraph

(* JOIN(v) := Union of [[w]] for w in succ(v): *)
let join_backwards_may node node_map cfg =
  List.fold_left
    (fun acc node_succ -> IdentifierSet.union acc (CFGNodeMap.find node_succ node_map))
    IdentifierSet.empty (CFG.succ node cfg)

(* JOIN(v) := Union of [[w]] for w in succ(v): *)
let join_backwards_must node node_map cfg =
  let union =
    List.fold_left
      (fun acc node_succ -> ExpSetCmpDesc.union acc (CFGNodeMap.find node_succ node_map))
      ExpSetCmpDesc.empty (CFG.succ node cfg) in
  List.fold_left
    (fun acc node_succ -> ExpSetCmpDesc.inter acc (CFGNodeMap.find node_succ node_map))
    union (CFG.succ node cfg)

(* JOIN(v) := Union of [[w]] for w in pred(v): *)
let join_forwards_may node node_map cfg =
  List.fold_left
    (fun acc node_pred -> StmSet.union acc (CFGNodeMap.find node_pred node_map))
    StmSet.empty (CFG.pred node cfg)

(* JOIN(v) := Intersection of [[w]] for w in pred(v): *)
let join_forwards_must node node_map cfg =
  let union =
    List.fold_left
      (fun acc node_pred -> ExpSetCmpDesc.union acc (CFGNodeMap.find node_pred node_map))
      ExpSetCmpDesc.empty (CFG.pred node cfg) in
  List.fold_left
    (fun acc node_pred -> ExpSetCmpDesc.inter acc (CFGNodeMap.find node_pred node_map))
    union (CFG.pred node cfg)

(* JOIN(v) := Intersection of [[w]] for w in pred(v): *)
let join_forwards_must_str node node_map cfg =
  let union =
    List.fold_left
      (fun acc node_pred -> StringSet.union acc (CFGNodeMap.find node_pred node_map))
      StringSet.empty (CFG.pred node cfg) in
  List.fold_left
    (fun acc node_pred -> StringSet.inter acc (CFGNodeMap.find node_pred node_map))
    union (CFG.pred node cfg)