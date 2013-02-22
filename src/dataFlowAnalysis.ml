(*
let join_forward_must (preds: CFG.node list) (succs: CFG.node list) = Intersection pred
let join_forward_may (preds: CFG.node list) (succs: CFG.node list) = Union pred
let join_backwards_must (preds: CFG.node list) (succs: CFG.node list) = Intersection succ
let join_backwards_may (preds: CFG.node list) (succs: CFG.node list) = Union succ
*)