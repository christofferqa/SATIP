module CFG = ControlFlowGraph

module StringMap =
  Map.Make (String)

module CFGNodeMap =
  Map.Make (struct
    type t = CFG.node
    let compare n1 n2 = compare (CFG.get_node_id n1) (CFG.get_node_id n2)
  end)