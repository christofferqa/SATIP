module CFG = ControlFlowGraph

module ExpSet =
  Set.Make(struct
    type t = Ast.exp
    let compare e1 e2 = compare e1.Ast.exp_id e2.Ast.exp_id
  end)

module StmSet =
  Set.Make(struct
    type t = Ast.stm
    let compare e1 e2 = compare e1.Ast.stm_id e2.Ast.stm_id
  end)

module CFGNodeMap =
  Map.Make (struct
    type t = CFG.node
    let compare n1 n2 = compare (CFG.get_node_id n1) (CFG.get_node_id n2)
  end)

module IdentifierSet =
  Set.Make(String)