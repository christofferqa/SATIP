module CFG = ControlFlowGraph

module IdentifierSet =
  Set.Make(String)

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

let pp_stm_set stm_set =
  let rec visit =
    (fun stm_set ->
      if (StmSet.cardinal stm_set) >= 2 then
        let stm = StmSet.choose stm_set in
        let () = Printf.printf "%s, " (Astpp.stm_to_string stm) in
        visit (StmSet.remove stm stm_set)
      else if (StmSet.cardinal stm_set) = 1 then
        let stm = StmSet.choose stm_set in
        Printf.printf "%s" (Astpp.stm_to_string stm)) in
  Printf.printf "[";
  visit stm_set;
  Printf.printf "]"

module CFGNodeSet =
  Set.Make (struct
    type t = CFG.node
    let compare n1 n2 = compare (CFG.get_node_id n1) (CFG.get_node_id n2)
  end)

module CFGNodeMap =
  Map.Make (struct
    type t = CFG.node
    let compare n1 n2 = compare (CFG.get_node_id n1) (CFG.get_node_id n2)
  end)