(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *)

module CFG = ControlFlowGraph
module CFGNodeSet = Set.Make(ControlFlowGraph.CFGNode)
module CFGNodeMap = Map.Make(ControlFlowGraph.CFGNode)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end

module Make(Ord: OrderedType) = struct
  module OrdSet = Set.Make(Ord)
  module OrdSetUtils = SetUtils.Make(Ord)
  
  module OrdMap = Map.Make(Ord)
  module OrdMapUtils = MapUtils.Make(Ord)
  
  type strategy =
    | Forwards
    | Backwards
  
  let dep strategy node cfg =
    let nodes =
      match strategy with
      | Forwards -> CFG.pred_all node cfg
      | Backwards -> CFG.succ_all node cfg in
    List.fold_left
      (fun acc node -> CFGNodeSet.add node acc)
      CFGNodeSet.empty nodes
  
  (* JOIN(v) := Union of [[w]] for w in succ(v): *)
  let join_backwards_may node node_map cfg =
    List.fold_left
      (fun acc node_succ -> OrdSet.union acc (CFGNodeMap.find node_succ node_map))
      OrdSet.empty (CFG.succ node cfg)
  
  (* JOIN(v) := Union of [[w]] for w in succ(v): *)
  let join_backwards_must node node_map cfg =
    let union =
      List.fold_left
        (fun acc node_succ -> OrdSet.union acc (CFGNodeMap.find node_succ node_map))
        OrdSet.empty (CFG.succ node cfg) in
    List.fold_left
      (fun acc node_succ -> OrdSet.inter acc (CFGNodeMap.find node_succ node_map))
      union (CFG.succ node cfg)
  
  (* JOIN(v) := Union of [[w]] for w in pred(v): *)
  let join_forwards_may node node_map cfg =
    List.fold_left
      (fun acc node_pred -> OrdSet.union acc (CFGNodeMap.find node_pred node_map))
      OrdSet.empty (CFG.pred node cfg)
  
  (* JOIN(v) := Intersection of [[w]] for w in pred(v): *)
  let join_forwards_must node node_map cfg =
    let union =
      List.fold_left
        (fun acc node_pred -> OrdSet.union acc (CFGNodeMap.find node_pred node_map))
        OrdSet.empty (CFG.pred node cfg) in
    List.fold_left
      (fun acc node_pred -> OrdSet.inter acc (CFGNodeMap.find node_pred node_map))
      union (CFG.pred node cfg)
  
  let pp_set_solution node_map =
    CFGNodeMap.iter
      (fun node set -> 
        let node_content = CFG.get_node_content node in
        Printf.printf "%s  -> " (CFG.node_content_to_string node_content);
        OrdSetUtils.pp_set set;
        print_newline())
      node_map
  
  let pp_map_solution node_map value_to_string =
    CFGNodeMap.iter
      (fun node map -> 
        let node_content = CFG.get_node_content node in
        Printf.printf "%s  -> " (CFG.node_content_to_string node_content);
        OrdMapUtils.pp_map map value_to_string;
        print_newline())
      node_map
end