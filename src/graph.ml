module type NodeType =
  sig
    type t
  end

module Make(NodeContent: NodeType) = struct
  type node =
   | Empty of int option
   | Node of NodeContent.t
  
  module NodeOrder  = 
  struct
    type t = node
    let compare = (fun n1 n2 -> if n1 == n2 then 0 else 1)
  end
  
  module NodeSet = Set.Make(NodeOrder)
  module NodeMap = Map.Make(NodeOrder)
  type transition_function = NodeSet.t NodeMap.t
  
  type cfg = 
    {  entry_point : node;
       exit_point  : node;
       pred        : node -> NodeSet.t;
       succ        : node -> NodeSet.t; }
  
  
  (**
    * Helper functions
    *)
  
  let cons e l = e::l
  
  (* node type predicates *)
  let is_empty_node node =
    match node with
    | Empty _ -> true
    | _ -> false
  
  let is_content_node node =
    not (is_empty_node node)
  
  let add_node node (set : NodeSet.t) =
    NodeSet.add node set
  
  (* adds a list of nodes to a set *)
  let add_nodes (nodes : node list) (set : NodeSet.t) = 
    List.fold_left 
      (fun set' node -> NodeSet.add node set') 
      set 
      nodes
  
  (* makes a new set containing the list of nodes *)
  let make_set (nodes : node list) : NodeSet.t =
    add_nodes nodes NodeSet.empty
  
  (* adds a single 'from -> to' arrow to map *)
  let add_arrow from_node to_node map : node -> NodeSet.t = 
    (fun n -> 
      if n == from_node
      then NodeSet.union (map from_node) (make_set [to_node])
      else map from_node)
  
  (* adds a list of arrow [(from, to),...] to map *)
  let add_arrows (arrows : ( NodeOrder.t * NodeOrder.t ) list ) (map : NodeOrder.t -> NodeSet.t) : NodeOrder.t -> NodeSet.t =
    List.fold_left 
      (fun map edge -> 
        let (from_node, to_node) = edge in
        add_arrow from_node to_node map) 
      map 
      arrows


  (**
    * Manipulation Functions
    *)
  
  let rec traverse node (succ : node -> NodeSet.t) seen f a = 
    if NodeSet.mem node seen 
    then (seen, a)
    else NodeSet.fold
      (fun node' (seen', a') -> traverse node' succ seen' f a')
      (succ node)
      ((add_nodes [node] seen), f a node)
  
  let fold_topdown f a g =
    traverse g.entry_point g.succ NodeSet.empty f a
  
  let fold_bottomup f a g =
    traverse g.entry_point g.pred NodeSet.empty f a
  
  let get_nodes g = 
    fold_topdown 
      (fun nodes node -> NodeSet.add node nodes)
      NodeSet.empty
      g
  
  
  (**
    *  Entry point 
    *)
  
  let make_graph  =
      ()
end
