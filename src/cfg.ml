(**
  * A Control Flow Graph built from an AST.
  *)



(**
  * Type declarations
  *)

type node_content =
 | SimpleStm of Ast.stm
 | ExpJump of Ast.exp

type node =
 | CFG_empty of int option
 | CFG_node of node_content
    
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
  | CFG_empty _ -> true
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
  * A function to build a control flow graph from a program.
  * The returned node is the root-element of the CFG.
  *)


let rec generate_cfg_of_single_stm stm =
  match stm.Ast.stm with
  | Ast.VarAssignment _ 
  | Ast.PointerAssignment _
  | Ast.Output _
  | Ast.LocalDecl _
  | Ast.Return _ -> 
    let node = CFG_node (SimpleStm stm) in
    {  entry_point = node;
       exit_point  = node;
       pred        = (fun _ -> NodeSet.empty);
       succ        = (fun _ -> NodeSet.empty) }

  | Ast.IfThen (e, s) ->
    (* graph 'nodes' *)
    let branch_node = CFG_node (ExpJump e) in
    let stms_cfg    = generate_cfg_of_block_stm s in
    let exit_node   = CFG_empty None in

    (* graph edges *)
    let pred' = add_arrows [(exit_node, stms_cfg.exit_point); 
                            (exit_node, branch_node); 
                            (stms_cfg.entry_point, branch_node)] stms_cfg.pred in

    let succ' = add_arrows [(stms_cfg.exit_point, exit_node); 
                            (branch_node, exit_node);
                            (branch_node, stms_cfg.entry_point)] stms_cfg.succ in
    
    (* graph construction *)
    {  entry_point = branch_node;
       exit_point  = exit_node;
       pred        = pred';
       succ        = succ'; }

  | Ast.IfThenElse (e, ss1, ss2) ->
    (* graph 'nodes' *)
    let branch_node = CFG_node (ExpJump e) in
    let ss1_cfg     = generate_cfg_of_block_stm ss1 in
    let ss2_cfg     = generate_cfg_of_block_stm ss2 in
    let exit_node   = CFG_empty None in

    (* graph edges *)
    let pred  = (fun n -> NodeSet.union (ss1_cfg.pred n) (ss2_cfg.pred n)) in
    let pred' = add_arrows [(exit_node, ss1_cfg.exit_point); 
                            (exit_node, ss2_cfg.exit_point); 
                            (ss1_cfg.entry_point, branch_node); 
                            (ss2_cfg.entry_point, branch_node)] pred in

    let succ  = (fun n -> NodeSet.union (ss1_cfg.succ n) (ss2_cfg.succ n)) in
    let succ' = add_arrows [(ss1_cfg.exit_point, exit_node); 
                            (ss2_cfg.exit_point, exit_node); 
                            (branch_node, ss1_cfg.entry_point); 
                            (branch_node, ss2_cfg.entry_point)] succ in
    
    (* graph construction *)
    {  entry_point = branch_node;
       exit_point  = exit_node;
       pred        = pred';
       succ        = succ'; }

  | Ast.While (e, stms) ->
    (* graph 'nodes' *)
    let branch_node = CFG_node (ExpJump e) in
    let stms_cfg = generate_cfg_of_block_stm stms in

    (* graph edges *)
    let pred' = add_arrows [(branch_node, stms_cfg.exit_point); 
                            (stms_cfg.entry_point, branch_node)] stms_cfg.pred in

    let succ' = add_arrows [(stms_cfg.exit_point, branch_node); 
                            (branch_node, stms_cfg.entry_point)] stms_cfg.succ in

    (* graph construction *)
    {  entry_point = branch_node;
       exit_point  = branch_node;
       pred        = pred';
       succ        = succ' }

and generate_cfg_of_block_stm stms =
  let empty_node = CFG_empty None in
  List.fold_left
    (fun cfg stm -> 
      let stm_cfg = generate_cfg_of_single_stm stm in
      let pred' = add_arrow cfg.exit_point cfg.entry_point cfg.pred in
      let succ' = add_arrow cfg.entry_point cfg.exit_point cfg.succ in
      { entry_point = cfg.entry_point;
        exit_point  = stm_cfg.exit_point;
        pred        = pred';
        succ        = succ'; })

    { entry_point = empty_node;
      exit_point  = empty_node;
      pred        = (fun _ -> NodeSet.empty);
      succ        = (fun _ -> NodeSet.empty) }
    stms


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
  *  Optimizations
  *)
(*
let remove_pred g node = 
  (fun node -> ( is_empty_node node ) &&
               ( NodeSet.cardinal (g.pred node) > 0 ) &&
               ( NodeSet.cardinal (g.succ node) > 0 ))

(* Computes the set of reachable nodes, when you only follow the edge when p holds*)
let rec reachable_when (node : node) (p : node -> bool) (trans_func : node -> NodeSet.t)  : NodeSet.t =
  NodeSet.fold
    (fun node set -> 
      if not (p node)
      then NodeSet.add node set
      else NodeSet.union set (reachable_when node p trans_func))
    NodeSet.empty
    (trans_func node)

let condense_transition_function_to_map trans_func keep_nodes = 
  List.fold_left
    (fun map node -> 
      let neighbours = reachable_when node remove_pred f in
      NodeMap.add node neighbours map)
    nodes
    transition_function.empty
      
let condense_graph g =
  let all_nodes = fold_topdown cons [] g in 
  let keep = List.filter (fun n -> not (remove_pred n)) all_nodes in
  let pred' = condense_transition_function g.pred keep in
  let succ' = condense_transition_function g.succ keep in
  { entry_point = g.entry_point;
    exit_point  = g.exit_point;
    pred        = pred;
    succ        = succ }
*)
(**
  *  Entry point 
  *)


let nothing = ()

let make_cfg (prog: Ast.program)  =
    ()


