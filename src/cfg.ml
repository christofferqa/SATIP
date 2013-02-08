(**
  * A Control Flow Graph built from an AST.
  *)



(**
  * Type declarations
  *)

type node_content =
 | SimpleStm of stm
 | ExpJump of exp

type node =
 | CFG_empty of int option
 | CFG_node of node_content
    
module NodeOrder : Set.OrderedType = 
struct
  type t = node
  let compare = Pervasives.(==)
end

module NodeSet = Set.make(NodeOrder)
module NodeMap = Map.make(NodeOrder)

type cfg = 
  {  entry_point : node;
     exit_point  : node;
     pred        : node -> NodeSet.t;
     succ        : node -> NodeSet.t; }

(**
  * Helper functions
  *)

(* node type predicates *)
let is_empty_node node =
  match node with
  | CFG_empty _ -> true
  | _ -> false

let is_content_node node =
  not is_empty_node node

(* adds a list of nodes to a set *)
let add_nodes nodes set = 
  List.fold_left (fun set' node -> NodeSet.add node set') set nodes

(* makes a new set containing the list of nodes *)
let make_set nodes =
  add_nodes nodes NodeSet.empty

(* adds a single 'from -> to' arrow to map *)
let add_arrow from_node to_node map = 
  (fun n -> 
    if n == from_node
    then NodeSet.union (map from_node) (make_set [to_node])
    else map from_node)

(* adds a list of arrow [(from, to),...] to map *)
let add_arrows arrows map =
  List.fold_left (fun map' (from_node, to_node) -> add_arrow from_node to_node map') map arrows

(**
  * A function to build a control flow graph from a program.
  * The returned node is the root-element of the CFG.
  *)


let rec generate_cfg_of_single_stm stm =
  match stm.stm with
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

  | Ast.IfThen e stms ->
    (* graph 'nodes' *)
    let branch_node = CFG_node (ExpJump e) in
    let stms_cfg    = generate_cfg_of_block_stm stms in
    let exit_node   = CFG_empty in

    (* graph edges *)
    let pred' = add_arrows [(exit_node, stms_cfg.exit_node); 
                            (exit_node, branch_node); 
                            (stms_cfg.entry_point, branch_node)] stms_cfg.pred in

    let succ' = add_arrows [(stms_cfg.exit_node, exit_node); 
                            (branch_node, exit_node);
                            (branch_node, stms_cfg.entry_point)] stms_cfg.succ in
    
    (* graph construction *)
    {  entry_point = branch_node;
       exit_point  = exit_node;
       pred        = pred';
       succ        = succ'; }

  | Ast.IfThenElse e ss1 ss2 ->
    (* graph 'nodes' *)
    let branch_node = CFG_node (ExpJump e) in
    let ss1_cfg     = generate_cfg_of_block_stm ss1 in
    let ss2_cfg     = generate_cfg_of_block_stm ss2 in
    let exit_node   = CFG_empty None in

    (* graph edges *)
    let pred  = (fun n -> NodeSet.union (ss1_cfg.pred n) (ss2_cfg.pred n)) in
    let pred' = add_arrows [(exit_node, ss1_cfg.exit_node); 
                            (exit_node, ss2_cfg.exit_node); 
                            (ss1_cfg.entry_point, branch_node); 
                            (ss2_cfg.entry_point, branch_node)] pred in

    let succ  = (fun n -> NodeSet.union (ss1_cfg.succ n) (ss2_cfg.succ n)) in
    let succ' = add_arrows [(ss1_cfg.exit_node, exit_node); 
                            (ss2_cfg.exit_node, exit_node); 
                            (branch_node, ss1_cfg.entry_point); 
                            (branch_node, ss2_cfg.entry_point)] succ in
    
    (* graph construction *)
    {  entry_point = branch_node;
       exit_point  = exit_node;
       pred        = pred';
       succ        = succ'; }

  | Ast.While e stms ->
    (* graph 'nodes' *)
    let branch_node = CFG (ExpJump e) in
    let stms_cfg = generate_cfg_of_block_stm stms in

    (* graph edges *)
    let pred' = add_arrows [(branch_node, stms_cfg.exit_point); 
                            (stms_cfg.entry_point, branch_node)] stms_cfg.pred in

    let succ' = add_arrows [(stms_cfg.exit_node, branch_node); 
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
      let pred' = add_arrow cfg.exit_point cfg.entry_point pred in
      let succ' = add_arrow cfg.entry_point cfg.exit_point succ in
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


let rec traverse node succ seen f a = 
  if NodeSet.mem node seen 
  then (seen, a)
  else List.fold_left 
    (fun (seen', a') node' -> traverse node' succ seen' f a')
    ((add_notes [node] seen), f a node)
    (succ node)

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

let rec compress_graph g =
  let nodes = fold_topdown (fun nodes node -> node::nodes) [] g in
  let (discard, keep) = List.partition ( fun n -> ( is_empty_node n ) && 
                                                  ( List.length ( g.pred n ) > 0 ) &&
                                                  ( List.length ( g.succ n ) > 0 ))
                                       nodes in
  let next' = 
    List.fold_left
  
  
  () ---> [] ---> ()
  () -----------> ()
    

    

(**
  *  Entry point 
  *)



let make_cfg (prog: Ast.program) : cfg =
    Error.tip_not_implemented_yet "cfg.ml" "Control Flow Graph not yet implemented."


