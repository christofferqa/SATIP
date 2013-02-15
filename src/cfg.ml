(**
  * A Control Flow Graph built from an AST.
  *)

(**
  * Type declarations
  *)

type node_content =
 | SimpleStm of Ast.stm
 | ExpJump of Ast.exp

let pp (id,nc) =
  match nc with
  | Some SimpleStm _ -> Printf.sprintf "(%d, %s)" id "Stm"
  | Some ExpJump _ -> Printf.sprintf "(%d, %s)" id "Exp"
  | None -> Printf.sprintf "(%d, %s)" id "Empty"

module NodeType = struct
  type t = node_content option
  let pp = pp
end

module ControlFlowGraph = DirectedGraph.Make(NodeType)

let source_pred g node = List.length (ControlFlowGraph.pred node g) = 0
let sink_pred g node = List.length (ControlFlowGraph.succ node g) = 0
let empty_pred g node = match ControlFlowGraph.get_node_content node with | None -> true | _ -> false

let get_sources g = ControlFlowGraph.select_nodes (source_pred g) g
let get_sinks g = ControlFlowGraph.select_nodes (sink_pred g) g
let get_empty g = ControlFlowGraph.select_nodes (empty_pred g) g

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
    let node = ControlFlowGraph.make_node (Some (SimpleStm stm)) in
    ControlFlowGraph.add node ControlFlowGraph.empty

  | Ast.IfThen (exp, stm) ->
    (* graph 'nodes' *)
    let branch_node = ControlFlowGraph.make_node (Some (ExpJump exp)) in
    let exit_node   = ControlFlowGraph.make_node None in

    let stm_cfg = generate_cfg_of_block_stm stm in
    let stm_cfg_sinks = get_sinks stm_cfg in	
    let stm_cfg_sources = get_sources stm_cfg in

    let cfg = ControlFlowGraph.add_many [branch_node; exit_node] stm_cfg in
    let cfg = ControlFlowGraph.connect_many [branch_node] (exit_node::stm_cfg_sources) cfg in
    ControlFlowGraph.connect_many stm_cfg_sinks [exit_node] cfg

  | Ast.IfThenElse (e, block1, block2) ->
    (* Recursively create graphs for the stms and combine them to one graph obj (will have 2 sources and 2 sinks) *)
    let stm1_cfg = generate_cfg_of_block_stm block1 in
    let stm2_cfg = generate_cfg_of_block_stm block2 in
    let stms_cfg = ControlFlowGraph.combine stm1_cfg stm2_cfg in
    
    (* Nodes to be connected *)
    let branch_node = ControlFlowGraph.make_node (Some (ExpJump e)) in
    let exit_node   = ControlFlowGraph.make_node None in
    let cfg_sinks   = get_sinks stms_cfg in	
    let cfg_sources = get_sources stms_cfg in

    (* add the two new nodes *)
    let cfg = ControlFlowGraph.add branch_node stms_cfg in
    let cfg = ControlFlowGraph.add exit_node cfg in

    (* connect branch to sources and sinks to exit_node *)
    let cfg = ControlFlowGraph.connect_many [branch_node] cfg_sources cfg in
    ControlFlowGraph.connect_many cfg_sinks [exit_node] cfg

  | Ast.While (e, block) ->
    (* graph 'nodes' *)
    let branch_node = ControlFlowGraph.make_node (Some (ExpJump e)) in
    let exit_node   = ControlFlowGraph.make_node None in

    let stm_cfg = generate_cfg_of_block_stm block in
    let stm_cfg_sinks = get_sinks stm_cfg in	
    let stm_cfg_sources = get_sources stm_cfg in

    let cfg = ControlFlowGraph.add_many [branch_node; exit_node] stm_cfg in

    let cfg = ControlFlowGraph.connect_many [branch_node] (exit_node::stm_cfg_sources) cfg in
    ControlFlowGraph.connect_many stm_cfg_sinks [branch_node] cfg

and generate_cfg_of_block_stm stms =
  List.fold_left
    (fun acc_cfg stm -> 
      let stm_cfg = generate_cfg_of_single_stm stm in
      let stm_cfg_sources = get_sources stm_cfg in
      let acc_cfg_sinks = get_sinks acc_cfg in
      let cfg' = ControlFlowGraph.combine acc_cfg stm_cfg in
      ControlFlowGraph.connect_many acc_cfg_sinks stm_cfg_sources cfg')
    ControlFlowGraph.empty
    stms
