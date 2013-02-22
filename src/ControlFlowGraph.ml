(**
  * A Control Flow Graph built from an AST.
  *)

(**
  * Type declarations
  *)
    
  type node =
  | SimpleStm of Ast.stm
  | ExpJump of Ast.exp
  | Empty
  | Entry
  | Exit
      
  let stm_to_string stm =
    match stm.Ast.stm with
    | Ast.VarAssignment (id, e) -> Printf.sprintf "%s = %s;" id.Ast.identifier (Astpp.exp_to_string e)
    | Ast.PointerAssignment (e, e') -> Printf.sprintf "%s = %s;" (Astpp.exp_to_string e) (Astpp.exp_to_string e')
    | Ast.Output e -> Printf.sprintf "output %s;" (Astpp.exp_to_string e)
    | Ast.LocalDecl is -> Printf.sprintf "var %s;" (List.fold_left (fun s i -> i.Ast.identifier ^ ", " ^ s) "" is)
    | Ast.Return e -> Printf.sprintf "return %s;" (Astpp.exp_to_string e)
    | _ -> "Error"
      
  let pp nc =
   match nc with
    | SimpleStm s -> Printf.sprintf "%s" (stm_to_string s)
    | ExpJump e -> Printf.sprintf "%s" (Astpp.exp_to_string e)
    | Empty -> Printf.sprintf "%s" "Empty"
    | Entry -> Printf.sprintf "%s" "Entry"
    | Exit -> Printf.sprintf "%s" "Exit"
      
  module NodeType = struct
    type t = node
    let pp = pp
  end
    
  module Graph = DirectedGraph.Make(NodeType)
  type t = Graph.node * Graph.t * Graph.node
    
  let source_pred g node = List.length (Graph.pred node g) = 0
  let sink_pred g node = List.length (Graph.succ node g) = 0
    
  let get_sources g = Graph.select_nodes (source_pred g) g
  let get_sinks g = Graph.select_nodes (sink_pred g) g
    
  let (|>) x f = f x
    
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
      let node = Graph.make_node (SimpleStm stm) in
      Graph.add node Graph.empty
	
    | Ast.IfThen (exp, stm) ->
      (* graph 'nodes' *)
      let branch_node = Graph.make_node (ExpJump exp) in
      let exit_node   = Graph.make_node Empty in
      
      let stm_cfg = generate_cfg_of_block_stm stm in
      let stm_cfg_sinks = get_sinks stm_cfg in	
      let stm_cfg_sources = get_sources stm_cfg in
      
      Graph.add_many [branch_node; exit_node] stm_cfg |>
      Graph.connect_many [branch_node] (exit_node::stm_cfg_sources) |>
      Graph.connect_many stm_cfg_sinks [exit_node] 
	      
    | Ast.IfThenElse (e, block1, block2) ->
      (* Recursively create graphs for the stms and combine them to one graph obj (will have 2 sources and 2 sinks) *)
      let stm1_cfg = generate_cfg_of_block_stm block1 in
      let stm2_cfg = generate_cfg_of_block_stm block2 in
      let stms_cfg = Graph.combine stm1_cfg stm2_cfg in
      
      (* Nodes to be connected *)
      let branch_node = Graph.make_node (ExpJump e) in
      let exit_node   = Graph.make_node Empty in
      let cfg_sinks   = get_sinks stms_cfg in	
      let cfg_sources = get_sources stms_cfg in

      Graph.add branch_node stms_cfg |>
      Graph.add exit_node |>
      Graph.connect_many [branch_node] cfg_sources |>
      Graph.connect_many cfg_sinks [exit_node]

    | Ast.While (e, block) ->
      (* graph 'nodes' *)
      let branch_node = Graph.make_node (ExpJump e) in
      let entry_node  = Graph.make_node Empty in
      let exit_node   = Graph.make_node Empty in
      
      let stm_cfg = generate_cfg_of_block_stm block in
      let stm_cfg_sinks = get_sinks stm_cfg in	
      let stm_cfg_sources = get_sources stm_cfg in
    
      Graph.add_many [branch_node; entry_node; exit_node] stm_cfg |>
      Graph.connect entry_node branch_node |>
      Graph.connect_many [branch_node] (exit_node::stm_cfg_sources) |>
      Graph.connect_many stm_cfg_sinks [branch_node] 

  and generate_cfg_of_block_stm stms =
    List.fold_left
      (fun acc_cfg stm -> 
	let stm_cfg = generate_cfg_of_single_stm stm in
	let stm_cfg_sources = get_sources stm_cfg in
	let acc_cfg_sinks = get_sinks acc_cfg in
	let cfg' = Graph.combine acc_cfg stm_cfg in
	Graph.connect_many acc_cfg_sinks stm_cfg_sources cfg')
      Graph.empty
      stms

  let generate_cfg_from_function func =
    let cfg = generate_cfg_of_block_stm func.Ast.function_decl.Ast.function_body in
    let cfg_sinks = get_sinks cfg in
    let cfg_sources = get_sinks cfg in
    let entry_node = Graph.make_node Entry in
    let exit_node = Graph.make_node Exit in

    Graph.add_many [entry_node; exit_node] cfg |>
    Graph.connect_many cfg_sinks [exit_node] |>
    Graph.connect_many [entry_node] cfg_sources |>
    (fun cfg -> (entry_node, cfg, exit_node))

  let fold f acc (_,g,_) =
    Graph.fold f acc g

