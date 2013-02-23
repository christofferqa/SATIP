type content =
| SimpleStm of Ast.stm
| ExpJump of Ast.exp
| Empty
| Entry
| Exit
    
module NodeContent : DirectedGraph.T with type t = content = struct
  type t = content
  let pp = (fun nc ->
    match nc with
    | ExpJump e -> Printf.sprintf "%s" (Astpp.exp_to_string e)
    | Empty -> Printf.sprintf "%s" "Empty"
    | Entry -> Printf.sprintf "%s" "Entry"
    | Exit -> Printf.sprintf "%s" "Exit"
    | SimpleStm stm -> 
      match stm.Ast.stm with
      | Ast.VarAssignment (id, e) -> Printf.sprintf "%s = %s;" id.Ast.identifier (Astpp.exp_to_string e)
      | Ast.PointerAssignment (e, e') -> Printf.sprintf "%s = %s;" (Astpp.exp_to_string e) (Astpp.exp_to_string e')
      | Ast.Output e -> Printf.sprintf "output %s;" (Astpp.exp_to_string e)
      | Ast.LocalDecl is -> Printf.sprintf "var %s;" (List.fold_left (fun s i -> i.Ast.identifier ^ ", " ^ s) "" is)
      | Ast.Return e -> Printf.sprintf "return %s;" (Astpp.exp_to_string e)
      | _ -> "Doesnt Occur")
end
    
include DirectedGraph.Make(NodeContent)
    
let source_pred g node = List.length (pred node g) = 0
let sink_pred g node = List.length (succ node g) = 0
  
let get_sources g = select_nodes (source_pred g) g
let get_sinks g = select_nodes (sink_pred g) g
  
let (|>) x f = f x
  
let rec generate_cfg_of_single_stm stm =
  match stm.Ast.stm with
  | Ast.VarAssignment _ 
  | Ast.PointerAssignment _
  | Ast.Output _
  | Ast.LocalDecl _
  | Ast.Return _ -> 
    add (make_node (SimpleStm stm)) empty
      
  | Ast.IfThen (exp, stm) ->
    (* graph 'nodes' *)
    let branch_node = make_node (ExpJump exp) in
    let exit_node   = make_node Empty in
    
    let stm_cfg = generate_cfg_of_block_stm stm in
    let stm_cfg_sinks = get_sinks stm_cfg in	
    let stm_cfg_sources = get_sources stm_cfg in
      
    add_many [branch_node; exit_node] stm_cfg |>
    connect_many [branch_node] (exit_node::stm_cfg_sources) |>
    connect_many stm_cfg_sinks [exit_node] 
	      
    | Ast.IfThenElse (e, block1, block2) ->
      (* Recursively create graphs for the stms and combine them to one graph obj (will have 2 sources and 2 sinks) *)
      let stm1_cfg = generate_cfg_of_block_stm block1 in
      let stm2_cfg = generate_cfg_of_block_stm block2 in
      let stms_cfg = combine stm1_cfg stm2_cfg in
      
      (* Nodes to be connected *)
      let branch_node = make_node (ExpJump e) in
      let exit_node   = make_node Empty in
      let cfg_sinks   = get_sinks stms_cfg in	
      let cfg_sources = get_sources stms_cfg in

      add branch_node stms_cfg |>
      add exit_node |>
      connect_many [branch_node] cfg_sources |>
      connect_many cfg_sinks [exit_node]

    | Ast.While (e, block) ->
      (* graph 'nodes' *)
      let branch_node = make_node (ExpJump e) in
      let entry_node  = make_node Empty in
      let exit_node   = make_node Empty in
      
      let stm_cfg = generate_cfg_of_block_stm block in
      let stm_cfg_sinks = get_sinks stm_cfg in	
      let stm_cfg_sources = get_sources stm_cfg in
    
      add_many [branch_node; entry_node; exit_node] stm_cfg |>
      connect entry_node branch_node |>
      connect_many [branch_node] (exit_node::stm_cfg_sources) |>
      connect_many stm_cfg_sinks [branch_node] 

  and generate_cfg_of_block_stm stms =
    List.fold_left
      (fun acc_cfg stm -> 
	let stm_cfg = generate_cfg_of_single_stm stm in
	let stm_cfg_sources = get_sources stm_cfg in
	let acc_cfg_sinks = get_sinks acc_cfg in
	let cfg' = combine acc_cfg stm_cfg in
	connect_many acc_cfg_sinks stm_cfg_sources cfg')
      empty
      stms

  let generate_cfg_from_function func =
    let cfg = generate_cfg_of_block_stm func.Ast.function_decl.Ast.function_body in
    let cfg_sinks = get_sinks cfg in
    let cfg_sources = get_sources cfg in
    let entry_node = make_node Entry in
    let exit_node = make_node Exit in
    add_many [entry_node; exit_node] cfg |>
    connect_many cfg_sinks [exit_node] |>
    connect_many [entry_node] cfg_sources 



