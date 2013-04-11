(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  * @author Troels Leth Jensen, tleth@cs.au.dk.
  *)

module EAst = EnvironmentAst
module StringMap = Map.Make(SetUtils.String)

type content =
| Entry of Ast.function_decl
| Exit of Ast.function_decl
| SimpleStm of Ast.stm
| ExpJump of Ast.exp
| CallNode of Ast.identifier * Ast.exp * int
| AfterCallNode of Ast.identifier * Ast.exp * int
| Empty

let node_content_to_string node_content =
  match node_content with
  | Entry func -> Printf.sprintf "Entry(%s)" func.Ast.function_decl.Ast.function_name.Ast.identifier
  | Exit func -> Printf.sprintf "Exit(%s)" func.Ast.function_decl.Ast.function_name.Ast.identifier
  | SimpleStm stm -> Printf.sprintf "%s" (Astpp.simple_stm_to_string stm)
  | ExpJump exp -> Printf.sprintf "%s" (Astpp.exp_to_string exp)
  | CallNode (_, exp, _) -> Printf.sprintf "_ = %s" (Astpp.exp_to_string exp)
  | AfterCallNode (id, _, _) -> Printf.sprintf "%s = _" (Astpp.identifier_to_string id)
  | Empty -> Printf.sprintf "%s" "Empty"

module NodeContent : DirectedGraph.T with type t = content =
  struct
    type t = content
    let pp = node_content_to_string
  end

include DirectedGraph.Make(NodeContent)

module CFGNode =
  struct
    type t = node
    let compare n1 n2 = compare (get_node_id n1) (get_node_id n2)
  end

let succ_aux node graph all =
  match all, get_node_content node with
  | false, CallNode _ ->
    List.filter
      (fun node ->
        match get_node_content node with
        | AfterCallNode _ -> false
        | _ -> true)
      (lookup node graph.succ)
  | _, _ ->
    lookup node graph.succ

let succ node graph = succ_aux node graph false
let succ_all node graph = succ_aux node graph true

let pred_aux node graph all =
  match all, get_node_content node with
  | false, AfterCallNode _ ->
    List.filter
      (fun node ->
        match get_node_content node with
        | CallNode _ -> false
        | _ -> true)
      (lookup node graph.pred)
  | _, _ ->
    lookup node graph.pred

let pred node graph = pred_aux node graph false
let pred_all node graph = pred_aux node graph true

let call_nodes node graph =
  List.filter
    (fun node ->
      match get_node_content node with
      | CallNode _ -> true
      | _ -> false)
    (pred_all node graph)

let after_call_nodes node graph =
  List.filter
    (fun node ->
      match get_node_content node with
      | AfterCallNode _ -> true
      | _ -> false)
    (succ_all node graph)

let call_node node graph = List.hd (call_nodes node graph)
let after_call_node node graph = List.hd (after_call_nodes node graph)


(**
  * CFG generation:
  *)

let source_pred g node = List.length (pred_all node g) = 0
let sink_pred g node = List.length (succ_all node g) = 0

let get_sources g = select_nodes (source_pred g) g
let get_sinks g = select_nodes (sink_pred g) g
  
let (|>) x f = f x
  
  
let rec generate_cfg_of_single_stm stm interprocedural =
  match stm.Ast.stm with
  | Ast.PointerAssignment _
  | Ast.Output _
  | Ast.LocalDecl _
  | Ast.Return _ -> 
    add (make_node (SimpleStm stm)) empty
    
  | Ast.VarAssignment (id, exp) ->
    (match interprocedural, exp.Ast.exp with
    | true, Ast.FunctionInvocation _ ->
      (* generate a call and after call node *)
      let tmp_node        = make_node Empty in
      let call_node       = make_node (CallNode (id, exp, get_node_id tmp_node + 2)) in
      let after_call_node = make_node (AfterCallNode (id, exp, get_node_id tmp_node + 1)) in
      
      add_many [call_node; after_call_node] empty |>
      connect call_node after_call_node
      
    | _, _ ->
      add (make_node (SimpleStm stm)) empty) 
      
  | Ast.IfThen (exp, stm) ->
    (* graph 'nodes' *)
    let branch_node = make_node (ExpJump exp) in
    let exit_node   = make_node Empty in
    
    let stm_cfg = generate_cfg_of_block_stm stm interprocedural in
    let stm_cfg_sinks = get_sinks stm_cfg in	
    let stm_cfg_sources = get_sources stm_cfg in
      
    add_many [branch_node; exit_node] stm_cfg |>
    connect_many [branch_node] (exit_node::stm_cfg_sources) |>
    connect_many stm_cfg_sinks [exit_node] 
	      
  | Ast.IfThenElse (e, block1, block2) ->
    (* Recursively create graphs for the stms and combine them to one graph obj (will have 2 sources and 2 sinks) *)
    let stm1_cfg = generate_cfg_of_block_stm block1 interprocedural in
    let stm2_cfg = generate_cfg_of_block_stm block2 interprocedural in
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
    
    let stm_cfg = generate_cfg_of_block_stm block interprocedural in
    let stm_cfg_sinks = get_sinks stm_cfg in	
    let stm_cfg_sources = get_sources stm_cfg in
  
    add_many [branch_node; entry_node; exit_node] stm_cfg |>
    connect entry_node branch_node |>
    connect_many [branch_node] (exit_node::stm_cfg_sources) |>
    connect_many stm_cfg_sinks [branch_node] 


and generate_cfg_of_block_stm stms interprocedural =
  List.fold_left
    (fun acc_cfg stm -> 
      let stm_cfg = generate_cfg_of_single_stm stm interprocedural in
      let stm_cfg_sources = get_sources stm_cfg in
      let acc_cfg_sinks = get_sinks acc_cfg in
      let cfg' = combine acc_cfg stm_cfg in
      connect_many acc_cfg_sinks stm_cfg_sources cfg')
    empty stms

let generate_cfg_from_function func interprocedural =
  let cfg = generate_cfg_of_block_stm func.Ast.function_decl.Ast.function_body interprocedural in
  let cfg_sinks = get_sinks cfg in
  let cfg_sources = get_sources cfg in
  let entry_node = make_node (Entry func) in
  let exit_node = make_node (Exit func) in
  let empty_nodes = select_nodes 
    (fun n -> match (get_node_content n) with | Empty -> true | _ -> false)
    cfg in
  add_many [entry_node; exit_node] cfg |>
  connect_many cfg_sinks [exit_node] |>
  connect_many [entry_node] cfg_sources |>
  remove_many empty_nodes


(**
  * Generation of interprocedural CFG
  *)

let connect_intraprocedural_cfgs cfg entry_exit_map =
  let func_name_from_invoke_exp exp =
    match exp.Ast.exp with
    | Ast.FunctionInvocation (id, exps) -> id.Ast.identifier
    | _ -> Error.phase "Control Flow Graph" "Only FunctionInvocation allowed for CallNodes" in
  fold
    (fun node acc_cfg ->
      match get_node_content node with
      | CallNode (_, exp, _) ->
        let (id_func_entry_node, _) = StringMap.find (func_name_from_invoke_exp exp) entry_exit_map in
        connect node id_func_entry_node acc_cfg
      | AfterCallNode (_, exp, _) ->
        let (_, id_func_exit_node) = StringMap.find (func_name_from_invoke_exp exp) entry_exit_map in
        connect id_func_exit_node node acc_cfg
      | _ ->
        acc_cfg)
    cfg cfg

let generate_cfg_from_program prog =
  let (cfg, entry_exit_map) =
    List.fold_left
      (fun (cfg_acc, entry_exit_map_acc) func ->
        let func_name = func.Ast.function_decl.Ast.function_name.Ast.identifier in
        let func_cfg = generate_cfg_from_function func true in
        let entry_node = List.hd (get_sources func_cfg) in
        let exit_node = List.hd (get_sinks func_cfg) in
        
        (combine cfg_acc func_cfg,
         StringMap.add func_name (entry_node, exit_node) entry_exit_map_acc))
      (empty, StringMap.empty) prog.Ast.program_decl in
  connect_intraprocedural_cfgs cfg entry_exit_map