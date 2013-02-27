open Structures
module NodeMap = CFGNodeMap
module CFG = ControlFlowGraph

let make_lambda (node: CFG.node) cfg =
  (fun nodes ->
    let add v = NodeMap.add node v nodes in
    match (CFG.get_node_content node) with
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.VarAssignment (id, e) ->
        (* [[v]] = JOIN(v)!id union {v}, where ! means "kill" *)
      	add
          (StmSet.union
            (* Here we calculate JOIN(v)!id: *)
            (StmSet.filter
              (fun stm ->
                match stm.Ast.stm with
                | Ast.VarAssignment (id', exp) ->
                  not (id.Ast.identifier_id = id'.Ast.identifier_id)
                | _ ->
                  Error.phase "Reaching Definitions Analysis" "Internal error: Did not expect other than variable assignments in the set.")
              (DataFlowAnalysis.join_forwards_may node nodes cfg))
            (* And here is {v}: *)
    	      (StmSet.singleton stm))
      | _ ->
        (* [[v]] = JOIN(v) *)
        add (DataFlowAnalysis.join_forwards_may node nodes cfg))
    | _ -> 
      (* [[v]] = JOIN(v) *)
      add (DataFlowAnalysis.join_forwards_may node nodes cfg))

let pp_value node_map =
  NodeMap.iter
    (fun node stm_set -> 
      let node_content = CFG.get_node_content node in
      Printf.printf "\n%s\n" (ControlFlowGraph.node_content_to_string node_content);
      StmSet.iter (fun stm -> Printf.printf " -> %s\n" (Astpp.stm_to_string stm)) stm_set)
    node_map

let analyze_function f cfg =
  let lambdas = 
    ControlFlowGraph.fold
      (fun node acc -> make_lambda node cfg :: acc)
      [] cfg in
  let big_F = FixedPoint.make_big_F lambdas in
  let bottom =
    CFG.fold
      (fun n set -> NodeMap.add n StmSet.empty set)
      NodeMap.empty cfg in
  let res = FixedPoint.naive big_F bottom in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl