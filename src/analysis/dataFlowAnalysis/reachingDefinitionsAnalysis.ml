open Structures
module CFG = ControlFlowGraph

let make_lambda (node: CFG.node) cfg =
  (fun node_map ->
    match (CFG.get_node_content node) with
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.VarAssignment (id, e) ->
        (* [[v]] = JOIN(v)!id union {v}, where ! means "kill" *)
        StmSet.union
          (* Here we calculate JOIN(v)!id: *)
          (StmSet.filter
            (fun stm ->
              match stm.Ast.stm with
              | Ast.VarAssignment (id', exp) ->
                not (id.Ast.identifier_id = id'.Ast.identifier_id)
              | _ ->
                Error.phase "Reaching Definitions Analysis" "Internal error: Did not expect other than variable assignments in the set.")
            (DataFlowAnalysis.join_forwards_may node node_map cfg))
          (* And here is {v}: *)
  	      (StmSet.singleton stm)
      | _ ->
        (* [[v]] = JOIN(v) *)
        DataFlowAnalysis.join_forwards_may node node_map cfg)
    | _ -> 
      (* [[v]] = JOIN(v) *)
      DataFlowAnalysis.join_forwards_may node node_map cfg)

let dep (node: CFG.node) cfg =
  let preds = 
    List.fold_left
      (fun acc node_pred -> CFGNodeSet.add node_pred acc)
      CFGNodeSet.empty (CFG.pred node cfg) in
  match (CFG.get_node_content node) with
  | CFG.SimpleStm stm ->
    (match stm.Ast.stm with
    | Ast.VarAssignment (id, e) ->
      (* [[v]] = JOIN(v)!id union {v}, where ! means "kill" *)
    	CFGNodeSet.add node preds
    | _ ->
      (* [[v]] = JOIN(v) *)
      preds)
  | _ -> 
    (* [[v]] = JOIN(v) *)
    preds

let pp_value node_map =
  CFGNodeMap.iter
    (fun node stm_set -> 
      let node_content = CFG.get_node_content node in
      Printf.printf "%s  -> " (ControlFlowGraph.node_content_to_string node_content);
      Structures.pp_stm_set stm_set;
      print_newline())
    node_map

let analyze_function f cfg =
  let res = FixedPoint.run_worklist make_lambda dep StmSet.empty cfg pp_value in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl