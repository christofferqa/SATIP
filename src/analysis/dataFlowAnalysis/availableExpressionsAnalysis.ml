open Structures
module ExpSet = ExpSetCmpDesc

let rec exps e =
  let filter_input_exps = (fun exp_set -> ExpSet.filter (fun exp -> not (Ast.exp_contains exp Ast.Input)) exp_set) in
  match e.Ast.exp with
  | Ast.Unop (_, e) -> exps e
  | Ast.Binop (e1, _, e2) ->
    filter_input_exps
      (ExpSet.union
        (ExpSet.singleton e)
        (ExpSet.union (exps e1) (exps e2)))
  | Ast.PointerInvocation (e, es) ->
    filter_input_exps
      (List.fold_left
        (fun acc e' -> ExpSet.union (exps e') acc)
        (exps e) es)
  | Ast.FunctionInvocation (_, es) ->
    filter_input_exps
      (List.fold_left
        (fun acc e' -> ExpSet.union (exps e') acc)
        ExpSet.empty es)
  | _ ->
    ExpSet.empty

let make_lambda (node: ControlFlowGraph.node) cfg =
  (fun node_map ->
    match (ControlFlowGraph.get_node_content node) with
    | ControlFlowGraph.Entry ->
      (* [[entry]] = {} *)
      ExpSet.empty
    | ControlFlowGraph.ExpJump exp ->
      (* [[v]] = JOIN(v) union exps(E) *)
      ExpSet.union
        (DataFlowAnalysis.join_forwards_must node node_map cfg)
        (exps exp)
    | ControlFlowGraph.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.VarAssignment (id, exp) ->
        (* [[v]] = (JOIN(v) union exps(E))!id, where ! means "kill" *)
        ExpSet.filter
          (fun exp -> not (Ast.exp_contains exp (Ast.Identifier id)))
          (* Here we calculate JOIN(v) union exps(E): *)
          (ExpSet.union
            (DataFlowAnalysis.join_forwards_must node node_map cfg)
            (exps exp))
      | Ast.Output exp ->
        (* [[v]] = JOIN(v) union exps(E) *)
        ExpSet.union
          (DataFlowAnalysis.join_forwards_must node node_map cfg)
          (exps exp)
      | Ast.LocalDecl ids ->
        (* [[v]] = JOIN(v) *)
        DataFlowAnalysis.join_forwards_must node node_map cfg
      | _ ->
        (* [[v]] = JOIN(v) *)
        DataFlowAnalysis.join_forwards_must node node_map cfg)
    | _ -> 
      (* [[v]] = JOIN(v) *)
      DataFlowAnalysis.join_forwards_must node node_map cfg)

let dep (node: ControlFlowGraph.node) cfg =
  let preds = 
    List.fold_left
      (fun acc node_pred -> CFGNodeSet.add node_pred acc)
      CFGNodeSet.empty (CFG.pred node cfg) in
  match (ControlFlowGraph.get_node_content node) with
  | ControlFlowGraph.Entry ->
    (* [[entry]] = {} *)
    CFGNodeSet.empty
  | ControlFlowGraph.ExpJump exp ->
    (* [[v]] = JOIN(v) union exps(E) *)
    CFGNodeSet.add node preds
  | ControlFlowGraph.SimpleStm stm ->
    (match stm.Ast.stm with
    | Ast.VarAssignment (id, exp) ->
      (* [[v]] = (JOIN(v) union exps(E))!id, where ! means "kill" *)
    CFGNodeSet.add node preds
    | Ast.Output exp ->
      (* [[v]] = JOIN(v) union exps(E) *)
    CFGNodeSet.add node preds
    | _ ->
      (* [[v]] = JOIN(v) *)
      preds)
  | _ -> 
    (* [[v]] = JOIN(v) *)
    preds

let pp_value node_map =
  CFGNodeMap.iter
    (fun node exp_set -> 
      let node_content = ControlFlowGraph.get_node_content node in
      Printf.printf "%s  -> " (ControlFlowGraph.node_content_to_string node_content);
      Structures.pp_exp_set_cmp_desc exp_set;
      print_newline())
    node_map

let all_exps cfg =
  ControlFlowGraph.fold
    (fun node acc ->
      let node_content = ControlFlowGraph.get_node_content node in
      match node_content with
      | ControlFlowGraph.ExpJump exp -> ExpSet.union (exps exp) acc
      | ControlFlowGraph.SimpleStm stm ->
        (match stm.Ast.stm with
        | Ast.LocalDecl ids -> acc
        | Ast.Output exp -> ExpSet.union (exps exp) acc
        | Ast.Return exp -> ExpSet.union (exps exp) acc
        | Ast.While (exp, stms) -> ExpSet.union (exps exp) acc
        | Ast.IfThen (exp, stms) -> ExpSet.union (exps exp) acc
        | Ast.IfThenElse (exp, stms1, stms2) -> ExpSet.union (exps exp) acc
        | Ast.PointerAssignment (exp1, exp2) -> ExpSet.union (exps exp1) (ExpSet.union (exps exp2) acc)
        | Ast.VarAssignment (identifier, exp) -> ExpSet.union (exps exp) acc)
      | _ -> acc)
    ExpSet.empty cfg

let analyze_function f cfg =
  let res = FixedPoint.run_worklist make_lambda dep (all_exps cfg) cfg in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl