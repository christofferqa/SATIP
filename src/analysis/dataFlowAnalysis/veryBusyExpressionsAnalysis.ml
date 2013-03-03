open Structures
module ExpSet = ExpSetCmpDesc

let exps = AvailableExpressionsAnalysis.exps
let pp_value = AvailableExpressionsAnalysis.pp_value
let all_exps = AvailableExpressionsAnalysis.all_exps

let make_lambda (node: ControlFlowGraph.node) cfg =
  (fun node_map ->
    match (ControlFlowGraph.get_node_content node) with
    | ControlFlowGraph.Exit ->
      (* [[exit]] = {} *)
      ExpSet.empty
    | ControlFlowGraph.ExpJump exp ->
      (* [[v]] = JOIN(v) union exps(E) *)
      ExpSet.union
        (DataFlowAnalysis.join_backwards_must node node_map cfg)
        (exps exp)
    | ControlFlowGraph.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.VarAssignment (id, exp) ->
        (* [[v]] = JOIN(v)!id union exps(E), where ! means "kill" *)
        ExpSet.union
          (ExpSet.filter
            (fun exp -> not (Ast.exp_contains exp (Ast.Identifier id)))
            (DataFlowAnalysis.join_backwards_must node node_map cfg))
          (exps exp)
      | Ast.Output exp ->
        (* [[v]] = JOIN(v) union exps(E) *)
        ExpSet.union
          (DataFlowAnalysis.join_backwards_must node node_map cfg)
          (exps exp)
      | _ ->
        (* [[v]] = JOIN(v) *)
        DataFlowAnalysis.join_backwards_must node node_map cfg)
    | _ -> 
      (* [[v]] = JOIN(v) *)
      DataFlowAnalysis.join_backwards_must node node_map cfg)

let dep (node: ControlFlowGraph.node) cfg =
  let succs = 
    List.fold_left
      (fun acc node_succ -> CFGNodeSet.add node_succ acc)
      CFGNodeSet.empty (CFG.succ node cfg) in
  match (ControlFlowGraph.get_node_content node) with
  | ControlFlowGraph.Exit ->
    (* [[exit]] = {} *)
    CFGNodeSet.empty
  | ControlFlowGraph.ExpJump exp ->
    (* [[v]] = JOIN(v) union exps(E) *)
    CFGNodeSet.add node succs
  | ControlFlowGraph.SimpleStm stm ->
    (match stm.Ast.stm with
    | Ast.VarAssignment (id, exp) ->
      (* [[v]] = JOIN(v)!id union exps(E), where ! means "kill" *)
    CFGNodeSet.add node succs
    | Ast.Output exp ->
      (* [[v]] = JOIN(v) union exps(E) *)
    CFGNodeSet.add node succs
    | _ ->
      (* [[v]] = JOIN(v) *)
      succs)
  | _ -> 
    (* [[v]] = JOIN(v) *)
    succs

let analyze_function f cfg =
  let res = FixedPoint.run_worklist make_lambda dep (all_exps cfg) cfg in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl