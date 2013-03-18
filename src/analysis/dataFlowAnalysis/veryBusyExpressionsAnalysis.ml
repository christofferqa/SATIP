open Structures
module DFA = DataFlowAnalysis
module CFG = ControlFlowGraph
module ExpSet = ExpSetCmpDesc

let exps = AvailableExpressionsAnalysis.exps
let pp_value = AvailableExpressionsAnalysis.pp_value
let all_exps = AvailableExpressionsAnalysis.all_exps

let make_lambda node cfg =
  (fun node_map ->
    match CFG.get_node_content node with
    | CFG.Exit ->
      (* [[exit]] = {} *)
      ExpSet.empty
    | CFG.ExpJump exp ->
      (* [[v]] = JOIN(v) union exps(E) *)
      ExpSet.union
        (DFA.join_backwards_must node node_map cfg)
        (exps exp)
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.VarAssignment (id, exp) ->
        (* [[v]] = JOIN(v)!id union exps(E), where ! means "kill" *)
        ExpSet.union
          (ExpSet.filter
            (fun exp -> not (Ast.exp_contains exp (Ast.Identifier id)))
            (DFA.join_backwards_must node node_map cfg))
          (exps exp)
      | Ast.Output exp ->
        (* [[v]] = JOIN(v) union exps(E) *)
        ExpSet.union
          (DFA.join_backwards_must node node_map cfg)
          (exps exp)
      | _ ->
        (* [[v]] = JOIN(v) *)
        DFA.join_backwards_must node node_map cfg)
    | _ -> 
      (* [[v]] = JOIN(v) *)
      DFA.join_backwards_must node node_map cfg)

let analyze_function func cfg =
  let res = FixedPoint.run_worklist make_lambda (DFA.dep DFA.Backwards) (all_exps cfg) cfg in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun func -> analyze_function func cfg) prog.Ast.program_decl