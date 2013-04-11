(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com
  *)

module CFG = ControlFlowGraph
module DFA = DataFlowAnalysis.Make(SetUtils.Stm)
module StmSetUtils = SetUtils.Make(SetUtils.Stm)
module StmSet = Set.Make(SetUtils.Stm)

let make_lambda node cfg =
  (fun node_map ->
    match CFG.get_node_content node with
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
            (DFA.join_forwards_may node node_map cfg))
          (* And here is {v}: *)
  	      (StmSet.singleton stm)
      | _ ->
        (* [[v]] = JOIN(v) *)
        DFA.join_forwards_may node node_map cfg)
    | _ -> 
      (* [[v]] = JOIN(v) *)
      DFA.join_forwards_may node node_map cfg)

let analyze_function f cfg =
  let fix = FixedPoint.run_worklist make_lambda (DFA.dep DFA.Forwards) StmSet.empty cfg in
  DFA.pp_set_solution fix

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl