open SignLattice
module EAst = EnvironmentAst
module CFG = ControlFlowGraph
module CFGUtils = ControlFlowGraphUtils
module DFA = DataFlowAnalysis.Make(SetUtils.Sign)
module StringDFA = DataFlowAnalysis.Make(SetUtils.String)
module StringMap = Map.Make(SetUtils.String)
module StringMapUtils = MapUtils.Make(SetUtils.String)
module CFGNodeMap = Map.Make(ControlFlowGraph.CFGNode)

(* Relation to notes: node corresponds to v, node_pres to w and node_pred_constraint to [[w]]. *)
let merger =
  (fun var sign1 sign2 ->
    match sign1, sign2 with
    | Some sign1, Some sign2 -> Some (least_upper_bound sign1 sign2)
    | Some sign1, None -> Some sign1
    | None, Some sign2 -> Some sign2
    | _, _ -> Error.phase "Sign Analysis" "Internal error: StringMap.merge should not give None, None.")

let join node node_map cfg bottom =
  List.fold_left
    (fun acc node_pred ->
      let node_pred_constraint = CFGNodeMap.find node_pred node_map in
      StringMap.merge merger acc node_pred_constraint)
    bottom (CFG.pred node cfg)

let rec eval sigma exp =
  match exp.Ast.exp with
  | Ast.Identifier id -> StringMap.find id.Ast.identifier sigma
  | Ast.Input -> QuestionMark
  | Ast.IntConst c -> if c = 0 then Zero else if c = 1 then One else if c > 1 then Plus else (* if c < 0 then *) Minus
  | Ast.Binop (exp1, binop, exp2) -> operator_abstract binop (eval sigma exp1) (eval sigma exp2)
  | _ -> Error.phase "Sign analysis" "Internal error: Expression not handled."

let make_lambda bottom node cfg =
  (fun node_map ->
    let join_v = join node node_map cfg bottom in
    match CFG.get_node_content node with
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.LocalDecl ids ->
        (* [[v]] = JOIN(v)[id_1 -> ?, ..., id_n -> ?] *)
        List.fold_left
          (fun acc id -> StringMap.add (Ast.i2s id) QuestionMark acc)
          join_v ids
      | Ast.VarAssignment (id, e) ->
        (* [[v]] = JOIN(v)[id -> eval(JOIN(V), E)] *)
        StringMap.add (Ast.i2s id) (eval join_v e) join_v
      | _ ->
        (* [[v]] = JOIN(v) *)
        join_v)
    | CFG.Entry func ->
      (* [[v]] = LUB (bottom[b_1 -> eval([[w]], E_1),...]) for w in pred(v) *)
      let bs = AstUtils.get_function_formals func in
      List.fold_left
        (fun acc w ->
          let w_constraint = CFGNodeMap.find w node_map in
          let es =
            match CFG.get_node_content w with
            | ControlFlowGraph.CallNode (_, exp, _) ->
              (match exp.Ast.exp with
              | Ast.FunctionInvocation (_, exps) -> exps
              | _ -> Error.phase "Interprocedural Context Insensitive Sign Analysis" "Call node must be a function invocation!")
            | _ -> Error.phase "Interprocedural Context Insensitive Sign Analysis" "Entry node can only have call node predecessors" in
          
          let new_node_map =
            List.fold_left2
              (fun acc_map b e ->
                StringMap.add b.Ast.identifier (eval w_constraint e) acc_map)
              StringMap.empty bs es in
              
          StringMap.merge merger acc new_node_map)
        bottom (CFG.call_nodes node cfg)
    | CFG.AfterCallNode (id, exp, cn_id) -> 
      (* [[v]] = [[v']][id -> [[w]](result)] *)
      let call_node = CFG.call_node node cfg in
      let call_node_constraint = CFGNodeMap.find call_node node_map in
      let pred_node = List.hd (CFG.pred node cfg) in
      let pred_node_constraint = CFGNodeMap.find pred_node node_map in
      StringMap.add (Ast.i2s id) (StringMap.find "result" pred_node_constraint) call_node_constraint
    | _ -> 
      (* [[v]] = JOIN(v) *)
      join_v)

let analyze_program prog cfg =
  let bottom =
    List.fold_left
      (fun acc func ->
        StringMap.fold
          (fun id decl acc ->
            match decl with
            | EnvironmentStructures.FunctionDecl _ -> acc
            | _ -> StringMap.add id Bottom acc)
          func.EAst.function_decl.EAst.function_env acc)
      StringMap.empty prog.EAst.program_decl in
  let fix = FixedPoint.run_worklist (make_lambda bottom) (DFA.dep DFA.Forwards) bottom cfg in
  StringDFA.pp_map_solution fix sign_to_string