module NodeMap = Structures.CFGNodeMap
module CFG = ControlFlowGraph
module DFA = DataFlowAnalysis.Make(SetUtils.String)
module StringSetUtils = SetUtils.Make(SetUtils.String)
module StringSet = Set.Make(SetUtils.String)

let rec vars e = 
  match e.Ast.exp with
  | Ast.Identifier id -> StringSet.singleton id.Ast.identifier
  | Ast.Binop (e1, _, e2) -> StringSet.union (vars e1) (vars e2)
  | Ast.Unop (_, e) -> vars e
  | Ast.PointerInvocation (e, es) ->
    List.fold_left
      (fun l e' -> StringSet.union (vars e') l)
      (vars e) es
  | Ast.FunctionInvocation (_, es) ->
    List.fold_left
      (fun l e' -> StringSet.union (vars e') l)
      StringSet.empty es
  | _ ->
    StringSet.empty

let make_lambda node cfg =
  (fun node_map ->
    match CFG.get_node_content node with
    | CFG.Entry ->
      (* [[entry]] = {} *)
      StringSet.empty
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.VarAssignment (id, e) ->
        (* [[v]] = JOIN(v) union {id} *)
        StringSet.add id.Ast.identifier (DFA.join_forwards_must node node_map cfg)
      | _ ->
        (* [[v]] = JOIN(v) *)
        DFA.join_forwards_must node node_map cfg)
    | _ ->
      (* [[v]] = JOIN(v) *)
      DFA.join_forwards_must node node_map cfg)

let analyze_function func cfg =
  let fix = FixedPoint.run_worklist make_lambda (DFA.dep DFA.Forwards) StringSet.empty cfg in
  DFA.pp_solution fix

let analyze_program prog cfg =
  List.iter (fun func -> analyze_function func cfg) prog.Ast.program_decl