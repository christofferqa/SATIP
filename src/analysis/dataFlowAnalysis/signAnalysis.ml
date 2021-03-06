(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *)

open SignLattice
module EAst = EnvironmentAst
module CFG = ControlFlowGraph
module DFA = DataFlowAnalysis.Make(SetUtils.Sign)
module StringDFA = DataFlowAnalysis.Make(SetUtils.String)
module StringMap = Map.Make(SetUtils.String)
module CFGNodeMap = Map.Make(ControlFlowGraph.CFGNode)

(* Relation to notes: node corresponds to v, node_pres to w and node_pred_constraint to [[w]]. *)
let join node node_map cfg bottom =
  List.fold_left
    (fun acc node_pred ->
      let node_pred_constraint = CFGNodeMap.find node_pred node_map in
      StringMap.merge
        (fun var sign1 sign2 ->
          match sign1, sign2 with
          | Some sign1, Some sign2 -> Some (least_upper_bound sign1 sign2)
          | Some sign1, None -> Some sign1
          | None, Some sign2 -> Some sign2
          | _, _ -> None)
        acc node_pred_constraint)
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
    | _ -> 
      (* [[v]] = JOIN(v) *)
      join_v)

let analyze_function func cfg =
  let bottom =
    (* The map where each identifier is mapped to the bottom-element: *)
    StringMap.fold
      (fun id decl acc ->
        match decl with
        | EnvironmentAst.FunctionDecl _ -> acc
        | _ -> StringMap.add id Bottom acc)
      func.EAst.function_decl.EAst.function_env StringMap.empty in
  let fix = FixedPoint.run_worklist (make_lambda bottom) (DFA.dep DFA.Forwards) bottom cfg in
  StringDFA.pp_map_solution fix sign_to_string

let analyze_program prog cfg =
  List.iter (fun func -> analyze_function func cfg) prog.EAst.program_decl