open ConstantPropagationLattice
open Structures
module CFG = ControlFlowGraph
module EAst = EnvironmentAst

(* Relation to notes: node corresponds to v, node_pres to w and node_pred_constraint to [[w]]. *)
let join node node_map cfg bottom =
  List.fold_left
    (fun acc node_pred ->
      let node_pred_constraint = CFGNodeMap.find node_pred node_map in
      StringMap.merge
        (fun var const1 const2 ->
          match const1, const2 with
          | Some const1, Some const2 -> Some (least_upper_bound const1 const2)
          | Some const1, None -> Some const1
          | None, Some const2 -> Some const2
          | _, _ -> Error.phase "constant propagation analysis" "Internal error: StringMap.merge should not give None, None.")
        acc node_pred_constraint)
    bottom (CFG.pred node cfg)

let rec eval sigma exp =
  match exp.Ast.exp with
  | Ast.Identifier id -> StringMap.find id.Ast.identifier sigma
  | Ast.IntConst c -> IntConst c
  | Ast.Input -> QuestionMark
  | Ast.Binop (exp1, binop, exp2) -> operator_abstract binop (eval sigma exp1) (eval sigma exp2)
  | _ -> Error.phase "constant propagation analysis" "Internal error: Expression not handled."

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
        (* [[v]] = JOIN(v)[id -> eval(JOIN(v), E)] *)
        StringMap.add (Ast.i2s id) (eval join_v e) join_v
      | _ ->
        (* [[v]] = JOIN(v) *)
        join_v)
    | _ -> 
      (* [[v]] = JOIN(v) *)
      join_v)

let dep (node: CFG.node) cfg =
  (* predecessors: *)
  List.fold_left
    (fun acc node_pred -> CFGNodeSet.add node_pred acc)
    CFGNodeSet.empty (CFG.pred node cfg)

let pp_value node_map =
  CFGNodeMap.iter
    (fun node vars_map -> 
      let node_content = CFG.get_node_content node in
      Printf.printf "%s  -> " (ControlFlowGraph.node_content_to_string node_content);
      Structures.pp_string_map vars_map const_to_string;
      print_newline())
    node_map

let analyze_function func cfg =
  let bottom =
    StringMap.fold
      (fun id decl acc ->
        match decl with
        | EnvironmentStructures.FunctionDecl _ -> acc
        | _ -> StringMap.add id Bottom acc)
      func.EAst.function_decl.EAst.function_env StringMap.empty in
  let res = FixedPoint.run_worklist (make_lambda bottom) dep bottom cfg in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.EAst.program_decl