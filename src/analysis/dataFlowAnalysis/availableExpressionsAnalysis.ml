open Structures
module NodeMap = CFGNodeMap
module CFG = ControlFlowGraph

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

let make_lambda (node: CFG.node) cfg =
  (fun nodes ->
    let add v = NodeMap.add node v nodes in
    match (CFG.get_node_content node) with
    | CFG.Entry ->
      (* [[entry]] = {} *)
      add ExpSet.empty
    | CFG.ExpJump exp ->
      (* [[v]] = JOIN(v) union exps(E) *)
      add
        (ExpSet.union
          (DataFlowAnalysis.join_forwards_must node nodes cfg)
          (exps exp))
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.VarAssignment (id, exp) ->
        (* [[v]] = (JOIN(v) union exps(E))!id, where ! means "kill" *)
      	add
          (ExpSet.filter
            (fun exp -> not (Ast.exp_contains exp (Ast.Identifier id)))
            (* Here we calculate JOIN(v) union exps(E): *)
            (ExpSet.union
              (DataFlowAnalysis.join_forwards_must node nodes cfg)
              (exps exp)))
      | Ast.Output exp ->
        (* [[v]] = JOIN(v) union exps(E) *)
        add
          (ExpSet.union
            (DataFlowAnalysis.join_forwards_must node nodes cfg)
            (exps exp))
      | _ ->
        (* [[v]] = JOIN(v) *)
        add (DataFlowAnalysis.join_forwards_must node nodes cfg))
    | _ -> 
      (* [[v]] = JOIN(v) *)
      add (DataFlowAnalysis.join_forwards_must node nodes cfg))

let pp_value node_map =
  NodeMap.iter
    (fun node exp_set -> 
      let node_content = CFG.get_node_content node in
      Printf.printf "\n%s\n" (ControlFlowGraph.node_content_to_string node_content);
      ExpSet.iter (fun exp -> Printf.printf " -> %s\n" (Astpp.exp_to_string exp)) exp_set)
    node_map

let all_exps cfg =
  CFG.fold
    (fun node acc ->
      let node_content = CFG.get_node_content node in
      match node_content with
      | CFG.ExpJump exp -> ExpSet.union (exps exp) acc
      | CFG.SimpleStm stm ->
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
  let lambdas = 
    CFG.fold
      (fun node acc -> make_lambda node cfg :: acc)
      [] cfg in
  let big_F = FixedPoint.make_big_F lambdas in
  let single_bottom = all_exps cfg in
  let bottom =
    CFG.fold
      (fun n set -> NodeMap.add n single_bottom set)
      NodeMap.empty cfg in
  let res = FixedPoint.naive big_F bottom in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl