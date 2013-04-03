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
  (fun nodes ->
    match CFG.get_node_content node with
    | CFG.Exit _ ->
      (* [[exit]] = {} *)
      StringSet.empty
    | CFG.ExpJump e -> 
      StringSet.union
        (DFA.join_backwards_may node nodes cfg)
        (vars e)
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.Output e ->
        (* [[output E]] = JOIN(v) union vars(E) *)
        StringSet.union
          (DFA.join_backwards_may node nodes cfg)
          (vars e)
      | Ast.VarAssignment (id, e) ->
        StringSet.union
  	      (vars e)
          (StringSet.remove
            (id.Ast.identifier)
  		      (DFA.join_backwards_may node nodes cfg))
      | Ast.LocalDecl ids ->
        List.fold_left
	        (fun set id -> StringSet.remove id.Ast.identifier set)
	        (DFA.join_backwards_may node nodes cfg) ids
      | _ ->
        DFA.join_backwards_may node nodes cfg)
    | _ -> 
      DFA.join_backwards_may node nodes cfg)

let analyze_function func cfg =
  let fix = FixedPoint.run_worklist make_lambda (DFA.dep DFA.Backwards) StringSet.empty cfg in
  DFA.pp_set_solution fix

let analyze_program prog cfg =
  List.iter (fun func -> analyze_function func cfg) prog.Ast.program_decl