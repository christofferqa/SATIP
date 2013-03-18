open Structures
module NodeMap = CFGNodeMap
module CFG = ControlFlowGraph
module DFA = DataFlowAnalysis

let rec vars e = 
  match e.Ast.exp with
  | Ast.Identifier id -> IdentifierSet.singleton id.Ast.identifier
  | Ast.Binop (e1, _, e2) -> IdentifierSet.union (vars e1) (vars e2)
  | Ast.Unop (_, e) -> vars e
  | Ast.PointerInvocation (e, es) ->
    List.fold_left
      (fun l e' -> IdentifierSet.union (vars e') l)
      (vars e) es
  | Ast.FunctionInvocation (_, es) ->
    List.fold_left
      (fun l e' -> IdentifierSet.union (vars e') l)
      IdentifierSet.empty es
  | _ ->
    IdentifierSet.empty

let make_lambda node cfg =
  (fun nodes ->
    match CFG.get_node_content node with
    | CFG.Exit ->
      (* [[exit]] = {} *)
      IdentifierSet.empty
    | CFG.ExpJump e -> 
      IdentifierSet.union
        (DFA.join_backwards_may node nodes cfg)
        (vars e)
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.Output e ->
        (* [[output E]] = JOIN(v) union vars(E) *)
        IdentifierSet.union
          (DFA.join_backwards_may node nodes cfg)
          (vars e)
      | Ast.VarAssignment (id, e) ->
        IdentifierSet.union
  	      (vars e)
          (IdentifierSet.remove
            (id.Ast.identifier)
  		      (DFA.join_backwards_may node nodes cfg))
      | Ast.LocalDecl ids ->
        List.fold_left
	        (fun set id -> IdentifierSet.remove id.Ast.identifier set)
	        (DFA.join_backwards_may node nodes cfg) ids
      | _ ->
        DFA.join_backwards_may node nodes cfg)
    | _ -> 
      DFA.join_backwards_may node nodes cfg)

let pp_value node_map : unit =
  NodeMap.iter
    (fun node id_set ->
      let node_content = CFG.get_node_content node in
      Printf.printf "%s  -> " (CFG.node_content_to_string node_content);
      Structures.pp_string_set id_set;
      print_newline())
    node_map

let analyze_function func cfg =
  let res = FixedPoint.run_worklist make_lambda (DFA.dep DFA.Backwards) IdentifierSet.empty cfg in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun func -> analyze_function func cfg) prog.Ast.program_decl