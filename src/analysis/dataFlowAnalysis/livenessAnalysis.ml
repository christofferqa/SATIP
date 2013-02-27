open Structures
module NodeMap = CFGNodeMap
module CFG = ControlFlowGraph

let rec vars e = 
  match e.Ast.exp with
  | Ast.Identifier id -> IdentifierSet.singleton id.Ast.identifier
  | Ast.Binop (e1,_,e2) -> IdentifierSet.union (vars e1) (vars e2)
  | Ast.Unop (_,e) -> vars e
  | Ast.PointerInvocation (e, es) ->
    List.fold_left
      (fun l e' -> IdentifierSet.union (vars e') l)
      (vars e) es
  | Ast.FunctionInvocation (_, es) ->
    List.fold_left
      (fun l e' -> IdentifierSet.union (vars e') l)
      IdentifierSet.empty es
  | _ -> IdentifierSet.empty

let make_lambda (node: CFG.node) cfg =
  (fun nodes ->
    let add v = NodeMap.add node v nodes in
    match (CFG.get_node_content node) with
    | CFG.Exit ->
      (* [[exit]] = {} *)
      add IdentifierSet.empty
    | CFG.ExpJump e -> 
      add
        (IdentifierSet.union
	        (DataFlowAnalysis.join_backwards_may node nodes cfg)
	        (vars e))
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.Output e ->
        (* [[output E]] = JOIN(v) union vars(E) *)
        add
          (IdentifierSet.union
	          (DataFlowAnalysis.join_backwards_may node nodes cfg)
	          (vars e))
      | Ast.VarAssignment (id, e) ->
      	add
          (IdentifierSet.union
    	      (vars e)
            (IdentifierSet.remove
              (id.Ast.identifier)
    		      (DataFlowAnalysis.join_backwards_may node nodes cfg)))
      | Ast.LocalDecl ids -> 
        add
          (List.fold_left
  	        (fun set id -> IdentifierSet.remove id.Ast.identifier set)
  	        (DataFlowAnalysis.join_backwards_may node nodes cfg) ids)
      | _ ->
        add (DataFlowAnalysis.join_backwards_may node nodes cfg))
    | _ -> 
      add (DataFlowAnalysis.join_backwards_may node nodes cfg))

let pp_value node_map : unit =
  NodeMap.iter
    (fun node id_set ->
      let node_content = CFG.get_node_content node in
      Printf.printf "\n%s\n" (ControlFlowGraph.node_content_to_string node_content);
      IdentifierSet.iter (fun id -> Printf.printf " -> %s\n" id) id_set)
    node_map

let analyze_function f cfg =
  let lambdas = 
    ControlFlowGraph.fold
      (fun node acc -> make_lambda node cfg :: acc)
      [] cfg in
  let big_F = FixedPoint.make_big_F lambdas in
  let bottom =
    CFG.fold
      (fun n set -> NodeMap.add n IdentifierSet.empty set)
      NodeMap.empty cfg in
  let res = FixedPoint.naive big_F bottom in
  pp_value res

let analyze_program prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl