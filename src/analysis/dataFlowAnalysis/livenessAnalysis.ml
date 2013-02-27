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

let pp_nc = (fun nc ->
    match nc with
    | CFG.ExpJump e -> Printf.sprintf "%s" (Astpp.exp_to_string e)
    | CFG.Empty -> Printf.sprintf "%s" "Empty"
    | CFG.Entry -> Printf.sprintf "%s" "Entry"
    | CFG.Exit -> Printf.sprintf "%s" "Exit"
    | CFG.SimpleStm stm ->
      match stm.Ast.stm with
      | Ast.VarAssignment (id, e) -> Printf.sprintf "%s = %s;" id.Ast.identifier (Astpp.exp_to_string e)
      | Ast.PointerAssignment (e, e') -> Printf.sprintf "%s = %s;" (Astpp.exp_to_string e) (Astpp.exp_to_string e')
      | Ast.Output e -> Printf.sprintf "output %s;" (Astpp.exp_to_string e)
      | Ast.LocalDecl is -> Printf.sprintf "var %s;" (List.fold_left (fun s i -> i.Ast.identifier ^ ", " ^ s) "" is)
      | Ast.Return e -> Printf.sprintf "return %s;" (Astpp.exp_to_string e)
      | _ -> "Doesnt Occur")

let pp_value node_map : unit =
  NodeMap.iter
    (fun node id_set ->
      let node_content = CFG.get_node_content node in
      Printf.printf "\n%s\n" (pp_nc node_content);
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

let analyze_liveness prog cfg =
  List.iter (fun f -> analyze_function f cfg) prog.Ast.program_decl