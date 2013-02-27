
module CFG = ControlFlowGraph
module IdentifierSet = Set.Make(String)
module NodeMap = Map.Make (struct type t = CFG.node let compare n1 n2 = compare (CFG.get_node_id n1) (CFG.get_node_id n2) end)

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

let (|>) x f = f x;;

let make_lambda (node : CFG.node) cfg =
  let join n map =
    List.fold_left
      (fun set n' -> 
	IdentifierSet.union 
	  set 
	  (NodeMap.find n' map))
      (IdentifierSet.empty)
      (CFG.succ n cfg) in
  (fun val_map ->
    let ret v = NodeMap.add node v val_map in
    match (CFG.get_node_content node) with
    | CFG.Exit ->
      ret IdentifierSet.empty
    | CFG.ExpJump e -> 
      ret (IdentifierSet.union
	     (join node val_map)
	     (vars e))
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.Output e -> 
	ret (IdentifierSet.union
	       (join node val_map)
	       (vars e))
      | Ast.VarAssignment (id, e) -> 
	ret ((IdentifierSet.remove
		(id.Ast.identifier)
		(join node val_map)) |>
	    (IdentifierSet.union
	       (vars e)))
      | Ast.LocalDecl ids -> 
	ret (List.fold_left
	       (fun set id -> IdentifierSet.remove id.Ast.identifier set)
	       (join node val_map)
	       ids)
      | _ -> 
	ret (join node val_map))
    | _ -> 
      ret (join node val_map))
      

let compose lambda_list =
  List.fold_left
    (fun composed' lambda  -> (fun v -> lambda v |> composed'))
    (fun i -> i)
    lambda_list


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

let pp_value v : unit =
  NodeMap.iter
    (fun k v -> 
      let nc = CFG.get_node_content k in
      let _ = Printf.printf "\n%s\n" (pp_nc nc) in
      IdentifierSet.iter
	(fun e -> Printf.printf " -> %s\n" e)
	v)
    v

let rec naive lambda value =
  let value' = lambda value in
  if value = value'
  then value
  else naive lambda value'


let liveness program = 
  let f = List.hd (program.Ast.program_decl) in
  let cfg = ControlFlowGraph.generate_cfg_from_function f in
  let _ = CFG.pp cfg in
  let lambdas = 
    ControlFlowGraph.fold
      (fun n l -> make_lambda n cfg::l)
      [] cfg in
  let big_gun = compose lambdas in
  let bottom = 
    CFG.fold
      (fun n set -> NodeMap.add n IdentifierSet.empty set)
      NodeMap.empty
      cfg in
  let _ = Printf.printf ("hej") in
  let res = naive big_gun bottom in
  pp_value res
 
  

