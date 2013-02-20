open Printf

(**
  * Input types to the cubic algorithm:
  *)

(* Tokens, variables and constraints *)
type token = Ast.identifier
type variable = Ast.exp
type incl_constraint =
  | TokenInclusion of token list * variable
  | VarInclusion of variable * variable
  | ConditionalInclusion of token * variable * variable * variable

(* Instances and solutions *)
type instance = { tokens: token list; variables: variable list; constraints: incl_constraint list }
type solution = (variable * token list) list


(**
  * Types used by the algorithm to solve an instance:
  *)

module IntegerMap = Map.Make(struct type t = int let compare = compare end)

type entry = { token : token; bit : bool ref; pairs : (variable * variable) list ref }

type int_to_entry_map = entry IntegerMap.t

module NodeType = struct
  type t = (variable * int_to_entry_map)
  let pp = (fun (var, entries) -> Astpp.exp_to_string var)
end

module Graph = DirectedGraph.Make(NodeType)

type var_to_node_map = Graph.node IntegerMap.t

(**
  * Utility functions
  *)

(* Getters: *)

let get_entries (var: variable) (nodes: var_to_node_map): int_to_entry_map =
  let (var, entries) = Graph.get_node_content (IntegerMap.find var.Ast.exp_id nodes) in
  entries

let get_entry (var: variable) (token: token) (nodes: var_to_node_map): entry =
  let entries = get_entries var nodes in
  IntegerMap.find token.Ast.identifier_id entries

(* Setters: *)

let add_pair_to_entry (var: variable) (token: token) (pair: variable * variable) (nodes: var_to_node_map) =
  let entry = get_entry var token nodes in
  entry.pairs := pair :: !(entry.pairs)


(**
  * Below is the actual functions to solve an instance.
  *)

(* Constructs the graph without any edges and a map from variable-id's to graph nodes *)
let init_graph (instance: instance): Graph.t * var_to_node_map =
  (* Add a node for each variable to the graph, and a mapping from the variable's id to its associated node in the graph: *)
  List.fold_left
    (fun ((graph, nodes): Graph.t * var_to_node_map) (var: variable) ->
      let entries =
        List.fold_left
        (fun (entries: int_to_entry_map) (token: token) ->
          IntegerMap.add token.Ast.identifier_id { token = token; bit = ref false; pairs = ref [] } entries)
        IntegerMap.empty instance.tokens in
      let node = Graph.make_node (var, entries) in
      (Graph.add node graph, IntegerMap.add var.Ast.exp_id node nodes))
    (Graph.empty, IntegerMap.empty) instance.variables  

(* Propagates all the token-bits from from_node to to_node. For each token-bit that is set at another node,
   edges according to the pairs related to the token-bit at that particular node is added to the graph. *)
let rec propagate_entry (to_node: Graph.node) (token: token) (graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
  let (_, to_entries) = Graph.get_node_content to_node in
  let to_entry = IntegerMap.find token.Ast.identifier_id to_entries in
  if !(to_entry.bit)
  then
    (* Bit already set, do nothing *)
    (graph, nodes)
  else
    (* Set bit *)
    let () = to_entry.bit := true in
    
    (* Add edges according to the entry pairs *)
    let (graph, nodes) =
      List.fold_left
        (fun ((graph, nodes): Graph.t * var_to_node_map) ((var1, var2): variable * variable) ->
          let var1_node = IntegerMap.find var1.Ast.exp_id nodes in
          let var2_node = IntegerMap.find var2.Ast.exp_id nodes in
          
          (* Add edge *)
          let graph = Graph.connect var1_node var2_node graph in
          
          (* New edge set, so propagate bitvectors *)
          propagate_entries var1_node var2_node graph nodes)
        (graph, nodes) !(to_entry.pairs) in
    
    (* Propagate this bitvector further *)
    let to_node_successors = Graph.succ to_node graph in
    List.fold_left
      (fun ((graph, nodes): Graph.t * var_to_node_map) (succ_node: Graph.node) ->
        propagate_entry succ_node token graph nodes)
      (graph, nodes) to_node_successors

and

propagate_entries (from_node: Graph.node) (to_node: Graph.node) (graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
  let (_, from_entries) = Graph.get_node_content from_node in
  IntegerMap.fold
    (fun (tokenid: int) (entry: entry) ((graph, nodes): Graph.t * var_to_node_map) ->
      if !(entry.bit)
      then
        (* Propagate this bitvector *)
        propagate_entry to_node entry.token graph nodes
      else 
        (* Do nothing *)
        (graph, nodes)
    )
    from_entries (graph, nodes)

let add_edges_from_pairs_list (pairs: (variable * variable) list) (graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
  List.fold_left
    (fun ((graph, nodes): Graph.t * var_to_node_map) ((var1, var2): variable * variable) ->
      let var1_node = IntegerMap.find var1.Ast.exp_id nodes in
      let var2_node = IntegerMap.find var2.Ast.exp_id nodes in
      let graph     = Graph.connect var1_node var2_node graph in
      propagate_entries var1_node var2_node graph nodes)
    (graph, nodes) pairs

let handle_token_inclusion (var: variable) (token: token)(graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
  (* Lookup the entry associated with token *)
  let entry = get_entry var token nodes in
  
  (* Set the bit corresponding to token to 1 *)
  entry.bit := true;
  
  (* Add edges corresponding to the associated list *)
  add_edges_from_pairs_list !(entry.pairs) graph nodes

let handle_token_inclusions (var: variable) (tokens: token list) (graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
  List.fold_left
    (fun ((graph, nodes): Graph.t * var_to_node_map) (token: token) ->
      handle_token_inclusion var token graph nodes)
    (graph, nodes) tokens

let handle_var_inclusion (var1: variable) (var2: variable) (graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
  let entries = get_entries var1 nodes in
  let tokens =
    IntegerMap.fold
      (fun (tokenid: int) (entry: entry) (tokens: token list) ->
        if !(entry.bit)
        then entry.token :: tokens
        else tokens)
      entries [] in
  handle_token_inclusions var2 tokens graph nodes

(* Solves an instance of the cubic algorithm *)
let get_solution_graph (instance: instance): Graph.t * var_to_node_map =
  let (graph, nodes) = init_graph instance in
  
  let (graph, nodes) =
    List.fold_left
      (fun ((graph, nodes): Graph.t * var_to_node_map) (incl_constraint: incl_constraint) ->
        match incl_constraint with
        | TokenInclusion (tokens, var) ->
          (* Look up the node associated with var, and set the corresponding bit to 1.
             If its list of pairs is not empty, then an edge between the nodes corresponding to
             y and z is added for every pair (y,z). *)
          handle_token_inclusions var tokens graph nodes
          
        | VarInclusion (var1, var2) ->
          (* Is handled as TokenInclusion (each token that is in the closure of var1 is added to var2). *)
          handle_var_inclusion var1 var2 graph nodes
          
        | ConditionalInclusion(token, var1, var2, var3) ->
          (* This constraint is handled by first testing if the bit corresponding to token in the
             node corresponding to var1 is 1. If this i so, then an edge between var2 and var3 is added.
             Otherwise the pair (y,z) is added to the list for that bit. *)
          let (_, var1_entries) = Graph.get_node_content (IntegerMap.find var1.Ast.exp_id nodes) in
          let var1_entry = IntegerMap.find token.Ast.identifier_id var1_entries in
          if !(var1_entry.bit)
          then
            (* Bit already set, do nothing *)
            (graph, nodes)
          else
            (* Set bit to 1 *)
            let () = var1_entry.bit := true in
            
            (* Add the edges corresponding to the pairs and propagate bitvectors *)
            add_edges_from_pairs_list !(var1_entry.pairs) graph nodes)
      (graph, nodes) instance.constraints in
  
  let () = Graph.pp graph in
  (graph, nodes)
  
let solve_instance (instance: instance): solution =
  let (graph, nodes) = get_solution_graph instance in
  
  IntegerMap.fold
    (fun (varid: int) (node: Graph.node) (solution: solution) ->
      let (var, entries) = Graph.get_node_content node in
      let var_closure =
        (* Get the closure (list of tokens) of this variable *)
        IntegerMap.fold
          (fun (tokenid: int) (entry: entry) (tokens: token list) ->
            if !(entry.bit)
            then entry.token :: tokens
            else tokens)
          entries [] in
      
      if List.length var_closure > 0
      then (var, var_closure) :: solution
      else solution)
    nodes []