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

type entry = { bit : bool ref; pairs : (variable * variable) list ref }

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
  let (_, (var, entries)) = IntegerMap.find var.Ast.exp_id nodes in
  entries

let get_entry (var: variable) (token: token) (nodes: var_to_node_map): entry =
  let entries = get_entries var nodes in
  IntegerMap.find token.Ast.identifier_id entries

(* Setters: *)

let add_pair_to_entry (var: variable) (token: token) (pair: variable * variable) (nodes: var_to_node_map) =
  let entry = get_entry var token nodes in
  entry.pairs := pair :: !entry.pairs


(**
  * Below is the actual functions to solve an instance.
  *)

(* Constructs the graph without any edges and a map from variable-id's to graph nodes *)
let init_graph (instance: instance): Graph.t * var_to_node_map =
  (* Add a node for each variable to the graph, and a mapping from the variable's id to its associated node in the graph: *)
  List.fold_left (fun ((graph, nodes): Graph.t * var_to_node_map) (var: variable) ->
    let entries =
      List.fold_left (fun (entries: int_to_entry_map) (token: token) ->
        IntegerMap.add token.Ast.identifier_id { bit = ref false; pairs = ref [] } entries
      ) IntegerMap.empty instance.tokens
      in
    let node = Graph.make_node (var, entries) in
    (Graph.add node graph, IntegerMap.add var.Ast.exp_id node nodes)
  ) (Graph.empty, IntegerMap.empty) instance.variables  

let handle_token_inclusion (graph: Graph.t) (nodes: var_to_node_map) (token: token) (var: variable): Graph.t * var_to_node_map =
  (* Lookup the entry associated with token *)
  let entry = get_entry var token nodes in
  
  (* Set the bit corresponding to token to 1 *)
  entry := true;
  
  (* Add edges corresponding to the associated list *)
  let pairs = !entry.pairs in
  List.fold_left (fun ((graph, nodes): Graph.t * var_to_node_map) ((var1, var2): variable * variable) ->
    let var1_node = IntegerMap.find var1.Ast.exp_id nodes in
    let var2_node = IntegerMap.find var2.Ast.exp_id nodes in
    let graph = Graph.connect var1_node var2_node graph in
    (graph, nodes)
  ) (graph, nodes) pairs

(* Solves an instance of the cubic algorithm *)
let solve_instance (instance: instance): solution =
  let (graph, nodes) = init_graph instance in
  
  let (graph, nodes) =
    List.fold_left (fun ((graph, nodes): Graph.t * var_to_node_map) (incl_constraint: incl_constraint) ->
      match incl_constraint with
      | TokenInclusion (tokens, var) ->
        (* Look up the node associated with var, and set the corresponding bit to 1.
           If its list of pairs is not empty, then an edge between the nodes corresponding to
           y and z is added for every pair (y,z). *)
        List.fold_left (fun ((graph, nodes): Graph.t * var_to_node_map) (token: token) ->
          handle_token_inclusion graph nodes token var
        ) (graph, nodes) tokens
        
      | VarInclusion (var1, var2) ->
        (* Is handled as TokenInclusion (each token that is in the closure of var1 is added to var2). *)
        (graph, nodes)
        
      | ConditionalInclusion(token, var1, var2, var3) ->
        (* This constraint is handled by first testing if the bit corresponding to token in the
           node corresponding to var1 is 1. If this i so, then an edge between var2 and var3 is added.

           Otherwise the pair (y,z) is added to the list for that bit. *)
        (graph, nodes)
        
    ) (graph, nodes) instance.constraints
    in
  
  let () = Graph.pp graph in
  
  []
