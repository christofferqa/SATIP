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

(*
type int_to_bool_map = bool IntegerMap.t
type int_to_vars_map = ((variable * variable) list ref) IntegerMap.t
*)

module NodeType = struct
  type t = (variable * int_to_entry_map)
  let pp = (fun (var, int_to_entry_map) -> Astpp.exp_to_string var)
end

module Graph = DirectedGraph.Make(NodeType)

type var_to_node_map = Graph.node IntegerMap.t


(**
  * Utility functions
  *)

let get_var_bitvector_ref (var: variable): int_to_bool_map ref =
  let var_node = IntegerMap.find var.Ast.exp_id map in
  match var_node with
  | (_, Some (var, bv_ref, assoc_lists_ref)) -> bv_ref
  | (_, None) -> Error.phase "Cubic" "Internal error. Did not expect any empty nodes."

let get_var_bitvector (var: variable): int_to_bool_map =
  !(get_var_bitvector_ref var)

let get_var_assoc_lists_ref (var: variable): int_to_vars_map ref =
  let var_node = IntegerMap.find var.Ast.exp_id map in
  match var_node with
  | (_, Some (var, bv_ref, assoc_lists_ref)) -> assoc_lists_ref
  | (_, None) -> Error.phase "Cubic" "Internal error. Did not expect any empty nodes."

let get_var_assoc_lists (var: variable): int_to_vars_map =
  !(get_var_assoc_lists_ref var)

let get_var_assoc_list_ref (var: variable) (token: token): (variable * variable) list ref =
  let var_assoc_lists_ref = get_var_assoc_lists_ref var in
  let () =
    if not (IntegerMap.mem token.Ast.identifier_id !var_assoc_lists_ref)
    then var_assoc_lists_ref := IntegerMap.add token.Ast.identifier_id [] !var_assoc_lists_ref
    in
  IntegerMap.find token.Ast.identifier_id !var_assoc_lists_ref

let get_var_assoc_list (var: variable) (token: token): (variable * variable) list =
  !(get_var_assoc_list_ref var token)


(**
  * Below is the actual functions to solve an instance.
  *)

(* Constructs the graph without any edges and a map from variable-id's to graph nodes *)
let init_graph (instance: instance): Graph.t * var_to_node_map =
  (* Each graph node will be associated with a bitvector. We initialize this bitvector to all zeros (false): *)
  let initial_bv =
    List.fold_left (fun (bv: int_to_bool_map) (token: token) ->
      IntegerMap.add token.Ast.identifier_id false bv
    ) IntegerMap.empty instance.tokens
    in
    
  (* Add a node for each variable to the graph, and a mapping from the variable's id to its associated node in the graph: *)
  List.fold_left (fun ((graph, map): Graph.t * var_to_node_map) (var: variable) ->
    let node = Graph.make_node (Some (var, ref initial_bv, ref IntegerMap.empty)) in
    (Graph.add node graph, IntegerMap.add var.Ast.exp_id node map)
  ) (Graph.empty, IntegerMap.empty) instance.variables  

let handle_token_inclusion (graph: Graph.t) (map: var_to_node_map) (token: token) (var: variable): Graph.t * var_to_node_map =
  (* Lookup the node associated with var *)
  let var_node = IntegerMap.find var.Ast.exp_id map in
  
  (* Set the bit corresponding to token to 1 *)
  let assoc_list =
    match var_node with
    | (_, Some (var, bv_ref, assoc_lists_ref)) ->
      (* Update the bit vector *)
      bv_ref := IntegerMap.add token.Ast.identifier_id true !bv_ref;
      
      (* Return the list associated with token *)
      if IntegerMap.mem token.Ast.identifier_id !assoc_lists_ref then
        let assoc_list_ref = IntegerMap.find token.Ast.identifier_id !assoc_lists_ref in
        !assoc_list_ref
      else
        []
    | (_, None) -> Error.phase "Cubic" "Internal error. Did not expect any empty nodes."
    in
  
  (* Add edges corresponding to the associated list *)
  List.fold_left (fun ((graph, map): Graph.t * var_to_node_map) ((var1, var2): variable * variable) ->
    let var1_node = IntegerMap.find var1.Ast.exp_id map in
    let var2_node = IntegerMap.find var2.Ast.exp_id map in
    let graph = Graph.connect var1_node var2_node graph in
    (graph, map)
  ) (graph, map) assoc_list

(* Solves an instance of the cubic algorithm *)
let solve_instance (instance: instance): solution =
  let (graph, map) = init_graph instance in
  
  let (graph, map) =
    List.fold_left (fun ((graph, map): Graph.t * var_to_node_map) (incl_constraint: incl_constraint) ->
      match incl_constraint with
      | TokenInclusion (tokens, var) ->
        (* Look up the node associated with var, and set the corresponding bit to 1.
           If its list of pairs is not empty, then an edge between the nodes corresponding to
           y and z is added for every pair (y,z). *)
        List.fold_left (fun ((graph, map): Graph.t * var_to_node_map) (token: token) ->
          handle_token_inclusion graph map token var
        ) (graph, map) tokens
        
      | VarInclusion (var1, var2) ->
        (* Is handled as TokenInclusion (each token that is in the closure of var1 is added to var2). *)
        (graph, map)
        
      | ConditionalInclusion(token, var1, var2, var3) ->
        (* This constraint is handled by first testing if the bit corresponding to token in the
           node corresponding to var1 is 1. If this i so, then an edge between var2 and var3 is added.
           Otherwise the pair (y,z) is added to the list for that bit. *)
        let 
        (graph, map)
        
    ) (graph, map) instance.constraints
    in
  
  let () = Graph.pp graph in
  
  []