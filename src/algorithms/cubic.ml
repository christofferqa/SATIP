open Printf

(**
  * Tokens, Variables and Constraints:
  *)

type token = Ast.identifier
type variable = Ast.exp
type incl_constraint =
  | TokenInclusion of token list * variable
  | VarInclusion of variable * variable
  | ConditionalInclusion of token * variable * variable * variable


(**
  * Finally instances and solutions:
  *)

type instance = { tokens: token list; variables: variable list; constraints: incl_constraint list }
type solution = (variable * token list) list


(**
  * Types necessary for constructing the graph:
  *)

module IntegerMap = Map.Make(struct type t = int let compare = compare end)

type bitvector_assoc = ((variable * variable) list) IntegerMap.t
type bitvector = (bool list) * bitvector_assoc


module NodeType = struct
  type t = (variable * bitvector)
  let pp =  fun (var, vector) -> Astpp.exp_to_string var
end

module Graph = DirectedGraph.Make(NodeType)

type var_to_node_map = Graph.node IntegerMap.t

(**
  * Solution:
  *)

(*
let assign_ids_to_variables (variables: variable list): variable_with_id list =
  let rec visit = (fun (remaining: variable list) (acc: variable_with_id list) (next_id: int) ->
    match remaining with
    | [] -> acc
    | var :: remaining' -> (var, next_id) :: visit remaining' acc (next_id + 1)
  ) in
  visit variables [] 1
*)

let solve_instance (instance: instance): solution =
  (* First we construct the actual graph without any edges. *)
  
  (* In order to make it easy to find nodes, we give each variable a unique id: *)
  (* let variables = assign_ids_to_variables instance.variables in *)
  
  (* Each graph node will be associated with a bitvector. We initialize this bitvector to all zeros (false): *)
  let initial_bitvector = (List.fold_left (fun (acc: bool list) (token: token) -> false :: acc) [] instance.tokens, IntegerMap.empty) in
  
  (* Add a node for each variable to the graph, and a mapping from the variable's id to its associated node in the graph: *)
  let (graph, map) =
    List.fold_left (fun ((graph, map): Graph.t * var_to_node_map) (var: variable) ->
      let node = Graph.make_node (var, initial_bitvector) in
      (Graph.add node graph, IntegerMap.add var.Ast.exp_id node map)
    ) (Graph.empty, IntegerMap.empty) instance.variables
    in
  
  let (graph, map) =
    List.fold_left (fun ((graph, map): Graph.t * var_to_node_map) (incl_constraint: incl_constraint) ->
      match incl_constraint with
      | TokenInclusion (tokens, var) ->
        (* Look up the node associated with var, and set the corresponding bit to 1.
           If its list of pairs is not empty, then an edge between the nodes corresponding to
           y and z is added for every pair (y,z). *)
        (graph, map)
      | VarInclusion (var1, var2) ->
        (* Is handled as TokenInclusion (each token that is in the closure of var1 is added to var2). *)
        (graph, map)
      | ConditionalInclusion(token, var1, var2, var3) ->
        (* This constraint is handled by first testing if the bit corresponding to token in the
           node corresponding to var1 is 1. If this i so, then an edge between var2 and var3 is added.

           Otherwise the pair (y,z) is added to the list for that bit. *)
        (graph, map)
    ) (graph, map) instance.constraints
    in
  
  let () = Graph.pp graph in
  
  []
