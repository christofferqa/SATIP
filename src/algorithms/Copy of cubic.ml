open Printf

module type InstanceType =
  sig
    type token
    val token_compare : token -> token -> int
    val token_to_string : token -> string
    
    type variable
    val variable_compare : variable -> variable -> int
    val variable_to_string : variable -> string
  end

module Make(Instance: InstanceType) = struct
  (**
    * Input types to the cubic algorithm:
    *)
  
  (* Tokens, variables and constraints *)
  type token = Token.token
  type variable = Token.variable
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
  
  (* module VariableMap = Map.Make(struct type t = int let compare = compare end) *)
  module TokenMap = Map.Make(struct type t = token let compare = Token.token_compare end)
  module VariableMap = Map.Make(struct type t = variable let compare = Token.variable_compare end)
  
  type entry = { token : token; bit : bool ref; pairs : (variable * variable) list ref }
  
  type token_to_entry_map = entry TokenMap.t
  
  module NodeType = struct
    type t = (variable * token_to_entry_map)
    let pp = (fun (var, entries) -> Astpp.exp_to_string var)
  end
  
  module Graph = DirectedGraph.Make(NodeType)
  
  type var_to_node_map = Graph.node VariableMap.t
  
  (**
    * Utility functions
    *)
  
  (* Getters: *)
  
  let get_entries (var: variable) (nodes: var_to_node_map): token_to_entry_map =
    let (var, entries) = Graph.get_node_content (VariableMap.find var.Ast.exp_id nodes) in
    entries
  
  let get_entry (var: variable) (token: token) (nodes: var_to_node_map): entry =
    let entries = get_entries var nodes in
    TokenMap.find token entries
  
  
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
            (fun (entries: token_to_entry_map) (token: token) ->
              TokenMap.add token { token = token; bit = ref false; pairs = ref [] } entries)
            TokenMap.empty instance.tokens in
        let node = Graph.make_node (var, entries) in
        (Graph.add node graph, VariableMap.add var.Ast.exp_id node nodes))
      (Graph.empty, VariableMap.empty) instance.variables  
  
  (* Propagates all the token-bits from from_node to to_node. For each token-bit that is set at another node,
     edges according to the pairs related to the token-bit at that particular node is added to the graph. *)
  let rec propagate_entry (to_node: Graph.node) (token: token) (graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
    let (_, to_entries) = Graph.get_node_content to_node in
    let to_entry = TokenMap.find token to_entries in
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
            let var1_node = VariableMap.find var1.Ast.exp_id nodes in
            let var2_node = VariableMap.find var2.Ast.exp_id nodes in
            
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
    TokenMap.fold
      (fun (token: token) (entry: entry) ((graph, nodes): Graph.t * var_to_node_map) ->
        if !(entry.bit)
        then
          (* Propagate this bitvector *)
          propagate_entry to_node token graph nodes
        else
          (* Do nothing *)
          (graph, nodes)
      )
      from_entries (graph, nodes)
  
  let add_edges_from_pairs_list (pairs: (variable * variable) list) (graph: Graph.t) (nodes: var_to_node_map): Graph.t * var_to_node_map =
    List.fold_left
      (fun ((graph, nodes): Graph.t * var_to_node_map) ((var1, var2): variable * variable) ->
        let var1_node = VariableMap.find var1.Ast.exp_id nodes in
        let var2_node = VariableMap.find var2.Ast.exp_id nodes in
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
      TokenMap.fold
        (fun (token: token) (entry: entry) (tokens: token list) ->
          if !(entry.bit)
          then token :: tokens
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
            let (_, var1_entries) = Graph.get_node_content (VariableMap.find var1.Ast.exp_id nodes) in
            let var1_entry = TokenMap.find token var1_entries in
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
    
    VariableMap.fold
      (fun (varid: int) (node: Graph.node) (solution: solution) ->
        let (var, entries) = Graph.get_node_content node in
        let var_closure =
          (* Get the closure (list of tokens) of this variable *)
          TokenMap.fold
            (fun (token: token) (entry: entry) (tokens: token list) ->
              if !(entry.bit)
              then entry.token :: tokens
              else tokens)
            entries [] in
        
        if List.length var_closure > 0
        then (var, var_closure) :: solution
        else solution)
      nodes []
      
      
  (**
    * Pretty printing
    *)
  
  let rec tokens_to_string (tokens: token list): string =
    match tokens with
    | [] -> ""
    | token :: [] -> (Token.to_string token)
    | token :: tokens' -> (Token.to_string token) ^ ", " ^ (tokens_to_string tokens')
  
  let constraint_to_string (incl_constraint: incl_constraint): string =
    match incl_constraint with
    | VarInclusion (var1, var2) ->
      "[[" ^ (Astpp.exp_to_string var1) ^ "]] subset [[" ^ (Astpp.exp_to_string var2) ^ "]]"
    | TokenInclusion (tokens, var) ->
      "{" ^ (tokens_to_string tokens) ^ "} subset [[" ^ (Astpp.exp_to_string var) ^ "]]"
    | ConditionalInclusion (token, var1, var2, var3) ->
      (Token.to_string token) ^ " in [[" ^ (Astpp.exp_to_string var1) ^ "]] => [[" ^ (Astpp.exp_to_string var2) ^ "]] subset [[" ^(Astpp.exp_to_string var3) ^ "]]"
  
  let pp_constraint (incl_constraint: incl_constraint) =
    printf "%s" (constraint_to_string incl_constraint)
  
  let pp_instance (instance: instance) =
    printf "Tokens: %s" (tokens_to_string instance.tokens); print_newline();
    printf "Variables: %s" (Astpp.exps_to_string instance.variables); print_newline();
    print_endline "Constraints:";
    List.iter (fun (incl_constraint: incl_constraint) ->
      printf "  "; pp_constraint incl_constraint; print_newline()
    ) instance.constraints
  
  let pp_solution (solution: solution) =
    List.iter
      (fun ((var, tokens): (variable * token list)) ->
        printf "  [[";
        Astpp.pp_exp var;
        printf "]] = {";
        printf "%s" (tokens_to_string tokens);
        printf "}";
        print_newline())
      solution
end