(**
  *  Module wrapping the type of node content
  *)

module type T = 
  sig
    type t
    val pp : int * t -> string
  end


(**
  * Signature for the graph module
  *)
    
module type Graph =
sig
  type t
  type c
  type node = int * c

  val make_node : c -> node

  val empty : t
  val add : node -> t -> t
  val connect : node -> node -> t -> t


  val succ : node -> t -> node list
  val pred : node -> t -> node list
  val pp : t -> unit
end


(**
  * Graph functor 
  *)

module Make (Type : T) : (Graph with type c = Type.t) = struct

(**
  * Type declarations
  *)

  type c = Type.t
  type node = int * c

  module NodeOrder = struct
    type t = node
    let compare (a, _) (b, _) = compare a b
  end
  module NodeMap = Map.Make(NodeOrder)
  module NodeSet = Set.Make(NodeOrder)
    
  type t = 
    { nodes : NodeSet.t;
      succ  : (node list) NodeMap.t;
      pred  : (node list) NodeMap.t; }

  let empty = 
    { nodes = NodeSet.empty;
      succ  = NodeMap.empty;
      pred  = NodeMap.empty; }


(**
  * Helper functions - not in the interface
  *)

  (* map lookup with [] on failure *)
  let lookup k m = try NodeMap.find k m with Not_found -> []

  (* returns a new map: forall k,v pairs in map, where k in 'keys', 'node' NOT in v *)
  let remove_node_from_bindings node map keys =
    List.fold_left
      (fun map' k -> 
	(* filter out 'node' *)
	let v' = List.filter ((!=) node) (lookup k map') in  
	NodeMap.add k v' map')
      map keys 

  (* returns a new map: forall k,v pairs in map, where k in 'keys', 'node' IS in v *)
  let add_node_to_bindings node map keys =
    List.fold_left
      (fun map' k -> 
	let v = lookup k map in
	if List.mem node v
	then map'
	else NodeMap.add k (node::v) map')
      map keys 
      
(**
  *  Interface functions
  *)

  (* Node creation / interfacing *)
  let make_node content = (0, content)    
  let get_content (_, content) = content 

  (* Graph properties *)
  let size graph = NodeSet.cardinal graph.nodes
  let mem node graph = NodeSet.mem node graph.nodes

  (* Graph traversing *)
  let succ node graph = lookup node graph.succ
  let pred node graph = lookup node graph.pred
    
  (* Graph manipulation *)
  let add node graph = 
    { graph with nodes = (NodeSet.add node graph.nodes) }

  let remove node graph = 
    (* list of nodes pointing to 'node' *)
    let pred_nodes = pred node graph in 
    (* list of nodes pointed-to from 'node' *)
    let succ_nodes = succ node graph in 
    let succ'  = remove_node_from_bindings node graph.succ pred_nodes in
    let pred'  = remove_node_from_bindings node graph.pred succ_nodes in
    let nodes' = NodeSet.remove node graph.nodes in
    { nodes = nodes';
      succ  = (NodeMap.remove node succ');
      pred  = (NodeMap.remove node pred') }
      
  let connect a b graph = 
    if (not (mem a graph)) ||
       (not (mem b graph))
    then graph
    else 
      let succ' = add_node_to_bindings b graph.succ [a] in
      let pred' = add_node_to_bindings a graph.pred [b] in
    { nodes = graph.nodes;
      succ  = succ';
      pred  = pred' }

  let pp graph =
    let indent = "   " in
    let () = Printf.printf "digraph G {\n" in
    let () = 
      NodeMap.iter 
	(fun from_node v ->
	  List.iter
	    (fun to_node -> Printf.printf "%s%s -> %s;\n" indent (Type.pp from_node) (Type.pp to_node))
	    v)
	graph.succ in
    let () = Printf.printf "}"
    in 
    ()
end


