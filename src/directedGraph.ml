(**
  *  Module wrapping the type of node content
  *)

module type T = 
  sig
    type t
    val pp : t -> string
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
  val get_node_content : node -> c

  val empty : t
  val add : node -> t -> t
  val add_many : node list -> t -> t
  val remove : node -> t -> t
  val remove_many : node list -> t -> t
  val connect : node -> node -> t -> t
  val connect_many : node list -> node list -> t -> t
  val combine : t -> t -> t

  val select_nodes : (node -> bool) -> t -> node list

  val succ : node -> t -> node list
  val pred : node -> t -> node list
    
  val find_cycles : t -> node list list

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
  let make_id =
    let count = ref 0 in
    fun () -> let id  = !count in
	      begin 
		count := !count + 1;
		id
	      end

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
  let make_node content = 
    let id = make_id () in (id, content)

  let get_node_content (_, content) = content 

  (* Graph properties *)
  let size graph = NodeSet.cardinal graph.nodes
  let mem node graph = NodeSet.mem node graph.nodes
  let select_nodes pred graph = 
    NodeSet.fold (fun n l -> if (pred n) then n::l else l) graph.nodes []

  (* Graph traversing *)
  let succ node graph = lookup node graph.succ
  let pred node graph = lookup node graph.pred
    
  (* Graph manipulation *)
  let add node graph = 
    { graph with nodes = (NodeSet.add node graph.nodes) }

  let add_many nodes graph =
    List.fold_left (fun g n -> add n g) graph nodes

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

  let remove_many nodes graph = 
    List.fold_left (fun g n -> remove n g) graph nodes
      
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

  let connect_many a_list b_list graph =
    List.fold_left 
      (fun graph' a -> 
	List.fold_left
	  (fun graph'' b -> connect a b graph'')
	  graph'
	  b_list)
      graph 
      a_list
      
  let combine g1 g2 = 
    let nodes' = NodeSet.union g1.nodes g2.nodes in 
    let succ' = 
      NodeSet.fold 
	(fun k map -> 
	  let v = (lookup k g1.succ)@(lookup k g2.succ) in
	  NodeMap.add k v map)
	nodes' NodeMap.empty in
    let pred' = 
      NodeSet.fold 
	(fun k map -> 
	  let v = (lookup k g1.pred)@(lookup k g2.pred) in
	  NodeMap.add k v map)
	nodes' NodeMap.empty in
    { nodes = nodes';
      succ  = succ';
      pred  = pred' }
      

  (* Finds all cycles using Tarjan's algorithm O(V + E) *)
  let find_cycles graph =
    let index = ref 0 in
    let node_indexs = ref NodeMap.empty in
    let node_low_links = ref NodeMap.empty in
    let stack = ref [] in
    let sscs = ref [] in
    let rec remove_until l pred =
      match l with
      | [] -> [], []
      | e::l' ->
	if pred e
	then [e],l'
	else
	  let a,b = remove_until l' pred in
	  (e::a),b in
   
    let rec strong_connect v =
      let _ = node_indexs := NodeMap.add v !index !node_indexs in
      let _ = node_low_links := NodeMap.add v !index !node_low_links in
      let _ = index := !index + 1 in
      let _ = stack := v::(!stack) in
      let _ = List.iter 
	        (fun w -> 
		  if not (NodeMap.mem w !node_indexs)
		  then 
		    let _ = strong_connect w in
		    node_low_links := NodeMap.add v (min (NodeMap.find v !node_low_links)
						         (NodeMap.find w !node_low_links)) !node_low_links
		  else
		    node_low_links := NodeMap.add v (min (NodeMap.find v !node_low_links)
			                    		 (NodeMap.find w !node_indexs)) !node_low_links)
		(succ v graph) in
      if (NodeMap.find v !node_low_links) <> (NodeMap.find v !node_indexs)
      then ()
      else 
	let ssc,s' = remove_until !stack ((==) v) in
	let _ = stack := s' in
	sscs := ssc::(!sscs)

    in
    let _ = 
      NodeSet.iter 
	(fun v -> 
	  if NodeMap.mem v !node_indexs
	  then () 
	  else strong_connect v) 
	graph.nodes in
    !sscs

  let pp graph =
    let indent = "   " in
    begin
      Printf.printf "\ndigraph G {\n";
      NodeSet.iter (fun (id,content) -> Printf.printf "%snode%d [label=\"%s\"]\n" indent id (Type.pp content)) graph.nodes;
      NodeMap.iter (fun (id_a, _) v -> List.iter (fun (id_b, _) -> Printf.printf "%snode%d -> node%d;\n" indent id_a id_b) v) graph.succ;
      Printf.printf "}\n";
    end
end

