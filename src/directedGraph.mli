(**
  * @author Troels Leth Jensen, tleth@cs.au.dk.
  *)

module type T =
  sig
    type t
    val pp : t -> string
  end

module type Graph =
sig
  module NodeOrder : sig
    type t
    val compare : t -> t -> int
  end
  module NodeSet : Set.S
  module NodeMap : Map.S
  
  type t =
    { nodes : NodeSet.t;
      succ  : (node list) NodeMap.t;
      pred  : (node list) NodeMap.t; }
  and c
  and node
    
  val make_node : c -> node
  val get_node_content : node -> c
  val get_node_id : node -> int
  val empty : t
  val add : node -> t -> t
  val add_many : node list -> t -> t
  val remove : node -> t -> t
  val remove_many : node list -> t -> t
  val connect : node -> node -> t -> t
  val connect_many : node list -> node list -> t -> t
  val combine : t -> t -> t
  val select_nodes : (node -> bool) -> t -> node list
  val lookup : node -> (node list) NodeMap.t -> node list
  val succ : node -> t -> node list
  val pred : node -> t -> node list
  val find_cycles : t -> node list list
  val fold : (node -> 'a -> 'a) -> 'a -> t -> 'a 
  val pp : t -> unit
end

module Make (Type : T) : (Graph with type c = Type.t)