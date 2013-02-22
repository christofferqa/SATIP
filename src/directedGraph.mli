module type T =
  sig
    type t
    val pp : t -> string
  end

module type Graph =
sig
  type t
  type c
  type node

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

  val fold : (c -> 'a -> 'a) -> 'a -> t -> 'a 

  val pp : t -> unit
end

module Make (Type : T) : (Graph with type c = Type.t)
