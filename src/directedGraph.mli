module type T =
  sig
    type t
    val pp : int * t -> string
  end

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

module Make (Type : T) : (Graph with type c = Type.t)
