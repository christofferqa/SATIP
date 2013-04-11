(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  * @author Troels Leth Jensen, tleth@cs.au.dk.
  *)

type content =
| Entry of Ast.function_decl
| Exit of Ast.function_decl
| SimpleStm of Ast.stm
| ExpJump of Ast.exp
| CallNode of Ast.identifier * Ast.exp * int
| AfterCallNode of Ast.identifier * Ast.exp * int
| Empty

type t
type c = content
type node

module CFGNode :
  sig
    type t = node
    val compare : t -> t -> int
  end

val get_node_content: node -> c
val node_content_to_string : c -> string
val get_node_id: node -> int
val succ : node -> t -> node list
val succ_all : node -> t -> node list
val pred : node -> t -> node list
val pred_all : node -> t -> node list
val call_node : node -> t -> node
val call_nodes : node -> t -> node list
val after_call_node : node -> t -> node
val after_call_nodes : node -> t -> node list
val generate_cfg_from_function : Ast.function_decl -> bool -> t
val generate_cfg_from_program : Ast.program -> t
val pp : t -> unit
val fold : (node -> 'a -> 'a) -> 'a -> t -> 'a