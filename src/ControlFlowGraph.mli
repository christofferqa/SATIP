type content =
| SimpleStm of Ast.stm
| ExpJump of Ast.exp
| Empty
| Entry
| Exit

type t
type c = content
type node

val get_node_content: node -> c
val get_node_id: node -> int
val succ : node -> t -> node list
val pred : node -> t -> node list
val generate_cfg_from_function : Ast.function_decl -> t
val pp : t -> unit
val fold : (node -> 'a -> 'a) -> 'a -> t -> 'a 
