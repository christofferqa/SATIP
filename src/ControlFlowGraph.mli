
type node =
| SimpleStm of Ast.stm
| ExpJump of Ast.exp
| Empty
| Entry
| Exit


type t

val fold : (node -> 'a -> 'a) -> 'a -> t -> 'a 
val generate_cfg_from_function : Ast.function_decl -> t
  
