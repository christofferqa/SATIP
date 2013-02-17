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

let solve_instance (instance: instance): solution =
  []