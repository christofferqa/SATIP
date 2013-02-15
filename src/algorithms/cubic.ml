type token = Ast.identifier
type variable = Ast.exp

type incl_constraint =
  | TokenInclusion of token list * variable
  | VarInclusion of variable * variable
  | ConditionalInclusion of token * variable * variable * variable

let solve_constraints (tokens: token list) (variables: variable list) (constraints: incl_constraint list) : (variable * token list) list =
  []