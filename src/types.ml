(**
  * ...
  *)

type type_exp =
  | Int
  | Pointer of type_exp
  | Function of type_exp list * type_exp

type type_exp_variable =
  | AlphaVar
  | IntVar
  | IdentifierVar of Ast.identifier
  | ExpressionVar of Ast.exp
  | PointerVar of type_exp_variable
  | FunctionVar of type_exp_variable list * type_exp_variable

type type_constraint =
  | TypeIdentifierConstraint of Ast.identifier * type_exp_variable
  | TypeExpConstraint of Ast.exp * type_exp_variable