(**
  * Structures used by the environment phase.
  *)

type decl =
  | FunctionDecl of Ast.function_decl
  | FormalDecl of Ast.function_decl
  | LocalDecl of Ast.stm

module Env = Map.Make (String)
type env = decl Env.t