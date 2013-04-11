(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *)

let get_function_formals func =
  func.Ast.function_decl.Ast.function_formals

let get_invocation_arguments invoke_exp =
  match invoke_exp.Ast.exp with
  | Ast.FunctionInvocation (_, exps) -> exps
  | Ast.PointerInvocation (_, exps) -> exps
  | _ -> Error.phase "Ast Utilities" "Can not retrieve invocation arguments of a non-invocation expression."