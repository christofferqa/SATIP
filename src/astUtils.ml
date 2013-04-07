open Ast

let get_function_formals func = func.function_decl.function_formals

let get_invocation_arguments invoke_exp =
  match invoke_exp.exp with
  | Ast.FunctionInvocation (_, exps) -> exps
  | Ast.PointerInvocation (_, exps) -> exps
  | _ -> Error.phase "Ast Utils" "Can not retrieve invocation arguments of a non-invocation expression."