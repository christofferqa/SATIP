(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *)

module CFG = ControlFlowGraph

let get_function_formals node =
  match CFG.get_node_content node with
  | CFG.Entry func -> AstUtils.get_function_formals func
  | CFG.Exit func -> AstUtils.get_function_formals func
  | _ -> Error.phase "Control Flow Graph Utilities" "Can not retrieve function formals of a non-entry/exit point."

let get_call_node_arguments node =
  match CFG.get_node_content node with
  | CFG.CallNode (_, exp, _) ->
    (match exp.Ast.exp with
    | Ast.FunctionInvocation (_, exps) -> exps
    | _ -> Error.phase "Control Flow Graph Utilities" "Can not retrieve function arguments of a non-function invocation.")
  | _ -> Error.phase "Control Flow Graph Utilities" "Can not retrieve function arguments of a non-call node."