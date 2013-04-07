open ControlFlowGraph

let get_function_formals node =
  match get_node_content node with
  | Entry func -> AstUtils.get_function_formals func
  | Exit func -> AstUtils.get_function_formals func
  | _ -> Error.phase "Control Flow Graph Utils" "Can not retrieve function formals of a non-entry/exit point."

let get_call_node_arguments node =
  match get_node_content node with
  | ControlFlowGraph.CallNode (_, exp, _) ->
    (match exp.Ast.exp with
    | Ast.FunctionInvocation (_, exps) -> exps
    | _ -> Error.phase "Control Flow Graph Utils" "Call node must be a function invocation.")
  | _ -> Error.phase "Control Flow Graph Utils" "Can not retrieve function arguments of a non-call node."