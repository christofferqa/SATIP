open Printf
open Structures

module EAst = EnvironmentAst


(**
  * Function for printing an already constructed environment.
  *)

let pp_env (prog: EAst.program) =
  Utils.iter prog.EAst.program_decl (fun (func: EAst.function_decl) ->
    printf "Environment of function %s: " (Ast.i2s func.EAst.function_decl.EAst.function_name);
    Env.iter (fun (id: string) (node: decl) ->
      printf "%s " id
    ) func.EAst.function_decl.EAst.function_env;
    print_newline();
  )