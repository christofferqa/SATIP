(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *
  * Environment AST type. A property function_env is added to each function_decl_desc
  * in the AST.
  *)

module StringMap = Map.Make(SetUtils.String)


(**
  * Functions:
  *)

type decl =
  | FunctionDecl of Ast.function_decl
  | FormalDecl of Ast.function_decl
  | LocalDecl of Ast.stm

type function_decl = { function_decl_pos: Lexing.position; function_decl: function_decl_desc }
and function_decl_desc
  = { function_name    : Ast.identifier;
      function_formals : Ast.identifier list;
      function_body    : Ast.stm list;
      function_env     : decl StringMap.t (* New *) }


(**
  * Programs.
  *)

type program
  = { program_name : string;
      program_decl : function_decl list }

(**
  * Helper functions.
  *)

let return_exp_from_func func =
  let return_stm = List.nth func.function_decl.function_body ((List.length func.function_decl.function_body) - 1) in
  match return_stm.Ast.stm with
  | Ast.Return exp -> exp
  | _ -> Error.phase "AST" "Internal error. Expected a return statement."