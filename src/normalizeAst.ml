(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *
  * Normalized AST type. A property function_return is added to each function_decl_desc
  * in the AST.
  *)

module EAst = EnvironmentAst
module StringMap = Map.Make(SetUtils.String)


(**
  * Functions:
  *)

type function_decl = { function_decl_pos: Lexing.position; function_decl: function_decl_desc }
and function_decl_desc
  = { function_name    : Ast.identifier;
      function_formals : Ast.identifier list;
      function_body    : Ast.stm list;
      function_env     : EAst.decl StringMap.t;
      function_return  : Ast.identifier (* New *) }


(**
  * Programs.
  *)

type program
  = { program_name : string;
      program_decl : function_decl list }