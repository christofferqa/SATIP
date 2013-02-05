(**
  * AST type produced by the parser.
  *)

type identifier = Ast.identifier


(**
  * Operators
	*)

type binop = Ast.binop
type unop = Ast.unop


(**
  * Expressions
	*)

type exp = Ast.exp
and exp_desc = Ast.exp_desc


(**
  * Statements
	*)

type stm = Ast.stm
and stm_desc = Ast.stm_desc


(**
  * Functions
	*)

type function_decl = { function_decl_pos: Lexing.position; function_decl: function_decl_desc }
and function_decl_desc
  = { function_name    : identifier;
      function_formals : identifier list;
			function_body    : stm list;
			function_env     : Structures.env (* New *) }


(**
  * Programs
	*)

type program
  = { program_name : string;
      program_decl : function_decl list }