(************************************************************************)
(** AST type produced by the parser                                     *)
(************************************************************************)

type identifier = { identifier_pos : Lexing.position; identifier : string } 

type name = { name_pos: Lexing.position; name: string }

(* *************** Types *************** *)

type base_type =
  | Int

(* *************** Operators *************** *)

type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Eq

type unop =
  | Pointer
	| Dereference

(* *************** Expressions *************** *)

type exp = { exp_pos: Lexing.position; exp: exp_desc }
and exp_desc =
  | IntConst of string
  | Var of identifier
  | Binop of exp * binop * exp
  | Unop of unop * exp
	| Input
	| Malloc
  | Null
  | FunctionInvocation of identifier * exp list
  | PointerInvocation of exp * exp list

(* *************** Statements *************** *)

and stm = { stm_pos: Lexing.position; stm: stm_desc }
and stm_desc =
  | VarAssignment of identifier * exp
  | PointerAssignment of exp * exp
	| Output of exp
  | IfThen of exp * stm list
  | IfThenElse of exp * stm list * stm list
  | While of exp * stm list
  | LocalDecl of identifier list
  | Return of exp

(* *************** Functions **************** *)

type function_decl = { function_decl_pos: Lexing.position; function_decl: function_decl_desc }
and function_decl_desc
  = { function_name    : identifier;
      function_formals : identifier list;
			function_body    : stm list }

(* *************** Program **************** *)

type program
  = { program_name : string;
      program_decl : function_decl list }

(************************************************************************)
(** {Helper functions for the AST}                                      *)
(************************************************************************)

