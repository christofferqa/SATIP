(**
  * Pretty printer for the AST.
	*)

open Printf
open Ast


(**
  * Misc.
	*)

let pp_identifier (id: Ast.identifier) =
	printf "%s" id.Ast.identifier

let rec pp_identifiers (ids: Ast.identifier list) =
	match ids with
	| [] -> ()
	| id :: [] ->
		pp_identifier id
	| id :: ids' ->
		pp_identifier id;
		printf ", ";
		pp_identifiers ids'

let pp_binop (binop: Ast.binop) =
	match binop with
  | Plus -> printf "+"
  | Minus -> printf "-"
  | Times -> printf "*"
  | Divide -> printf "/"
  | Gt -> printf ">"
  | Eq -> printf "=="

let pp_unop (unop: Ast.unop) =
	match unop with
  | Pointer -> printf "&"
	| Dereference -> printf "!"


(**
  * Expressions
	*)

let rec pp_exp (exp: Ast.exp) =
	match exp.exp with
  | IntConst c -> printf "%s" c
  | Var id -> pp_identifier id
  | Binop (exp1, binop, exp2) ->
		pp_exp exp1;
		pp_binop binop;
		pp_exp exp2
  | Unop (unop, exp') ->
		pp_unop unop;
		pp_exp exp'
	| Input -> printf "input"
	| Malloc -> printf "malloc"
  | Null -> printf "null"
  | FunctionInvocation (id, exps) ->
		pp_identifier id;
		printf "(";
		pp_exps exps;
		printf ")"
  | PointerInvocation (exp', exps) ->
		pp_exp exp';
		printf "(";
		pp_exps exps;
		printf ")"

and

pp_exps (exps: Ast.exp list) =
	match exps with
	| [] -> ()
	| exp :: [] ->
		pp_exp exp
	| exp :: exps' ->
		pp_exp exp;
		printf ", ";
		pp_exps exps'


(**
  * Statements
	*)

let rec pp_stm (stm: Ast.stm) (prefix: string) =
	match stm.stm with
  | VarAssignment (id, exp) ->
		printf "%s" prefix;
		pp_identifier id;
		printf " = ";
		pp_exp exp;
		printf ";"
  | PointerAssignment (exp1, exp2) ->
		printf "%s*" prefix;
		pp_exp exp1;
		printf " = ";
		pp_exp exp2;
		printf ";"
	| Output exp ->
		printf "%soutput " prefix;
		pp_exp exp
  | IfThen (exp, stms) ->
		printf "%sif (" prefix;
		pp_exp exp;
		printf ") {";
		print_newline();
		pp_stms stms (prefix ^ "  ");
		print_newline();
		printf "%s}" prefix
  | IfThenElse (exp, stms1, stms2) ->
		printf "%sif (" prefix;
		pp_exp exp;
		printf ") {";
		print_newline();
		pp_stms stms1 (prefix ^ "  ");
		print_newline();
		printf "%s} else {" prefix;
		print_newline();
		pp_stms stms2 (prefix ^ "  ");
		print_newline();
		printf "%s}" prefix
  | While (exp, stms) ->
		printf "%swhile (" prefix;
		pp_exp exp;
		printf ") {";
		print_newline();
		pp_stms stms (prefix ^ "  ");
		print_newline();
		printf "%s}" prefix
  | LocalDecl ids ->
		printf "%svar " prefix;
		pp_identifiers ids;
		printf ";"
  | Return exp ->
		printf "%sreturn " prefix;
		pp_exp exp;
		printf ";"

and

pp_stms (stms: Ast.stm list) (prefix: string) =
	match stms with
	| [] -> ()
	| stm :: [] ->
		pp_stm stm prefix
	| stm :: stms' ->
		pp_stm stm prefix;
		print_newline();
		pp_stms stms' prefix


(**
  * Functions
	*)

let rec pp_function (func: Ast.function_decl_desc) =
	pp_identifier func.function_name;
	printf "(";
	pp_identifiers func.function_formals;
	printf ") {";
	print_newline();
	pp_stms func.function_body "  ";
	print_newline();
	printf "}";
	print_newline()

and

pp_functions (funcs: Ast.function_decl list) =
	match funcs with
	| [] -> ()
	| func :: [] ->
		pp_function func.function_decl
	| func :: funcs' ->
		pp_function func.function_decl;
		print_newline();
		pp_functions funcs'


(**
  * Programs
	*)

let rec pp_program (prog: Ast.program) =
	pp_functions prog.program_decl