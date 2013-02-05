(**
  * Compiler phase to build a map from names to AST nodes where these are declared.
  *)

(**
  * Environment and environment lookup
	* The environment consists of a map from strings (variable names) to their Ast declaration node.
	*)

type decl =
	| FunctionDecl of Ast.function_decl
	| ArgumentDecl of Ast.function_decl
	| LocalDecl of Ast.stm
	
module Env = Map.Make (String)
type env = decl Env.t


(**
  * Construct a type environment from a AST.
	*)

let rec add_identifier (id: Ast.identifier) (node: decl) (map: env): env =
	if Env.mem id.Ast.identifier map then
		Error.error id.Ast.identifier_pos ("Error in environment: Identifier " ^ id.Ast.identifier ^ " already declared.")
	else
		Env.add id.Ast.identifier node map

and

add_identifiers (ids: Ast.identifier list) (node: decl) (map: env): env =
	match ids with
	| [] -> map
	| id :: ids' ->
		let map = add_identifier id node map in
		add_identifiers ids' node map

let rec env_stm : env = Env.empty

and

env_stms : env = Env.empty

let rec env_function (func: Ast.function_decl) (map: env) : env =
	let map = add_identifier func.Ast.function_decl.Ast.function_name (FunctionDecl func) map in
	let map = add_identifiers func.Ast.function_decl.Ast.function_formals (FunctionDecl func) map in
	(* todo: add local decls *)
	map

and

env_functions (funcs: Ast.function_decl list) (map: env) : env =
	match funcs with
	| [] -> map
	| func :: funcs' ->
		let map = env_function func map in
		let map = env_functions funcs' map in
		map

let env_program (prog : Ast.program): env =
  env_functions prog.Ast.program_decl Env.empty