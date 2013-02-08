(**
  * Compiler phase to build a map from names to AST nodes where these are declared.
  
  * A property function_env is added to each function_decl_desc in the (Environment) AST.
  *
  * An error is reported if:
  * - An identifier is declared more than once, or
  * - An identifier is used but not declared.
  *)

open Printf
open Structures

module EAst = EnvironmentAst


(**
  * Functions to add identifiers to an environment map.
  * Reports an error if the identifier is already in the environment.
  *)

let rec add_identifier (id: Ast.identifier) (node: decl) (map: env): env =
  if Env.mem id.Ast.identifier map then
    Error.error
      (Ast.i2p id)
      ("Error in environment: Identifier " ^ id.Ast.identifier ^ " already declared.")
  else
    Env.add (Ast.i2s id) node map

and

add_identifiers (ids: Ast.identifier list) (node: decl) (map: env): env =
  Utils.fold ids map (fun (id: Ast.identifier) (acc: env) -> add_identifier id node acc)


(**
  * Tree traversal to construct the environment below.
  *)

(**
  * Functions to check that all identifiers are declared before they are used.
  *)

let env_check_identifier_is_declared (id: Ast.identifier) (map: env) =
  if not (Env.mem (Ast.i2s id) map) then
    Error.error
      (Ast.i2p id)
      ("Error in environment: Identifier " ^ (Ast.i2s id) ^ " used but not declared.")


let rec env_check_exp (exp: Ast.exp) (map: env) =
  match exp.Ast.exp with
  | Ast.Identifier id ->
    env_check_identifier_is_declared id map
  | Ast.Binop (exp1, binop, exp2) ->
    let () = env_check_exp exp1 map in
    env_check_exp exp2 map 
  | Ast.Unop (unop, exp) ->
    env_check_exp exp map
  | Ast.FunctionInvocation (id, exps) ->
    let () = env_check_identifier_is_declared id map in
    env_check_exps exps map
  | Ast.PointerInvocation (exp, exps) ->
    env_check_exps (exp :: exps) map
  | _ -> () (* IntConst c, Input, Malloc, Null *)

and

env_check_exps (exps: Ast.exp list) (map: env) =
  Utils.iter exps (fun (exp: Ast.exp) -> env_check_exp exp map)


(**
  * Functions to build the actual (Environment) AST.
  *
  * The environment of each function f is:
  * - All global functions,
  * - The formals of f, and
  * - The local declarations inside f.
  *)

let rec env_stm (stm: Ast.stm) (map: env) : env =
  match stm.Ast.stm with
  | Ast.LocalDecl ids ->
    add_identifiers ids (LocalDecl stm) map
  | Ast.Output exp
  | Ast.Return exp ->
    let () = env_check_exp exp map in
    map
  | Ast.While (exp, stms)
  | Ast.IfThen (exp, stms) ->
    let () = env_check_exp exp map in
    env_stms stms map
  | Ast.IfThenElse (exp, stms1, stms2) ->
    let () = env_check_exp exp map in
    env_stms stms2 (env_stms stms1 map)
  | Ast.PointerAssignment (exp1, exp2) ->
    let () = env_check_exp exp1 map in
    let () = env_check_exp exp2 map in
    map
  | Ast.VarAssignment (identifier, exp) ->
    let () = env_check_identifier_is_declared identifier map in
    let () = env_check_exp exp map in
    map

and

env_stms (stms: Ast.stm list) (map: env): env =
  Utils.fold stms map (fun (stm: Ast.stm) (acc: env) -> env_stm stm acc)


(**
  * env_function builds the new EAst.function_decl, which differs from Ast.function_decl
  * in the function_env property.
  *)

let env_function (func: Ast.function_decl) (func_env: env): EAst.function_decl =
  (* Add the formals to the environment of this function: *)
  let func_env =
    add_identifiers
      func.Ast.function_decl.Ast.function_formals
      (FormalDecl func)
      func_env
    in
  
  (* Construct EAst.function_decl_desc (let env_stms add locals to the environment): *)
  let func_decl_desc =
    { EAst.function_name    = func.Ast.function_decl.Ast.function_name;
      EAst.function_formals = func.Ast.function_decl.Ast.function_formals;
      EAst.function_body    = func.Ast.function_decl.Ast.function_body;
      EAst.function_env     = env_stms func.Ast.function_decl.Ast.function_body func_env } in
      
  { EAst.function_decl_pos = func.Ast.function_decl_pos;
    EAst.function_decl = func_decl_desc }


let env_functions (funcs: Ast.function_decl list) (func_env: env): EAst.function_decl list =
  let funcs = Utils.fold funcs [] (fun (func: Ast.function_decl) (acc: EAst.function_decl list) ->
    (* Construct EAst.function_decl and append it to the accumulator: *)
    env_function func func_env :: acc) in
  
  (* Reverse the list to keep the original order of the functions: *)
  List.rev funcs


let env_program (prog: Ast.program): EAst.program =
  (* First compute a map with the function environment: *)
  let func_env = Utils.fold prog.Ast.program_decl Env.empty (fun (func: Ast.function_decl) (acc: env) ->
    add_identifier func.Ast.function_decl.Ast.function_name (FunctionDecl func) acc
  ) in
  
  (* Then construct the EAst.program_decl by letting env_functions
     construct a list of EAst.function_decl's: *)
  { EAst.program_name = prog.Ast.program_name;
    EAst.program_decl = env_functions prog.Ast.program_decl func_env }