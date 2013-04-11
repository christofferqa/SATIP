(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *
  * Compiler phase to build a map from variables (identifiers) to their declaration (AST nodes).
  * A property function_env is added to each function_decl_desc in the AST.
  *
  * An error is reported if:
  * - An identifier is declared more than once, or
  * - An identifier is used but not declared.
  *)

module EAst = EnvironmentAst
module StringMap = Map.Make(SetUtils.String)


(**
  * Functions to add identifiers to an environment map (each identifier is mapped
  * to its corresponding declaration). Reports an error if the identifier is
  * already in the environment.
  *)

let rec add_identifier id decl env =
  if StringMap.mem id.Ast.identifier env
  then Error.phase "Environment" ("Identifier " ^ id.Ast.identifier ^ " have already been declared.")
  else StringMap.add id.Ast.identifier decl env

and

add_identifiers ids decl env =
  List.fold_left
    (fun acc id -> add_identifier id decl acc)
    env ids


(**
  * Functions to check that all identifiers are declared before they are used.
  *)

let env_check_identifier_is_declared id env =
  if not (StringMap.mem id.Ast.identifier env)
  then Error.phase "Environment" ("Identifier " ^ id.Ast.identifier ^ " used but not declared.")


let rec env_check_exp exp env =
  match exp.Ast.exp with
  | Ast.Identifier id ->
    env_check_identifier_is_declared id env
  | Ast.Binop (exp1, binop, exp2) ->
    let () = env_check_exp exp1 env in
    env_check_exp exp2 env 
  | Ast.Unop (unop, exp) ->
    env_check_exp exp env
  | Ast.FunctionInvocation (id, exps) ->
    let () = env_check_identifier_is_declared id env in
    env_check_exps exps env
  | Ast.PointerInvocation (exp, exps) ->
    env_check_exps (exp :: exps) env
  | _ -> () (* IntConst c, Input, Malloc, Null *)

and

env_check_exps exps env =
  List.iter (fun exp -> env_check_exp exp env) exps


(**
  * Functions to build the actual (Environment) AST.
  *
  * The environment of each function f is:
  * - All global functions,
  * - The formals of f, and
  * - The local declarations inside f.
  *)

let rec env_stm stm env =
  match stm.Ast.stm with
  | Ast.LocalDecl ids ->
    add_identifiers ids (EAst.LocalDecl stm) env
  | Ast.Output exp
  | Ast.Return exp ->
    let () = env_check_exp exp env in
    env
  | Ast.While (exp, stms)
  | Ast.IfThen (exp, stms) ->
    let () = env_check_exp exp env in
    env_stms stms env
  | Ast.IfThenElse (exp, stms1, stms2) ->
    let () = env_check_exp exp env in
    env_stms stms2 (env_stms stms1 env)
  | Ast.PointerAssignment (exp1, exp2) ->
    let () = env_check_exp exp1 env in
    let () = env_check_exp exp2 env in
    env
  | Ast.VarAssignment (identifier, exp) ->
    let () = env_check_identifier_is_declared identifier env in
    let () = env_check_exp exp env in
    env

and

env_stms stms env =
  List.fold_left
    (fun acc stm -> env_stm stm acc)
    env stms


(* env_function builds the new EAst.function_decl, which differs from Ast.function_decl
 * in the function_env property. *)
let env_function func func_env =
  (* Add the formals to the environment of this function: *)
  let func_env =
    add_identifiers
      func.Ast.function_decl.Ast.function_formals
      (EAst.FormalDecl func)
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


let env_functions funcs func_env =
  List.fold_right
    (fun func acc ->
      (* Construct EAst.function_decl and append it to the accumulator: *)
      env_function func func_env :: acc)
    funcs []


let env_program prog =
  (* First compute a map with the function environment: *)
  let func_env =
    List.fold_left
      (fun acc func ->
        add_identifier func.Ast.function_decl.Ast.function_name (EAst.FunctionDecl func) acc)
      StringMap.empty prog.Ast.program_decl in
  
  (* Then construct the EAst.program_decl by letting env_functions
     construct a list of EAst.function_decl's: *)
  { EAst.program_name = prog.Ast.program_name;
    EAst.program_decl = env_functions prog.Ast.program_decl func_env }