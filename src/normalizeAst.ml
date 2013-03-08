open Printf
open EnvironmentStructures

module EAst = EnvironmentAst

let generate_unique_identifier env =
  let rec find = (fun str num ->
    let id = Printf.sprintf "%s%d" str num in
    if Env.mem id env
    then find str (num + 1)
    else id) in
  { Ast.identifier_pos = Lexing.dummy_pos; Ast.identifier = find "tmp" 1; Ast.identifier_id = Utils.new_id !(Utils.id) }

let rec normalize_exp exp stms env depth =
  match exp.Ast.exp with
  | Ast.Binop (exp1, binop, exp2) ->
    let (normalized_exp1, stms, env) = normalize_exp exp1 stms env (depth + 1) in
    let (normalized_exp2, stms, env) = normalize_exp exp2 stms env (depth + 1) in
    ({ exp with Ast.exp = Ast.Binop (normalized_exp1, binop, normalized_exp2) }, stms, env)
  | Ast.Unop (unop, exp') -> (exp, stms, env)
  | Ast.FunctionInvocation (id, exps) -> (exp, stms, env)
  | Ast.PointerInvocation (exp', exps) -> (exp, stms, env)
  | Ast.IntConst c -> (exp, stms, env)
  | Ast.Input -> (exp, stms, env)
  | Ast.Malloc ->
    if depth = 0 then
      (exp, stms, env)
    else
      let temp_id = generate_unique_identifier env in
      let exp_id = { Ast.exp_pos = Lexing.dummy_pos; Ast.exp = Ast.Identifier temp_id; Ast.exp_id = Utils.new_id !(Utils.id) } in
      let stm_temp_decl = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id]; Ast.stm_id = Utils.new_id !(Utils.id) } in
      let stm_temp_assign = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id, exp); Ast.stm_id = Utils.new_id !(Utils.id) } in
      let env = Env.add temp_id.Ast.identifier (LocalDecl stm_temp_decl) env in
      (exp_id, stm_temp_decl :: stm_temp_assign :: stms, env)
  | Ast.Null -> (exp, stms, env)
  | Ast.Identifier id -> (exp, stms, env)

let rec normalize_stm stm env stms =
  match stm.Ast.stm with
  | Ast.LocalDecl ids ->
    (stm :: stms, env)
  | Ast.Output exp ->
    let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
    let stm = { stm with Ast.stm = Ast.Output normalized_exp } in
    (new_stms @ stm :: stms, env)  
  | Ast.Return exp ->
    let (normalized_exp, new_stms, env) = normalize_exp exp stms env 0 in
    let stm = { stm with Ast.stm = Ast.Return normalized_exp } in
    (new_stms @ stm :: stms, env)
  | Ast.While (exp, stms') ->
    let (normalized_exp, new_stms, env) = normalize_exp exp stms env 0 in
    let (normalized_stms, env) = normalize_stms stms' env in
    let stm = { stm with Ast.stm = Ast.While (normalized_exp, normalized_stms) } in
    (new_stms @ stm :: stms, env)
  | Ast.IfThen (exp, stms') ->
    let (normalized_exp, new_stms, env) = normalize_exp exp stms env 0 in
    let (normalized_stms, env) = normalize_stms stms' env in
    let stm = { stm with Ast.stm = Ast.IfThen (normalized_exp, normalized_stms) } in
    (new_stms @ stm :: stms, env)
  | Ast.IfThenElse (exp, stms1, stms2) ->
    let (normalized_exp, new_stms, env) = normalize_exp exp stms env 0 in
    let (normalized_stms1, env) = normalize_stms stms1 env in
    let (normalized_stms2, env) = normalize_stms stms2 env in
    let stm = { stm with Ast.stm = Ast.IfThenElse (normalized_exp, normalized_stms1, normalized_stms2) } in
    (new_stms @ stm :: stms, env)
  | Ast.PointerAssignment (exp1, exp2) ->
    (match exp1.Ast.exp, exp2.Ast.exp with
    | Ast.Unop (Ast.Dereference, _), Ast.Identifier _ ->
      (* Already normalized *)
      (stm :: stms, env)
    | Ast.Unop (Ast.Dereference, _), _ ->
      (* Normalize right hand side *)
      let temp_id = generate_unique_identifier env in
      let exp_id = { Ast.exp_pos = Lexing.dummy_pos; Ast.exp = Ast.Identifier temp_id; Ast.exp_id = Utils.new_id !(Utils.id) } in
      let stm_temp_decl = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id]; Ast.stm_id = Utils.new_id !(Utils.id) } in
      let stm_temp_assign = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id, exp2); Ast.stm_id = Utils.new_id !(Utils.id) } in
      let env = Env.add temp_id.Ast.identifier (LocalDecl stm_temp_decl) env in
      
      let stm_ptr_assign = { stm with Ast.stm = Ast.PointerAssignment (exp1, exp_id) } in
      
      (stm_temp_decl :: stm_temp_assign :: stm_ptr_assign :: stms, env)
    | _, Ast.Identifier _ ->
      (* Normalize left hand side *)
      let temp_id = generate_unique_identifier env in
      let exp_id = { Ast.exp_pos = Lexing.dummy_pos; Ast.exp = Ast.Identifier temp_id; Ast.exp_id = Utils.new_id !(Utils.id) } in
      let stm_temp_decl = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id]; Ast.stm_id = Utils.new_id !(Utils.id) } in
      let stm_temp_assign = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id, exp1); Ast.stm_id = Utils.new_id !(Utils.id) } in
      let env = Env.add temp_id.Ast.identifier (LocalDecl stm_temp_decl) env in
      
      let stm_ptr_assign = { stm with Ast.stm = Ast.PointerAssignment (exp_id, exp2) } in
      
      (stm_temp_decl :: stm_temp_assign :: stm_ptr_assign :: stms, env)
    | _, _ ->
      (* Normalize both sides *)
      let temp_id1 = generate_unique_identifier env in
      let exp_id1 = { Ast.exp_pos = Lexing.dummy_pos; Ast.exp = Ast.Identifier temp_id1; Ast.exp_id = Utils.new_id !(Utils.id) } in
      let stm_temp_decl1 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id1]; Ast.stm_id = Utils.new_id !(Utils.id) } in
      let stm_temp_assign1 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id1, exp1); Ast.stm_id = Utils.new_id !(Utils.id) } in
      let env = Env.add temp_id1.Ast.identifier (LocalDecl stm_temp_decl1) env in
      
      let temp_id2 = generate_unique_identifier env in
      let exp_id2 = { Ast.exp_pos = Lexing.dummy_pos; Ast.exp = Ast.Identifier temp_id2; Ast.exp_id = Utils.new_id !(Utils.id) } in
      let stm_temp_decl2 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id2]; Ast.stm_id = Utils.new_id !(Utils.id) } in
      let stm_temp_assign2 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id2, exp2); Ast.stm_id = Utils.new_id !(Utils.id) } in
      let env = Env.add temp_id2.Ast.identifier (LocalDecl stm_temp_decl2) env in
      
      let stm_ptr_assign = { stm with Ast.stm = Ast.PointerAssignment (exp_id1, exp_id2) } in
      
      (stm_temp_decl1 :: stm_temp_decl2 :: stm_temp_assign1 :: stm_temp_assign2 :: stm_ptr_assign :: stms, env))
  | Ast.VarAssignment (id, exp) ->
    let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
    let stm_assign = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (id, normalized_exp); Ast.stm_id = Utils.new_id !(Utils.id) } in
    (new_stms @ stm_assign :: stms, env)

and

normalize_stms stms env =
  List.fold_right
    (fun stm (stms, env) ->
      normalize_stm stm env stms)
    stms ([], env)


let normalize_function func =
  let env = func.EAst.function_decl.EAst.function_env in
  let (body, env) = normalize_stms func.EAst.function_decl.EAst.function_body env in
  let func_decl_desc =
    { EAst.function_name    = func.EAst.function_decl.EAst.function_name;
      EAst.function_formals = func.EAst.function_decl.EAst.function_formals;
      EAst.function_body    = body;
      EAst.function_env     = env } in
  { EAst.function_decl_pos = func.EAst.function_decl_pos;
    EAst.function_decl = func_decl_desc }


let normalize_functions funcs =
  List.fold_right
    (fun func acc ->
      normalize_function func :: acc)
    funcs []


let normalize_program prog =
  { EAst.program_name = prog.EAst.program_name;
    EAst.program_decl = normalize_functions prog.EAst.program_decl }