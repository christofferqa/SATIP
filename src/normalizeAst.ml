open Printf
open EnvironmentStructures
module EAst = EnvironmentAst


(**
  * This phase normalizes the environment AST such that:
  * - all functions calls are of the form: id_1 = (id_2)(a_1,...,a_n); 
  * - all return expressions are variables
  * - each pointer manipulation is one of the six kinds:
  *   1) id = malloc
  *   2) id_1 = &id_2
  *   3) id_1 = id_2
  *   4) id_1 = *id_2
  *   5) *id_1 = id_2
  *   6) id = null
  *)


(**
  * Helper functions.
  *)


(* A function to find a new identifier "tmpi" for a natural number i, that is not in the environment. *)
let generate_unique_identifier env =
  let rec find = (fun str num ->
    let id = Printf.sprintf "%s%d" str num in
    if Env.mem id env
    then find str (num + 1)
    else id) in
  { Ast.identifier_pos = Lexing.dummy_pos; Ast.identifier = find "tmp" 1; Ast.identifier_id = Utils.new_id !(Utils.id) }

(* A function that inserts a temporary assignment of exp into id in front of stms. *)
let add_temp_assign_to id exp stms env =
  let temp_id =
    match id with
    | Some id -> id
    | None -> generate_unique_identifier env in
  let stm_temp_decl = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id]; Ast.stm_id = Utils.new_id !(Utils.id) } in
  let stm_temp_assign = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id, exp); Ast.stm_id = Utils.new_id !(Utils.id) } in
  (temp_id,
   stm_temp_decl :: stm_temp_assign :: stms,
   Env.add temp_id.Ast.identifier (LocalDecl stm_temp_decl) env)


(**
  * Normalization functions.
  *)


let rec normalize_exp exp stms env depth =
  match exp.Ast.exp with
  | Ast.IntConst c -> (exp, stms, env)
  | Ast.Input -> (exp, stms, env)
  | Ast.Null -> (exp, stms, env)
  | Ast.Identifier id -> (exp, stms, env)
  | Ast.Binop (exp1, binop, exp2) ->
    let (normalized_exp1, stms, env) = normalize_exp exp1 stms env (depth + 1) in
    let (normalized_exp2, stms, env) = normalize_exp exp2 stms env (depth + 1) in
    ({ exp with Ast.exp = Ast.Binop (normalized_exp1, binop, normalized_exp2) }, stms, env)
    
  | Ast.FunctionInvocation (id, exps) ->
    let (normalized_exps, stms, env) =
      List.fold_right
        (fun exp (normalized_exps, stms, env) ->
          match exp.Ast.exp with
          | Ast.Identifier _ ->
            (* exp is already normalized *)
            (exp :: normalized_exps, stms, env)
          | _ ->
            let (normalized_exp, stms, env) = normalize_exp exp stms env (depth + 1) in
            (normalized_exp :: normalized_exps, stms, env))
        exps ([], stms, env) in
    ({ exp with Ast.exp = Ast.FunctionInvocation (id, normalized_exps) }, stms, env)
    
  | Ast.PointerInvocation (exp', exps) ->
    let (normalized_exp', stms, env) = normalize_exp exp' stms env (depth + 1) in
    let (normalized_exps, stms, env) =
      List.fold_right
        (fun exp (normalized_exps, stms, env) ->
          match exp.Ast.exp with
          | Ast.Identifier _ ->
            (* exp is already normalized *)
            (exp :: normalized_exps, stms, env)
          | _ ->
            let (normalized_exp, stms, env) = normalize_exp exp stms env (depth + 1) in
            (normalized_exp :: normalized_exps, stms, env))
        exps ([], stms, env) in
    
    let (temp_id, stms, env) = add_temp_assign_to None normalized_exp' stms env in
    ({ exp with Ast.exp = Ast.FunctionInvocation (temp_id, normalized_exps) }, stms, env)
    
  | Ast.Malloc ->
    if depth = 0 then
      (exp, stms, env)
    else
      let (temp_id, stms, env) = add_temp_assign_to None exp stms env in
      (Ast.i2exp temp_id, stms, env)
      
  | Ast.Unop (unop, exp') ->
    let (normalized_exp', stms, env) = normalize_exp exp' stms env (depth + 1) in
    ({ exp with Ast.exp = Ast.Unop (unop, normalized_exp') }, stms, env)


let rec normalize_stm stm env stms =
  match stm.Ast.stm with
  | Ast.LocalDecl ids -> (stm :: stms, env)
  | Ast.Output exp ->
    (* normalize subexpressions *)
    let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
    let stm = { stm with Ast.stm = Ast.Output normalized_exp } in
    (new_stms @ stm :: stms, env)
    
  | Ast.Return exp ->
    (match exp.Ast.exp with
    | Ast.Identifier _ ->
      (* already normalized *)
      (stm :: stms, env)
      
    | _ ->
      (* normalize subexpressions *)
      let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
      
      (* normalize return statement *)
      let temp_id = generate_unique_identifier env in
      let stm_return = { stm with Ast.stm = Ast.Return (Ast.i2exp temp_id) } in
      let (_, stms, env) = add_temp_assign_to (Some temp_id) normalized_exp (stm_return :: stms) env in
      
      (new_stms @ stms, env))
      
  | Ast.While (exp, stms') ->
    (* normalize subexpressions and -statements *)
    let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
    let (normalized_stms, env) = normalize_stms stms' env in
    let stm = { stm with Ast.stm = Ast.While (normalized_exp, normalized_stms) } in
    (new_stms @ stm :: stms, env)
    
  | Ast.IfThen (exp, stms') ->
    (* normalize subexpressions and -statements *)
    let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
    let (normalized_stms, env) = normalize_stms stms' env in
    let stm = { stm with Ast.stm = Ast.IfThen (normalized_exp, normalized_stms) } in
    (new_stms @ stm :: stms, env)
    
  | Ast.IfThenElse (exp, stms1, stms2) ->
    (* normalize subexpressions and -statements *)
    let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
    let (normalized_stms1, env) = normalize_stms stms1 env in
    let (normalized_stms2, env) = normalize_stms stms2 env in
    let stm = { stm with Ast.stm = Ast.IfThenElse (normalized_exp, normalized_stms1, normalized_stms2) } in
    (new_stms @ stm :: stms, env)
    
  | Ast.PointerAssignment (exp1, exp2) ->
    (* normalize pointer assignment *)
    (match exp1.Ast.exp, exp2.Ast.exp with
    | Ast.Unop (Ast.Dereference, _), Ast.Identifier _ ->
      (* already normalized *)
      (stm :: stms, env)
      
    | Ast.Unop (Ast.Dereference, _), _ ->
      (* normalize right hand side *)
      (* normalize subexpressions *)
      let (normalized_exp2, new_stms, env) = normalize_exp exp2 [] env 0 in
      
      (* normalize pointer assignment *)
      let temp_id = generate_unique_identifier env in
      let stm_ptr_assign = { stm with Ast.stm = Ast.PointerAssignment (exp1, Ast.i2exp temp_id) } in
      let (_, stms, env) = add_temp_assign_to (Some temp_id) normalized_exp2 (stm_ptr_assign :: stms) env in
      
      (new_stms @ stms, env)
      
    | _, Ast.Identifier _ ->
      (* normalize left hand side *)
      (* normalize subexpressions *)
      let (normalized_exp1, new_stms, env) = normalize_exp exp1 [] env 0 in
      
      (* normalize pointer assignment *)
      let temp_id = generate_unique_identifier env in
      let stm_ptr_assign = { stm with Ast.stm = Ast.PointerAssignment (Ast.i2exp temp_id, exp2) } in
      let (_, stms, env) = add_temp_assign_to (Some temp_id) normalized_exp1 (stm_ptr_assign :: stms) env in
      
      (new_stms @ stms, env)
      
    | _, _ ->
      (* normalize both sides *)
      (* normalize pointer assignment (left) *)
      let temp_id1 = generate_unique_identifier env in
      let stm_temp_decl1 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id1]; Ast.stm_id = Utils.new_id !(Utils.id) } in
      let stm_temp_assign1 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id1, exp1); Ast.stm_id = Utils.new_id !(Utils.id) } in
      let env = Env.add temp_id1.Ast.identifier (LocalDecl stm_temp_decl1) env in
      
      (* normalize subexpressions *)
      let (normalized_exp2, new_stms, env) = normalize_exp exp2 [] env 0 in
      
      (* normalize pointer assignment (right) *)
      let temp_id2 = generate_unique_identifier env in
      let stm_temp_decl2 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.LocalDecl [temp_id2]; Ast.stm_id = Utils.new_id !(Utils.id) } in
      let stm_temp_assign2 = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (temp_id2, normalized_exp2); Ast.stm_id = Utils.new_id !(Utils.id) } in
      let env = Env.add temp_id2.Ast.identifier (LocalDecl stm_temp_decl2) env in
      
      let stm_ptr_assign = { stm with Ast.stm = Ast.PointerAssignment (Ast.i2exp temp_id1, Ast.i2exp temp_id2) } in
      
      (new_stms @ stm_temp_decl1 :: stm_temp_decl2 :: stm_temp_assign1 :: stm_temp_assign2 :: stm_ptr_assign :: stms, env))
      
  | Ast.VarAssignment (id, exp) ->
    (* normalize subexpressions *)
    let (normalized_exp, new_stms, env) = normalize_exp exp [] env 0 in
    let stm = { Ast.stm_pos = Lexing.dummy_pos; Ast.stm = Ast.VarAssignment (id, normalized_exp); Ast.stm_id = Utils.new_id !(Utils.id) } in
    (new_stms @ stm :: stms, env)

and

normalize_stms stms env =
  List.fold_right
    (fun stm (stms, env) -> normalize_stm stm env stms)
    stms ([], env)

let normalize_function func =
  let (body, env) = normalize_stms func.EAst.function_decl.EAst.function_body
                                   func.EAst.function_decl.EAst.function_env in
  let func_decl_desc =
    { EAst.function_name    = func.EAst.function_decl.EAst.function_name;
      EAst.function_formals = func.EAst.function_decl.EAst.function_formals;
      EAst.function_body    = body;
      EAst.function_env     = env } in
  { EAst.function_decl_pos = func.EAst.function_decl_pos;
    EAst.function_decl = func_decl_desc }

let normalize_functions funcs =
  List.fold_right
    (fun func acc -> normalize_function func :: acc)
    funcs []

let normalize_program prog =
  { EAst.program_name = prog.EAst.program_name;
    EAst.program_decl = normalize_functions prog.EAst.program_decl }