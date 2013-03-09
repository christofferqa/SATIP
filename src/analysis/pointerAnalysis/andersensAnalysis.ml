open Structures
open EnvironmentStructures

module Instance = struct
  type token = Ast.exp
  let token_compare = (fun e1 e2 ->
    match e1.Ast.exp, e2.Ast.exp with
    | Ast.Identifier id1, Ast.Identifier id2 -> compare id1.Ast.identifier id2.Ast.identifier
    | Ast.Malloc, Ast.Malloc -> compare e1.Ast.exp_id e2.Ast.exp_id
    | Ast.Identifier _, Ast.Malloc -> -1
    | Ast.Malloc, Ast.Identifier _ -> 1
    | _, _ -> Error.phase "Andersen Constraint Generator" "Only expected identifier or malloc tokens.")
  let token_to_string = Astpp.exp_to_string
  
  type variable = Ast.exp
  let variable_compare = token_compare
  let variable_to_string = Astpp.exp_to_string
end

module EAst = EnvironmentAst
module ExpSet = ExpSetCmpIdDesc
module C = Cubic.Make(Instance)

type info =
  { taken_addresses_set : ExpSet.t;
    funcs : EAst.function_decl list;
    func_env : EnvironmentStructures.decl Env.t }

(**
  * Functions to find the Targets set.
  *)

let rec generate_targets_from_exp exp (target_set, taken_addresses_set) =
  match exp.Ast.exp with
  | Ast.Input -> (target_set, taken_addresses_set)
  | Ast.Null -> (target_set, taken_addresses_set)
  | Ast.IntConst _ -> (target_set, taken_addresses_set)
  | Ast.Identifier _ -> (target_set, taken_addresses_set)
  | Ast.FunctionInvocation (id, exps) -> generate_targets_from_exps exps (target_set, taken_addresses_set)
  | Ast.Malloc -> (ExpSet.add exp target_set, ExpSet.add exp taken_addresses_set)
  | Ast.Binop (exp1, binop, exp2) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp1 (target_set, taken_addresses_set) in
    generate_targets_from_exp exp2 (target_set, taken_addresses_set)
    
  | Ast.Unop (unop, exp) ->
    let taken_addresses_set =
      (match unop, exp.Ast.exp with
      | Ast.Pointer, Ast.Identifier _ -> ExpSet.add exp taken_addresses_set
      | _ -> taken_addresses_set) in
    generate_targets_from_exp exp (target_set, taken_addresses_set)
    
  | Ast.PointerInvocation (exp, exps) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp (target_set, taken_addresses_set) in
    generate_targets_from_exps exps (target_set, taken_addresses_set)

and

generate_targets_from_exps exps (target_set, taken_addresses_set) =
  List.fold_left
    (fun (target_set, taken_addresses_set) exp -> generate_targets_from_exp exp (target_set, taken_addresses_set))
    (target_set, taken_addresses_set) exps

let rec generate_targets_from_stm stm (target_set, taken_addresses_set) =
  match stm.Ast.stm with
  | Ast.Output exp
  | Ast.Return exp
  | Ast.VarAssignment (_, exp) -> generate_targets_from_exp exp (target_set, taken_addresses_set)
  
  | Ast.LocalDecl ids ->
    let target_set =
      List.fold_left
        (fun target_set id ->
          ExpSet.add (Ast.i2exp id) target_set)
        target_set ids in
    (target_set, taken_addresses_set)
    
  | Ast.While (exp, stms)
  | Ast.IfThen (exp, stms) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp (target_set, taken_addresses_set) in
    generate_targets_from_stms stms (target_set, taken_addresses_set)
    
  | Ast.IfThenElse (exp, stms1, stms2) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp (target_set, taken_addresses_set) in
    let (target_set, taken_addresses_set) = generate_targets_from_stms stms1 (target_set, taken_addresses_set) in
    generate_targets_from_stms stms2 (target_set, taken_addresses_set)
    
  | Ast.PointerAssignment (exp1, exp2) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp1 (target_set, taken_addresses_set) in
    generate_targets_from_exp exp2 (target_set, taken_addresses_set)

and

generate_targets_from_stms stms (target_set, taken_addresses_set) =
  List.fold_left
    (fun (target_set, taken_addresses_set) stm -> generate_targets_from_stm stm (target_set, taken_addresses_set))
    (target_set, taken_addresses_set) stms


(**
  * Functions to generate constraints.
  *)

let generate_constraints_from_identifier id id_exp instance info =
  (* a reference to a constrant function generates the constraint: {f} subset [[f]] *)
  if Env.mem id.Ast.identifier info.func_env then
    match Env.find id.Ast.identifier info.func_env with
    | EnvironmentStructures.FunctionDecl _ -> { instance with C.constraints = C.TokenInclusion ([id_exp], id_exp) :: instance.C.constraints }
    | _ -> instance
  else
    instance
  

let rec generate_constraints_from_exp exp instance info =
  match exp.Ast.exp with
  | Ast.FunctionInvocation (_, _) (* already handled in generate_constraints_from_stm *)
  | Ast.PointerInvocation (_, _) (* already handled in generate_constraints_from_stm *)
  | Ast.Malloc
  | Ast.Input
  | Ast.Null
  | Ast.IntConst _ -> instance
  | Ast.Binop (exp1, binop, exp2) -> generate_constraints_from_exp exp2 (generate_constraints_from_exp exp1 instance info) info
  | Ast.Unop (unop, exp) -> generate_constraints_from_exp exp instance info
  | Ast.Identifier id ->
    (* a reference to a constrant function generates the constraint: {f} subset [[f]] *)
    generate_constraints_from_identifier id exp instance info

and

generate_constraints_from_exps exps instance info =
  List.fold_right
    (fun exp instance -> generate_constraints_from_exp exp instance info)
    exps instance

let rec generate_constraints_invocation_exp_formals function_name exp arguments formals instance =
  match arguments, formals with
  | [], [] -> instance
  | argument :: arguments', formal :: formals' ->
    let instance = { instance with C.constraints = C.ConditionalInclusion (function_name, exp, argument, Ast.i2exp formal) :: instance.C.constraints } in
    generate_constraints_invocation_exp_formals exp function_name arguments' formals' instance
  | _ -> Error.phase "Andersen's Analysis" "Internal error. Expected the argument and formal list to be of the same length."

let rec generate_constraints_from_stm stm instance info =
  match stm.Ast.stm with
  | Ast.While (exp, stms) ->
    (* no constraints *)
    generate_constraints_from_stms stms instance info
    
  | Ast.IfThen (exp, stms) ->
    (* no constraints *)
    generate_constraints_from_stms stms instance info
    
  | Ast.IfThenElse (exp, stms1, stms2) ->
    (* no constraints *)
    generate_constraints_from_stms stms1 (generate_constraints_from_stms stms2 instance info) info
    
  | Ast.VarAssignment (id1, exp) ->
    (match exp.Ast.exp with
    | Ast.Malloc ->
      (* id1 = malloc: {malloc-i} subset [[id1]] *)
      { instance with C.constraints = C.TokenInclusion ([exp], Ast.i2exp id1) :: instance.C.constraints }
      
    | Ast.Identifier id2 ->
      (* id1 = id2: [[id2]] subset [[id1]] *)
      { instance with C.constraints = C.VarInclusion (exp, Ast.i2exp id1) :: instance.C.constraints }
      
    | Ast.Unop (Ast.Pointer, id2) ->
      (* id1 = &id2: {id2} subset [[id1]] *)
      { instance with C.constraints = C.TokenInclusion ([id2], Ast.i2exp id1) :: instance.C.constraints }
      
    | Ast.Unop (Ast.Dereference, id2) ->
      (* id1 = *id2: alpha in [[id2]] => [[alpha]] subset [[id1]] (for alpha in Targets) *)
      ExpSet.fold
        (fun alpha instance -> { instance with C.constraints = C.ConditionalInclusion (alpha, id2, alpha, Ast.i2exp id1) :: instance.C.constraints })
        info.taken_addresses_set instance
     
    | Ast.FunctionInvocation (id2, exps) ->
      (* a reference to a constant function generates the constraint: {f} subset [[f]] *)
      let instance = generate_constraints_from_identifier id2 exp instance info in
      
      (* function calls (id_2)(a_1,...a_n) generates the constraint f in [[id_2]] => [[a_1]] subset [[x_1]] /\ ... /\ [[a_n]] subset [[x_n]] /\ [[id]] subset [[id_1]], for ... *)
      let instance =
        List.fold_right
          (fun func instance ->
            let function_name = Ast.i2exp func.EAst.function_decl.EAst.function_name in
            let return_exp = EAst.return_exp_from_func func in

            (* The constraint for the return expression *)
            let instance = { instance with C.constraints = C.ConditionalInclusion (function_name, Ast.i2exp id2, return_exp, Ast.i2exp id1) :: instance.C.constraints } in
            
            (* The constraints for the formals *)
            generate_constraints_invocation_exp_formals function_name (Ast.i2exp id2) exps func.EAst.function_decl.EAst.function_formals instance)
          info.funcs instance in
      
      (* no need to check exps for constraints, as functions invocations has been normalized! *)
      instance
    
    | Ast.PointerInvocation (exp, exps) ->
      (* doesn't occur since AST have been normalized *)
      Error.phase "Andersen's Analysis" "Error. AST not properly normalized."
      
    | _ ->
      (* no constraints *)
      instance)
    
  | Ast.PointerAssignment (exp1, exp2) ->
    (match exp1.Ast.exp, exp2.Ast.exp with
    | (Ast.Unop (Ast.Dereference, id1), id2) ->
      (* *id1 = id2: alpha in [[id1]] => [[id2]] subset [[alpha]] (for alpha in Targets) *)
      ExpSet.fold
        (fun alpha instance -> { instance with C.constraints = C.ConditionalInclusion (alpha, id1, exp2, alpha) :: instance.C.constraints })
        info.taken_addresses_set instance
      
    | _ ->
      (* no constraints *)
      instance)
    
  | _ ->
    (* no constraints *)
    instance

and

generate_constraints_from_stms stms instance info =
  List.fold_right
    (fun stm instance -> generate_constraints_from_stm stm instance info)
    stms instance

let generate_constraints prog =
  let (target_set, taken_addresses_set) =
    List.fold_left
      (fun (target_set, taken_addresses_set) func ->
        let (target_set, taken_addresses_set) = generate_targets_from_stms func.EAst.function_decl.EAst.function_body (target_set, taken_addresses_set) in
        let function_name = Ast.i2exp func.EAst.function_decl.EAst.function_name in
        (ExpSet.add function_name target_set, taken_addresses_set))
      (ExpSet.empty, ExpSet.empty) prog.EAst.program_decl in
      
  let targets = ExpSet.elements target_set in
  
  List.fold_right
    (fun func instance ->
      let info =
        { taken_addresses_set = taken_addresses_set;
          funcs = prog.EAst.program_decl;
          func_env = func.EAst.function_decl.EAst.function_env } in
      generate_constraints_from_stms func.EAst.function_decl.EAst.function_body instance info)
    prog.EAst.program_decl { C.tokens = targets;
                             C.variables = targets;
                             C.constraints = [] }