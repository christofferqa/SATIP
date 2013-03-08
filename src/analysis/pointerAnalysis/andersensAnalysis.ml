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
module CubicAlg = Cubic.Make(Instance)

(**
  * Functions to find the Targets set.
  *)

let rec generate_targets_from_exp exp (target_set, taken_addresses_set) =
  match exp.Ast.exp with
  | Ast.Binop (exp1, binop, exp2) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp1 (target_set, taken_addresses_set) in
    generate_targets_from_exp exp2 (target_set, taken_addresses_set)
  | Ast.Unop (unop, exp) ->
    let taken_addresses_set =
      (match unop, exp.Ast.exp with
      | Ast.Pointer, Ast.Identifier _ -> ExpSet.add exp taken_addresses_set
      | _ -> taken_addresses_set) in
    generate_targets_from_exp exp (target_set, taken_addresses_set)
  | Ast.FunctionInvocation (id, exps) ->
    generate_targets_from_exps exps (target_set, taken_addresses_set)
  | Ast.PointerInvocation (exp, exps) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp (target_set, taken_addresses_set) in
    generate_targets_from_exps exps (target_set, taken_addresses_set)
  | Ast.Malloc ->
    (ExpSet.add exp target_set, ExpSet.add exp taken_addresses_set)
  | Ast.Input
  | Ast.Null
  | Ast.IntConst _
  | Ast.Identifier _ -> (target_set, taken_addresses_set)


and

generate_targets_from_exps exps (target_set, taken_addresses_set) =
  List.fold_left
    (fun (target_set, taken_addresses_set) exp -> generate_targets_from_exp exp (target_set, taken_addresses_set))
    (target_set, taken_addresses_set) exps

let rec generate_targets_from_stm stm (target_set, taken_addresses_set) =
  match stm.Ast.stm with
  | Ast.LocalDecl ids ->
    let target_set =
      List.fold_left
        (fun target_set id ->
          ExpSet.add (Ast.i2exp id) target_set)
        target_set ids in
    (target_set, taken_addresses_set)
  | Ast.Output exp ->
    generate_targets_from_exp exp (target_set, taken_addresses_set)
  | Ast.Return exp ->
    generate_targets_from_exp exp (target_set, taken_addresses_set)
  | Ast.While (exp, stms) ->
    let (target_set, taken_addresses_set) = generate_targets_from_exp exp (target_set, taken_addresses_set) in
    generate_targets_from_stms stms (target_set, taken_addresses_set)
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
  | Ast.VarAssignment (identifier, exp) ->
    generate_targets_from_exp exp (target_set, taken_addresses_set)

and

generate_targets_from_stms stms (target_set, taken_addresses_set) =
  List.fold_left
    (fun (target_set, taken_addresses_set) stm -> generate_targets_from_stm stm (target_set, taken_addresses_set))
    (target_set, taken_addresses_set) stms


(**
  * Functions to generate constraints.
  *)

let rec generate_constraints_from_stm stm instance target_set =
  match stm.Ast.stm with
  | Ast.While (exp, stms) -> (* no constraints *)
    generate_constraints_from_stms stms instance target_set
    
  | Ast.IfThen (exp, stms) -> (* no constraints *)
    generate_constraints_from_stms stms instance target_set
    
  | Ast.IfThenElse (exp, stms1, stms2) -> (* no constraints *)
    generate_constraints_from_stms stms1 (generate_constraints_from_stms stms2 instance target_set) target_set
    
  | Ast.VarAssignment (id1, exp) ->
    (match exp.Ast.exp with
    | Ast.Malloc -> (* id1 = malloc: {malloc-i} subset [[id1]] *)
      { instance with CubicAlg.constraints = CubicAlg.TokenInclusion ([exp], Ast.i2exp id1) :: instance.CubicAlg.constraints }
      
    | Ast.Identifier id2 -> (* id1 = id2: [[id2]] subset [[id1]] *)
      { instance with CubicAlg.constraints = CubicAlg.VarInclusion (exp, Ast.i2exp id1) :: instance.CubicAlg.constraints }
      
    | Ast.Unop (Ast.Pointer, id2) -> (* id1 = &id2: {id2} subset [[id1]] *)
      { instance with CubicAlg.constraints = CubicAlg.TokenInclusion ([id2], Ast.i2exp id1) :: instance.CubicAlg.constraints }
      
    | Ast.Unop (Ast.Dereference, id2) -> (* id1 = *id2: alpha in [[id2]] => [[alpha]] subset [[id1]] (for alpha in Targets) *)
      ExpSet.fold
        (fun alpha instance ->
          { instance with CubicAlg.constraints = CubicAlg.ConditionalInclusion (alpha, id2, alpha, Ast.i2exp id1) :: instance.CubicAlg.constraints })
        target_set instance
      
    | _ -> (* no constraints *)
      instance)
    
  | Ast.PointerAssignment (exp1, exp2) ->
    (match exp1.Ast.exp, exp2.Ast.exp with
    | (Ast.Unop (Ast.Dereference, id1), id2) -> (* *id1 = id2: alpha in [[id1]] => [[id2]] subset [[alpha]] (for alpha in Targets) *)
      ExpSet.fold
        (fun alpha instance ->
          { instance with CubicAlg.constraints = CubicAlg.ConditionalInclusion (alpha, id1, exp2, alpha) :: instance.CubicAlg.constraints })
        target_set instance
      
    | _ -> (* no constraints *)
      instance)
    
  | _ -> (* no constraints *)
    instance

and

generate_constraints_from_stms stms instance target_set =
  List.fold_right
    (fun stm instance ->
      generate_constraints_from_stm stm instance target_set)
    stms instance

let generate_constraints prog =
  let (target_set, taken_addresses_set) =
    List.fold_left
      (fun (target_set, taken_addresses_set) func ->
        generate_targets_from_stms func.EAst.function_decl.EAst.function_body (target_set, taken_addresses_set))
      (ExpSet.empty, ExpSet.empty) prog.EAst.program_decl in
      
  let targets = ExpSet.elements target_set in
  
  List.fold_right
    (fun func instance ->
      generate_constraints_from_stms func.EAst.function_decl.EAst.function_body instance taken_addresses_set)
    prog.EAst.program_decl { CubicAlg.tokens = targets; CubicAlg.variables = targets; CubicAlg.constraints = [] }