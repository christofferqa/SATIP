(**
  * This phase generates a bunch of type constraints, which is solved using the unification algorithm by
  * the TypeConstraintSolver phase.
  *)

open Printf

module T = Types
module EAst = EnvironmentAst


(**
  * Functions to generate type constraints:
  *)

let rec generate_type_constraints_from_exp (exp: EAst.exp) (constraints: T.type_constraint list): T.type_constraint list =
  match exp.Ast.exp with
  | Ast.IntConst c -> (* [[intconst]] = int *)
    (T.Expression exp, T.Int) :: constraints
    
  | Ast.Input -> (* [[input]] = int *)
    (T.Expression exp, T.Int) :: constraints
    
  | Ast.Malloc -> (* [[malloc]] = &alpha *)
    (T.Expression exp, T.Pointer T.Alpha) :: constraints
    
  | Ast.Null -> (* [[null]] == &alpha *)
    (T.Expression exp, T.Pointer T.Alpha) :: constraints
    
  | Ast.Identifier id -> (* no constraints *)
    constraints
    
  | Ast.Binop (exp1, binop, exp2) -> (* [[E1]] = [[E2]] = [[E1 op E2]] = int *)
    let constraints = generate_type_constraints_from_exp exp2 constraints in
    let constraints = generate_type_constraints_from_exp exp1 constraints in
    (T.Expression exp, T.Int) :: (T.Expression exp1, T.Int) :: (T.Expression exp2, T.Int) :: constraints
      
  | Ast.Unop (unop, exp') ->
    (match unop, exp'.Ast.exp with
     | Ast.Dereference, _ -> (* [[E]] = &[[*E]] *)
       printf "In a dereference: "; Astpp.pp_exp exp'; print_newline();
       (T.Expression exp', T.Pointer (T.Expression exp)) :: constraints
      
     | Ast.Pointer, Ast.Identifier id -> (* [[&id]] = &[[id]] *)
       (T.Expression exp, T.Pointer (T.Expression (Ast.i2exp id))) :: constraints
     | Ast.Pointer, _-> Error.error exp.Ast.exp_pos "Internal error in type checking: Expected identifier inside pointer expression")
    
  | Ast.FunctionInvocation (id, exps) -> (* [[id]] = ([[E1]], ..., [[En]]) -> [[id(E1, ..., En)]] *)
    let constraints = generate_type_constraints_from_exps exps constraints in
    (T.Expression (Ast.i2exp id), T.Function (T.type_exp_variable_list_from_exps exps, T.Expression exp)) :: constraints
    
  | Ast.PointerInvocation (exp, exps) ->
    let constraints = generate_type_constraints_from_exps exps constraints in
    (T.Expression exp, T.Function (T.type_exp_variable_list_from_exps exps, T.Expression exp)) :: constraints

and

generate_type_constraints_from_exps (exps: EAst.exp list) (constraints: T.type_constraint list): T.type_constraint list =
  List.fold_left (fun (constraints: T.type_constraint list) (exp: EAst.exp) ->
    generate_type_constraints_from_exp exp constraints
  ) constraints exps


let rec generate_type_constraints_from_stm (stm: EAst.stm) (constraints: T.type_constraint list): T.type_constraint list =
  match stm.Ast.stm with
  | Ast.Output exp -> (* [[E]] = int *)
    let constraints = generate_type_constraints_from_exp exp constraints in
    (T.Expression exp, T.Int) :: constraints
    
  | Ast.Return exp -> (* no constraints *)
    generate_type_constraints_from_exp exp constraints
    
  | Ast.While (exp, stms) -> (* [[E]] = int *)
    let constraints = generate_type_constraints_from_stms stms constraints in
    let constraints = generate_type_constraints_from_exp exp constraints in
    (T.Expression exp, T.Int) :: constraints
    
  | Ast.IfThen (exp, stms) -> (* [[E]] = int *)
    let constraints = generate_type_constraints_from_stms stms constraints in
    let constraints = generate_type_constraints_from_exp exp constraints in
    (T.Expression exp, T.Int) :: constraints
    
  | Ast.IfThenElse (exp, stms1, stms2) -> (* [[E]] = int *)
    let constraints = generate_type_constraints_from_stms stms2 constraints in
    let constraints = generate_type_constraints_from_stms stms1 constraints in
    let constraints = generate_type_constraints_from_exp exp constraints in
    (T.Expression exp, T.Int) :: constraints
    
  | Ast.VarAssignment (id, exp) -> (* [[id]] = [[E]] *)
    let constraints = generate_type_constraints_from_exp exp constraints in
    (T.Expression (Ast.i2exp id), T.Expression exp) :: constraints
    
  | Ast.PointerAssignment (exp1, exp2) -> (* [[id]] = &[E] *)
    let constraints = generate_type_constraints_from_exp exp2 constraints in
    let constraints = generate_type_constraints_from_exp exp1 constraints in
    (match exp1.Ast.exp with
     | Ast.Unop (Ast.Dereference, exp1') ->
       (match exp1'.Ast.exp with
        | Ast.Identifier id -> (T.Expression (Ast.i2exp id), T.Pointer (T.Expression exp2)) :: constraints
        | _ -> Error.error exp1'.Ast.exp_pos "Internal error in type checking: Expected identifier expression inside dereference expression.")
     | _ -> Error.error exp1.Ast.exp_pos "Internal error in type checking: Expected dereference expression.")
    
  | Ast.LocalDecl ids -> (* no constraints *)
    constraints

and

generate_type_constraints_from_stms (stms: EAst.stm list) (constraints: T.type_constraint list): T.type_constraint list =
  (* Fold right such that the constraints from the first statements comes before the
     constraints of the second statement, etc. *)
  List.fold_right (fun (stm: EAst.stm) (constraints: T.type_constraint list) ->
    generate_type_constraints_from_stm stm constraints
  ) stms constraints


let generate_type_constraints_from_prog (funcs: EAst.function_decl list): T.type_constraint list =
  (* Fold right such that the constraints from the first function comes before the
     constraints of the second function, etc. *)
  List.fold_right (fun (func: EAst.function_decl) (constraints: T.type_constraint list) ->
    let constraints = generate_type_constraints_from_stms func.EAst.function_decl.EAst.function_body constraints in
    let return_stm = List.nth func.EAst.function_decl.EAst.function_body ((List.length func.EAst.function_decl.EAst.function_body) - 1) in
    match return_stm.Ast.stm with
    | Ast.Return return_exp ->
      (T.Expression (Ast.i2exp func.EAst.function_decl.EAst.function_name),
       T.Function (T.type_exp_variable_list_from_identifiers func.EAst.function_decl.EAst.function_formals, T.Expression return_exp)) :: constraints
    | _ -> Error.error return_stm.Ast.stm_pos "Internal error in type checking: Expected return statement." 
  ) funcs []


(**
  * Functions to find a solution to the generated type constraints (if one exist) using the unification
  * algorithm.
  *)

let generate_type_constraints (prog: EAst.program): T.type_constraint list =
  let constraints = generate_type_constraints_from_prog prog.EAst.program_decl in
  constraints