(**
  * This phase generates a bunch of type constraints, and uses the unification algorithm to find
  * a solution (i.e. a typing), if there is one.
  *
  * See http://cs.brown.edu/courses/cs173/2004/Textbook/12-11-04.pdf for the overall idea.
  *)

open Printf

module T = Types
module EAst = EnvironmentAst


(**
  * Functions to generate type constraints:
  *)

let type_exp_variable_list_from_identifiers (ids: Ast.identifier list): T.type_exp_variable list =
  List.fold_right (fun (id: Ast.identifier) (acc: T.type_exp_variable list) ->
    (T.ExpressionVar (Ast.i2exp id)) :: acc
  ) ids []

let type_exp_variable_list_from_exps (exps: EAst.exp list): T.type_exp_variable list =
  List.fold_right (fun (exp: EAst.exp) (acc: T.type_exp_variable list) ->
    (T.ExpressionVar exp) :: acc
  ) exps []

let rec generate_constraints_from_exp (exp: EAst.exp) (constraints: T.type_constraint list): T.type_constraint list =
  match exp.Ast.exp with
  | Ast.IntConst c -> (* [[intconst]] = int *)
    (T.TypeConstraint (exp, T.IntVar)) :: constraints
    
  | Ast.Input -> (* [[input]] = int *)
    (T.TypeConstraint (exp, T.IntVar)) :: constraints
    
  | Ast.Malloc -> (* [[malloc]] = &alpha *)
    (T.TypeConstraint (exp, T.PointerVar T.AlphaVar)) :: constraints
    
  | Ast.Null -> (* [[null]] == &alpha *)
    (T.TypeConstraint (exp, T.PointerVar T.AlphaVar)) :: constraints
    
  | Ast.Identifier id -> (* no constraints *)
    constraints
    
  | Ast.Binop (exp1, binop, exp2) -> (* [[E1]] = [[E2]] = [[E1 op E2]] = int *)
    (T.TypeConstraint (exp, T.IntVar))
      :: (T.TypeConstraint (exp1, T.IntVar))
      :: (T.TypeConstraint (exp2, T.IntVar))
      :: constraints
      
  | Ast.Unop (unop, exp') ->
    (match unop, exp'.Ast.exp with
     | Ast.Dereference, _ -> (* [[E]] = &[[*E]] *)
       (T.TypeConstraint (exp', T.PointerVar (T.ExpressionVar exp))) :: constraints
      
     | Ast.Pointer, Ast.Identifier id -> (* [[&id]] = &[[id]] *)
       (T.TypeConstraint (exp, T.PointerVar (T.ExpressionVar (Ast.i2exp id)))) :: constraints
     | Ast.Pointer, _-> Error.error exp.Ast.exp_pos "Internal error in type checking: Expected identifier inside pointer expression")
    
  | Ast.FunctionInvocation (id, exps) -> (* [[id]] = ([[E1]], ..., [[En]]) -> [[id(E1, ..., En)]] *)
    let constraints = generate_constraints_from_exps exps constraints in
    (T.TypeConstraint (Ast.i2exp id, T.FunctionVar (type_exp_variable_list_from_exps exps, T.ExpressionVar exp))) :: constraints
    
  | Ast.PointerInvocation (exp, exps) ->
    let constraints = generate_constraints_from_exps exps constraints in
    (T.TypeConstraint (exp, T.FunctionVar (type_exp_variable_list_from_exps exps, T.ExpressionVar exp))) :: constraints

and

generate_constraints_from_exps (exps: EAst.exp list) (constraints: T.type_constraint list): T.type_constraint list =
  List.fold_left (fun (constraints: T.type_constraint list) (exp: EAst.exp) ->
    generate_constraints_from_exp exp constraints
  ) constraints exps


let rec generate_constraints_from_stm (stm: EAst.stm) (constraints: T.type_constraint list): T.type_constraint list =
  match stm.Ast.stm with
  | Ast.Output exp -> (* [[E]] = int *)
    let constraints = generate_constraints_from_exp exp constraints in
    (T.TypeConstraint (exp, T.IntVar)) :: constraints
    
  | Ast.Return exp -> (* no constraints *)
    generate_constraints_from_exp exp constraints
    
  | Ast.While (exp, stms) -> (* [[E]] = int *)
    let constraints = generate_constraints_from_stms stms constraints in
    let constraints = generate_constraints_from_exp exp constraints in
    (T.TypeConstraint (exp, T.IntVar)) :: constraints
    
  | Ast.IfThen (exp, stms) -> (* [[E]] = int *)
    let constraints = generate_constraints_from_stms stms constraints in
    let constraints = generate_constraints_from_exp exp constraints in
    (T.TypeConstraint (exp, T.IntVar)) :: constraints
    
  | Ast.IfThenElse (exp, stms1, stms2) -> (* [[E]] = int *)
    let constraints = generate_constraints_from_stms stms2 constraints in
    let constraints = generate_constraints_from_stms stms1 constraints in
    let constraints = generate_constraints_from_exp exp constraints in
    (T.TypeConstraint (exp, T.IntVar)) :: constraints
    
  | Ast.VarAssignment (id, exp) -> (* [[id]] = [[E]] *)
    let constraints = generate_constraints_from_exp exp constraints in
    (T.TypeConstraint (Ast.i2exp id, T.ExpressionVar exp)) :: constraints
    
  | Ast.PointerAssignment (exp1, exp2) -> (* [[id]] = &[E] *)
    let constraints = generate_constraints_from_exp exp2 constraints in
    let constraints = generate_constraints_from_exp exp1 constraints in
    (match exp1.Ast.exp with
     | Ast.Unop (Ast.Dereference, exp1') ->
       (match exp1'.Ast.exp with
        | Ast.Identifier id -> (T.TypeConstraint (Ast.i2exp id, T.PointerVar (T.ExpressionVar exp2))) :: constraints
        | _ -> Error.error exp1'.Ast.exp_pos "Internal error in type checking: Expected identifier expression inside dereference expression.")
     | _ -> Error.error exp1.Ast.exp_pos "Internal error in type checking: Expected dereference expression.")
    
  | Ast.LocalDecl ids -> (* no constraints *)
    constraints

and

generate_constraints_from_stms (stms: EAst.stm list) (constraints: T.type_constraint list): T.type_constraint list =
  (* Fold right such that the constraints from the first statements comes before the
     constraints of the second statement, etc. *)
  List.fold_right (fun (stm: EAst.stm) (constraints: T.type_constraint list) ->
    generate_constraints_from_stm stm constraints
  ) stms constraints


let generate_constraints_from_prog (funcs: EAst.function_decl list): T.type_constraint list =
  (* Fold right such that the constraints from the first function comes before the
     constraints of the second function, etc. *)
  List.fold_right (fun (func: EAst.function_decl) (constraints: T.type_constraint list) ->
    let constraints = generate_constraints_from_stms func.EAst.function_decl.EAst.function_body constraints in
    let return_stm = List.nth func.EAst.function_decl.EAst.function_body ((List.length func.EAst.function_decl.EAst.function_body) - 1) in
    match return_stm.Ast.stm with
    | Ast.Return return_exp ->
      (T.TypeConstraint (Ast.i2exp func.EAst.function_decl.EAst.function_name,
        T.FunctionVar (type_exp_variable_list_from_identifiers func.EAst.function_decl.EAst.function_formals, T.ExpressionVar return_exp))) :: constraints
    | _ -> Error.error return_stm.Ast.stm_pos "Internal error in type checking: Expected return statement." 
  ) funcs []


(**
  * Functions to find a solution to the generated type constraints (if one exist) using the unification
  * algorithm.
  *)

let type_check_program (prog: EAst.program): T.type_constraint list =
  let constraints = generate_constraints_from_prog prog.EAst.program_decl in
  constraints