(**
  * This phase solves a bunch of type constraints using the unification algorithm.
  *)

open Printf

module T = Types
module EAst = EnvironmentAst


(**
  * Functions for substitutions.
  *)

(* Replaces type_exp_var by type_exp_var_replacement in type_exp_var' *)
let rec subst_in_type_exp_var (type_exp_var: T.type_exp_variable) (type_exp_var_replacement: T.type_exp_variable) (type_exp_var': T.type_exp_variable): T.type_exp_variable =
  (*
  printf "In subst:";
  printf "  type_exp_var = "; TypeConstraintpp.pp_type_exp_variable type_exp_var; print_newline();
  printf "  type_exp_var_replacement = "; TypeConstraintpp.pp_type_exp_variable type_exp_var_replacement; print_newline();
  printf "  type_exp_var' = "; TypeConstraintpp.pp_type_exp_variable type_exp_var'; print_newline();
  *)
  if type_exp_var = type_exp_var'
  then type_exp_var_replacement
  else
    match type_exp_var' with
    | Types.Alpha uid -> type_exp_var'
    | Types.Int -> type_exp_var'
    | Types.Expression exp' ->
      (match type_exp_var with
       | Types.Expression exp ->
         (* Make a special check on identifiers, since they may be equal even though the two type_exp_var's are not *)
         if (Ast.is_identical_identifiers exp exp') then
           type_exp_var_replacement
         else
           type_exp_var'
       | _ -> type_exp_var')
    | Types.Pointer type_exp_var' -> Types.Pointer (subst_in_type_exp_var type_exp_var type_exp_var_replacement type_exp_var')
    | Types.Function (type_exp_vars, type_exp_var') -> T.Function (subst_in_type_exp_vars type_exp_var type_exp_var_replacement type_exp_vars,
                                                                   subst_in_type_exp_var  type_exp_var type_exp_var_replacement type_exp_var')

and

(* Replaces type_exp_var by type_exp_var_replacement in type_exp_vars *)
subst_in_type_exp_vars (type_exp_var: T.type_exp_variable) (type_exp_var_replacement: T.type_exp_variable) (type_exp_vars: T.type_exp_variable list): (T.type_exp_variable list) =
  List.fold_right (fun (type_exp_var': T.type_exp_variable) (acc: T.type_exp_variable list) ->
    (subst_in_type_exp_var type_exp_var type_exp_var_replacement type_exp_var') :: acc
  ) type_exp_vars []

(* Replaces exp by exp_replacement in type_constraints *)
let subst_in_type_constraints (type_exp_var: T.type_exp_variable) (type_exp_var_replacement: T.type_exp_variable) (type_constraints: T.type_constraint list): T.type_constraint list =
  List.fold_right (fun ((type_exp_var1, type_exp_var2): T.type_constraint) (acc: T.type_constraint list) ->
    (subst_in_type_exp_var type_exp_var type_exp_var_replacement type_exp_var1,
     subst_in_type_exp_var type_exp_var type_exp_var_replacement type_exp_var2) :: acc
  ) type_constraints []


(**
  * Unify a type_constraint consisting of two type_exp_variables.
  *)

(* Adds the type_constraint to type_constraints, if it is not trivial, i.e. for instance [[1]] = int *)
let add_if_not_trivial ((type_exp_var1, type_exp_var2): T.type_constraint) (type_constraints: T.type_constraint list): (T.type_constraint list) =
  match type_exp_var1, type_exp_var2 with
  | T.Int, T.Int -> type_constraints
  | T.Expression exp, T.Int ->
    (match exp.Ast.exp with
     | Ast.IntConst c -> type_constraints
     | Ast.Input -> type_constraints
     | _ -> (type_exp_var1, type_exp_var2) :: type_constraints)
  | T.Int, T.Expression exp ->
    (match exp.Ast.exp with
     | Ast.IntConst c -> type_constraints
     | _ -> (type_exp_var2, type_exp_var1) :: type_constraints)
  | T.Alpha uid, _ -> type_constraints
  | _, T.Alpha uid -> type_constraints
  | _, _ -> (type_exp_var1, type_exp_var2) :: type_constraints


(* Unifies type_exp_var1 with type_exp_var2 *)
let rec unify_one ((type_exp_var1, type_exp_var2): T.type_constraint) (stack: T.type_constraint list) (substitutions: T.type_constraint list): (T.type_constraint list) * (T.type_constraint list) =
  (*
  print_endline "Handle type constraint:";
  printf "  type_exp_var1 = "; TypeConstraintpp.pp_type_exp_variable type_exp_var1; print_newline();
  printf "  type_exp_var2 = "; TypeConstraintpp.pp_type_exp_variable type_exp_var2; print_newline(); print_newline();
  print_endline "Stack is:"; TypeConstraintpp.pp_type_constraints stack; print_newline();
  print_endline "Substitutions is:"; TypeConstraintpp.pp_type_constraints substitutions; print_newline(); print_newline();
  *)
  if type_exp_var1 = type_exp_var2 then
    (* do nothing *)
    (stack, substitutions)
  else
    match type_exp_var1, type_exp_var2 with
    | Types.Expression exp1, Types.Alpha uid ->
      (* Replace each occurence of alpha with type_exp_var1, and add the popped type constraint to the substitutions: *)
      (subst_in_type_constraints type_exp_var2 type_exp_var1 stack,
       add_if_not_trivial (type_exp_var1, type_exp_var2) (subst_in_type_constraints type_exp_var2 type_exp_var1 substitutions))
      
    | Types.Expression exp1, _ ->
      (* Replace each occurence of type_exp_var1 with type_exp_var2, and add the popped type constraint to the substitutions: *)
      (subst_in_type_constraints type_exp_var1 type_exp_var2 stack,
       add_if_not_trivial (type_exp_var1, type_exp_var2) (subst_in_type_constraints type_exp_var1 type_exp_var2 substitutions))
      
    | Types.Alpha uid, Types.Expression exp2 ->
      (* Replace each occurence of alpha with exp1: *)
      (subst_in_type_constraints type_exp_var1 type_exp_var2 stack,
       add_if_not_trivial (type_exp_var1, type_exp_var2) (subst_in_type_constraints type_exp_var1 type_exp_var2 substitutions))
      
    | _, Types.Expression exp2 ->
      (* Replace each occurence of type_exp_var2 with type_exp_var1, and add the popped type constraint to the substitutions: *)
      (subst_in_type_constraints type_exp_var2 type_exp_var1 stack,
       add_if_not_trivial (type_exp_var1, type_exp_var2) (subst_in_type_constraints type_exp_var2 type_exp_var1 substitutions))
      
    | Types.Pointer type_exp_var1', Types.Pointer type_exp_var2' ->
      (* Call recursively *)
      unify_one (type_exp_var1', type_exp_var2') stack substitutions
      
    | Types.Function (type_exp_vars1, type_exp_var1'), Types.Function (type_exp_vars2, type_exp_var2') ->
      let rec add_argument_constraints = (fun (type_exp_vars1: Types.type_exp_variable list) (type_exp_vars2: Types.type_exp_variable list) (acc: Types.type_constraint list) ->
        match type_exp_vars1, type_exp_vars2 with
        | [], [] -> acc
        | type_exp_var1 :: type_exp_vars1', type_exp_var2 :: type_exp_vars2' -> (type_exp_var1, type_exp_var2) :: add_argument_constraints type_exp_vars1' type_exp_vars2' acc
        | _, _ -> Error.error Lexing.dummy_pos "TypeConstraintSolver: Error, expected argument lists to be of same lenght!"
      ) in
      let stack = add_argument_constraints type_exp_vars1 type_exp_vars2 stack in
      (* Add the following constraint to the stack: type_exp_var1' = type_exp_var2': *)
      ((type_exp_var1', type_exp_var2') :: stack,
       substitutions)
      
    | _, _ ->
      (* Not typable *)
      Error.phase "TypeConstraintSolver"
        ("The program is not typable. Cannot unify " ^
        (TypeConstraintpp.type_exp_variable_to_string type_exp_var1) ^ " with " ^
        (TypeConstraintpp.type_exp_variable_to_string type_exp_var2) ^ ".")


(**
  * Unifications. Finds a solution to the type_constraints if one exists.
  *)

let solve_type_constraints (type_constraints: T.type_constraint list): (T.type_constraint list) =
  let rec visit = (fun (stack: T.type_constraint list) (substitutions: T.type_constraint list) ->
    (* Pop a type constraint: from the stack: *)
    match stack with
    | [] -> substitutions
    | type_constraint :: stack ->
      let (stack, substitutions) = unify_one type_constraint stack substitutions in
      visit stack substitutions
  ) in
  
  (* Start with the empty substitution, [], and return the solution: *)
  visit type_constraints []