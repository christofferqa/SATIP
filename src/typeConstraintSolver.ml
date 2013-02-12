(**
  * This phase solves a bunch of type constraints using the unification algorithm.
  *)

open Printf

module T = Types
module EAst = EnvironmentAst


(**
  * Functions for substitutions.
  *)

(* Replaces exp by exp_replacement in type_exp_var *)
let rec replace_exp_by_type_exp_var_in_type_exp_var (exp: Ast.exp) (exp_replacement: T.type_exp_variable) (type_exp_var: T.type_exp_variable): T.type_exp_variable =
  match type_exp_var with
  | T.Alpha -> type_exp_var
  | T.Int -> type_exp_var
  | T.Expression exp' ->
    if (Ast.is_identical_identifiers exp exp') then
      exp_replacement
    else if exp = exp' then
      exp_replacement
    else
      type_exp_var
  | T.Pointer type_exp_var -> T.Pointer (replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var)
  | T.Function (type_exp_vars, type_exp_var') -> T.Function (replace_exp_by_type_exp_var_in_type_exp_vars exp exp_replacement type_exp_vars,
                                                             replace_exp_by_type_exp_var_in_type_exp_var  exp exp_replacement type_exp_var')

and

(* Replaces exp by exp_replacement in type_exp_vars *)
replace_exp_by_type_exp_var_in_type_exp_vars (exp: Ast.exp) (exp_replacement: T.type_exp_variable) (type_exp_vars: T.type_exp_variable list): (T.type_exp_variable list) =
  List.fold_right (fun (type_exp_var: T.type_exp_variable) (acc: T.type_exp_variable list) ->
    (replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var) :: acc
  ) type_exp_vars []


(* Replaces exp by exp_replacement in type_constraints *)
let replace_exp_by_type_exp_var (exp: Ast.exp) (exp_replacement: T.type_exp_variable) (type_constraints: T.type_constraint list): T.type_constraint list =
  List.fold_right (fun ((type_exp_var1, type_exp_var2): T.type_constraint) (acc: T.type_constraint list) ->
    (replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var1,
     replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var2) :: acc
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
     | _ -> (type_exp_var1, type_exp_var2) :: type_constraints)
  | T.Int, T.Expression exp ->
    (match exp.Ast.exp with
     | Ast.IntConst c -> type_constraints
     | _ -> (type_exp_var1, type_exp_var2) :: type_constraints)
  | _, _ -> (type_exp_var1, type_exp_var2) :: type_constraints


(* Unifies type_exp_var1 with type_exp_var2 *)
let unify_one ((type_exp_var1, type_exp_var2): T.type_constraint) (stack: T.type_constraint list) (substitutions: T.type_constraint list): (T.type_constraint list) * (T.type_constraint list) =
  print_endline "Handle type constraint:";
  printf "  type_exp_var1 = ";
  TypeConstraintpp.pp_type_exp_variable type_exp_var1;
  print_newline();
  printf "  type_exp_var2 = ";
  TypeConstraintpp.pp_type_exp_variable type_exp_var2;
  print_newline();
  print_endline "Stack is:";
  TypeConstraintpp.pp_type_constraints stack;
  print_newline();
  print_endline "Substitutions is:";
  TypeConstraintpp.pp_type_constraints substitutions;
  print_newline();
  print_newline();
  if type_exp_var1 = type_exp_var2 then
    (* do nothing *)
    (stack, substitutions)
  else
    match type_exp_var1, type_exp_var2 with
    | Types.Expression exp1, _ ->
      (* Replace each occurence of exp1 with type_exp_var2, and add the popped type constraint to the substitutions: *)
      (replace_exp_by_type_exp_var exp1 type_exp_var2 stack,
       add_if_not_trivial (type_exp_var1, type_exp_var2) (replace_exp_by_type_exp_var exp1 type_exp_var2 substitutions))
    | _, Types.Expression exp2 ->
      (* Replace each occurence of exp2 with type_exp_var1, and add the popped type constraint to the substitutions: *)
      (replace_exp_by_type_exp_var exp2 type_exp_var1 stack,
       add_if_not_trivial (type_exp_var1, type_exp_var2) (replace_exp_by_type_exp_var exp2 type_exp_var1 substitutions))
    | _, _ ->
      (* Not typable *)
      (* (stack, substitutions) *)
      Error.error Lexing.dummy_pos "Error in type constraint checking: Program is not typable."


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