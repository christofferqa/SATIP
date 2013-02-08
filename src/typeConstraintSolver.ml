
open Printf

module T = Types
module EAst = EnvironmentAst

let rec replace_exp_by_type_exp_var_in_type_exp_var (exp: Ast.exp) (exp_replacement: T.type_exp_variable) (type_exp_var: T.type_exp_variable): T.type_exp_variable =
  match type_exp_var with
  | T.Alpha -> type_exp_var
  | T.Int -> type_exp_var
  | T.Expression exp' -> if exp = exp' then exp_replacement else type_exp_var
  | T.Pointer type_exp_var -> T.Pointer (replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var)
  | T.Function (type_exp_vars, type_exp_var') -> T.Function (replace_exp_by_type_exp_var_in_type_exp_vars exp exp_replacement type_exp_vars,
                                                             replace_exp_by_type_exp_var_in_type_exp_var  exp exp_replacement type_exp_var')
and

replace_exp_by_type_exp_var_in_type_exp_vars (exp: Ast.exp) (exp_replacement: T.type_exp_variable) (type_exp_vars: T.type_exp_variable list): (T.type_exp_variable list) =
  (* Important: Order (i.e. fold_right) matters!
     If fold_left is used then the type_exp_variables for argument formals are reversed,
     see case T.function in replace_exp_by_type_exp_var_in_type_exp_var. *)
  List.fold_right (fun (type_exp_var: T.type_exp_variable) (acc: T.type_exp_variable list) ->
    (*
    printf "Erstat: ";
    Astpp.pp_exp exp;
    printf ", med: ";
    TypeConstraintpp.pp_type_exp_variable exp_replacement;
    printf ", i: ";
    TypeConstraintpp.pp_type_exp_variable type_exp_var;
    printf ". Resultat: ";
    TypeConstraintpp.pp_type_exp_variable (replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var);
    print_newline();
    *)
    (replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var) :: acc
  ) type_exp_vars []


let replace_exp_by_type_exp_var (exp: Ast.exp) (exp_replacement: T.type_exp_variable) (type_constraints: T.type_constraint list): T.type_constraint list =
  List.fold_right (fun ((type_exp_var1, type_exp_var2): T.type_constraint) (acc: T.type_constraint list) ->
    (replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var1,
     replace_exp_by_type_exp_var_in_type_exp_var exp exp_replacement type_exp_var2) :: acc
  ) type_constraints []

let handle_type_constraint ((type_exp_var1, type_exp_var2): T.type_constraint) (stack: T.type_constraint list) (substitutions: T.type_constraint list): (T.type_constraint list) * (T.type_constraint list) =
  if type_exp_var1 = type_exp_var2 then
    (* do nothing *)
    (stack, substitutions)
  else
    match type_exp_var1, type_exp_var2 with
    | Types.Expression exp1, _ ->
      (* Replace each occurence of exp1 with type_exp_var2, and add the popped type constraint to the substitutions: *)
      (replace_exp_by_type_exp_var exp1 type_exp_var2 stack,
       (type_exp_var1, type_exp_var2) :: replace_exp_by_type_exp_var exp1 type_exp_var2 substitutions)
    | _, Types.Expression exp2 ->
      (* Replace each occurence of exp2 with type_exp_var1, and add the popped type constraint to the substitutions: *)
      (replace_exp_by_type_exp_var exp2 type_exp_var1 stack,
       (type_exp_var1, type_exp_var2) :: replace_exp_by_type_exp_var exp2 type_exp_var1 substitutions)
    | _, _ ->
      (* Not typable *)
      Error.error Lexing.dummy_pos "Error in type constraint checking: Program is not typable."

let solve_type_constraints (type_constraints: T.type_constraint list): (T.type_constraint list) =
  let rec visit = (fun (stack: T.type_constraint list) (substitutions: T.type_constraint list) ->
    (* Pop a type constraint: from the stack: *)
    match stack with
    | [] -> substitutions
    | type_constraint :: stack ->
      let (stack, substitutions) = handle_type_constraint type_constraint stack substitutions in
      visit stack substitutions
  ) in
  
  (* Start with the empty substitution, [], and return the solution: *)
  visit type_constraints []