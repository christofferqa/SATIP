open Printf

module T = Types
module EAst = EnvironmentAst


(**
  * To string functions.
  *)

let rec type_exp_variable_to_string (type_exp_var: T.type_exp_variable): string =
  match type_exp_var with
  | T.Alpha uid -> "alpha(" ^ (string_of_int uid) ^ ")"
  | T.Int -> "int"
  | T.Expression exp -> "[[" ^ (Astpp.exp_to_string exp) ^ "]]";
  | T.Pointer type_exp_var' -> "&" ^ (type_exp_variable_to_string type_exp_var')
  | T.Function (type_exp_vars, type_exp_var') ->
    "(" ^ (type_exp_variables_to_string type_exp_vars) ^ ")->" ^ (type_exp_variable_to_string type_exp_var')

and

type_exp_variables_to_string (type_exp_vars: T.type_exp_variable list): string =
  match type_exp_vars with
  | [] -> ""
  | type_exp_var :: [] -> type_exp_variable_to_string type_exp_var
  | type_exp_var :: type_exp_vars' -> (type_exp_variable_to_string type_exp_var) ^ ", " ^ (type_exp_variables_to_string type_exp_vars')


(**
  * Function for printing the type constraints of the program.
  *)

let pp_type_exp_variable (type_exp_var: T.type_exp_variable) =
  printf "%s" (type_exp_variable_to_string type_exp_var)

let pp_type_exp_variables (type_exp_vars: T.type_exp_variable list) =
  printf "%s" (type_exp_variables_to_string type_exp_vars)

let pp_type_constraint ((type_exp_var1, type_exp_var2): T.type_constraint) =
  printf "  ";
  pp_type_exp_variable type_exp_var1;
  printf " = ";
  pp_type_exp_variable type_exp_var2


let pp_type_constraints (type_constraints: T.type_constraint list) =
  List.iter (fun (type_constraint: T.type_constraint) ->
    pp_type_constraint type_constraint;
    print_newline()
  ) type_constraints