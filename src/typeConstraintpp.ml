open Printf

module T = Types
module EAst = EnvironmentAst


(**
  * Function for printing the type constraints of the program.
  *)

let rec pp_type_exp_variable (type_exp_var: T.type_exp_variable) =
  match type_exp_var with
  | T.Alpha -> printf "alpha"
  | T.Int -> printf "int"
  | T.Expression exp -> printf "[["; (Astpp.pp_exp exp); printf "]]";
  | T.Pointer type_exp_var' -> printf "&"; pp_type_exp_variable type_exp_var'
  | T.Function (type_exp_vars, type_exp_var') ->
    printf "(";
    pp_type_exp_variables type_exp_vars;
    printf ")->";
    pp_type_exp_variable type_exp_var'

and

pp_type_exp_variables (type_exp_vars: T.type_exp_variable list) =
  match type_exp_vars with
  | [] -> ()
  | type_exp_var :: [] -> pp_type_exp_variable type_exp_var
  | type_exp_var :: type_exp_vars' ->
    pp_type_exp_variable type_exp_var;
    printf ", ";
    pp_type_exp_variables type_exp_vars'


let pp_type_constraint ((type_exp_var1, type_exp_var2): T.type_constraint) =
  printf "  [[";
  pp_type_exp_variable type_exp_var1;
  printf "]] = ";
  pp_type_exp_variable type_exp_var2


let pp_type_constraints (type_constraints: T.type_constraint list) =
  print_endline "Generated constraints:";
  List.iter (fun (type_constraint: T.type_constraint) ->
    pp_type_constraint type_constraint;
    print_newline()
  ) type_constraints