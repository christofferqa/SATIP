open Printf

module T = Types
module EAst = EnvironmentAst


(**
  * Function for printing the type constraints of the program.
  *)

let rec pp_type_exp_variable (type_exp_var: T.type_exp_variable) =
  match type_exp_var with
  | T.AlphaVar -> printf "alpha"
  | T.IntVar -> printf "int"
  | T.IdentifierVar id -> printf "[[%s]]" (Ast.i2s id)
  | T.ExpressionVar exp -> printf "[["; (Astpp.pp_exp exp); printf "]]";
  | T.PointerVar type_exp_var -> printf "&"; pp_type_exp_variable type_exp_var
  | T.FunctionVar (type_exp_vars, type_exp_var) ->
    printf "(";
    pp_type_exp_variables type_exp_vars;
    printf ")->";
    pp_type_exp_variable type_exp_var

and

pp_type_exp_variables (type_exp_vars: T.type_exp_variable list) =
  match type_exp_vars with
  | [] -> ()
  | type_exp_var :: [] -> pp_type_exp_variable type_exp_var
  | type_exp_var :: type_exp_vars' ->
    pp_type_exp_variable type_exp_var;
    printf ", ";
    pp_type_exp_variables type_exp_vars'


let pp_type_constraints (constraints: T.type_constraint list) =
  print_endline "Generated constraints:";
  List.iter (fun (constr: T.type_constraint) ->
    (match constr with
     | T.TypeIdentifierConstraint (id, type_exp_var) ->
       printf "  [[%s]] = " (Ast.i2s id);
       pp_type_exp_variable type_exp_var
     | T.TypeExpConstraint (exp, type_exp_var) ->
       printf "  [[";
       Astpp.pp_exp exp;
       printf "]] = ";
       pp_type_exp_variable type_exp_var);
    print_newline();
  ) constraints