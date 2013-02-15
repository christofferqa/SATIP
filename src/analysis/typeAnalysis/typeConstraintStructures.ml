(**
  * ...
  *)

module EAst = EnvironmentAst


let expressions_equal (exp1: Ast.exp) (exp2: Ast.exp): bool =
  exp1.Ast.exp = exp2.Ast.exp

type type_exp_variable =
  | Alpha of int (* uid to recognize other occurences of the same alpha *)
  | Int
  | Expression of Ast.exp
  | Pointer of type_exp_variable
  | Function of type_exp_variable list * type_exp_variable

type type_constraint = type_exp_variable * type_exp_variable


(**
  * Convenient helper functions:
  *)

let type_exp_variable_list_from_identifiers (ids: Ast.identifier list): type_exp_variable list =
  List.fold_right (fun (id: Ast.identifier) (acc: type_exp_variable list) ->
    (Expression (Ast.i2exp id)) :: acc
  ) ids []

let type_exp_variable_list_from_exps (exps: EAst.exp list): type_exp_variable list =
  List.fold_right (fun (exp: EAst.exp) (acc: type_exp_variable list) ->
    (Expression exp) :: acc
  ) exps []