(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *)

type type_exp_variable =
  | Alpha of int (* uid to recognize other occurences of the same fresh type variable *)
  | Int
  | Expression of Ast.exp
  | Pointer of type_exp_variable
  | Function of type_exp_variable list * type_exp_variable

type type_constraint = type_exp_variable * type_exp_variable


(**
  * Convenient helper functions:
  *)

let type_exp_variable_list_from_identifiers (ids: Ast.identifier list): type_exp_variable list =
  List.fold_right
    (fun id acc -> Expression (Ast.i2exp id) :: acc)
    ids []

let type_exp_variable_list_from_exps exps: type_exp_variable list =
  List.fold_right
    (fun exp acc -> Expression exp :: acc)
    exps []