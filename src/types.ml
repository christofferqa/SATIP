(**
  * ...
  *)

let expressions_equal (exp1: Ast.exp) (exp2: Ast.exp): bool =
  exp1.Ast.exp = exp2.Ast.exp
  
type type_exp =
  | Int
  | Pointer of type_exp
  | Function of type_exp list * type_exp

type type_exp_variable =
  | AlphaVar
  | IntVar
  | ExpressionVar of Ast.exp
  | PointerVar of type_exp_variable
  | FunctionVar of type_exp_variable list * type_exp_variable

type type_constraint =
  | TypeConstraint of Ast.exp * type_exp_variable


(** Not working: *)

(*
let rec replace_exp_in_type_exp_var (exp: Ast.exp) (replacement: T.type_exp_variable) (haystack: T.type_exp_variable): T.type_exp_variable =
  match haystack with
  | T.IntVar -> haystack
  | T.AlphaVar -> haystack
  | T.PointerVar type_exp_var -> T.PointerVar (replace_exp_in_type_exp_var exp replacement type_exp_var)
  | T.FunctionVar (type_exp_vars, type_exp_var) -> T.FunctionVar (replace_exp_in_type_exp_vars exp replacement type_exp_vars, replace_exp_in_type_exp_var exp replacement type_exp_var)
  | T.ExpressionVar exp' ->
    if T.expressions_equal exp exp'
    then replacement
    else haystack

and

replace_exp_in_type_exp_vars (exp: Ast.exp) (replacement: T.type_exp_variable) (haystack: T.type_exp_variable list): T.type_exp_variable list =
  List.fold_right (fun (type_exp_var: T.type_exp_variable) (acc: T.type_exp_variable list) ->
    replace_exp_in_type_exp_var exp replacement type_exp_var :: acc
  ) haystack []
 

let rec replace_exp_in_constraints (exp: Ast.exp) (replacement: T.type_exp_variable) (constraints: T.type_constraint list): T.type_constraint list =
  (* Replaces all occurences of exp by type_exp_var in the given list *)
  List.fold_right (fun (constr: T.type_constraint) (acc: T.type_constraint list) ->
    match constr with
    | T.TypeConstraint (exp, type_exp_var) ->
      TypeCheckingpp.pp_type_exp_variable type_exp_var;
      printf " ... ";
      let new_type_exp_var = replace_exp_in_type_exp_var exp replacement type_exp_var in
      TypeCheckingpp.pp_type_exp_variable new_type_exp_var;
      print_newline();
      (T.TypeConstraint (exp, new_type_exp_var)) :: acc
  ) constraints []
  

let replace_type_exp_variable_occurrence (type_exp_var: T.type_exp_variable) (exp: Ast.exp) (stack: T.type_constraint list) (substitutions: T.type_constraint list): (T.type_constraint list) * (T.type_constraint list) =
  (* Replaces all occurences of type_exp_var by exp on both the stack and in the substitution. *)
  (stack, substitutions)


let unify (type_constraint: T.type_constraint) (stack: T.type_constraint list) (substitutions: T.type_constraint list): (T.type_constraint list) * (T.type_constraint list) =
  let (exp, type_exp_var) =
    match type_constraint with
    | T.TypeConstraint (exp, type_exp_var) -> (exp, type_exp_var)
    in
  match type_exp_var with
  | T.IntVar ->
    (replace_exp_in_constraints exp T.IntVar stack, replace_exp_in_constraints exp T.IntVar substitutions)
  | T.AlphaVar -> (stack, substitutions)
  | T.PointerVar (type_exp_var) -> (stack, substitutions)
  | T.FunctionVar (type_exp_vars, type_exp_var) -> (stack, substitutions)
  | T.ExpressionVar exp' ->
    if T.expressions_equal exp exp' then
      (* If exp and type_exp_var are the same then do nothing. *)
      (stack, substitutions)
     else
       (* If exp is an identifier, replace all occurrences of exp by type_exp_var on both the stack and in the substitution,
          and add (exp, type_exp_var) to the substitution. *)
       let (stack, substitutions) =
         if Ast.is_identifier exp
         then
           let stack = replace_exp_in_constraints exp type_exp_var stack in
           let substitutions = replace_exp_in_constraints exp type_exp_var substitutions in
          (stack, substitutions)
         else (stack, substitutions) in
       
       (* If type_exp_var is an identifier, replace all occurrences of type_exp_var by exp on both the stack and in the substitution,
          and add (type_exp_var, exp) to the substitution. *)
       (* let (stack, substitutions) = *)
         if Ast.is_identifier exp'
         then replace_type_exp_variable_occurrence type_exp_var exp stack substitutions
         else
           Error.error Lexing.dummy_pos "Error in type checking: The program is not typable."
  | _ -> Error.tip_not_implemented_yet "typechecking.ml" "Todo"
      

let print_pop constr stack substs =
  print_endline "Popped a constraint from the stack:";
  TypeCheckingpp.pp_type_constraint constr;
  print_newline();
  print_newline();
  TypeCheckingpp.pp_type_constraints stack;
  print_newline();
  print_newline()

let solve_type_constraints (constraints: T.type_constraint list) =
  let rec visit = (fun (stack: T.type_constraint list) (substitutions: T.type_constraint list) ->
    match stack with
    | [] ->
      (* Return the substitutions, i.e. a typing: *)
      substitutions
      
    | type_constraint :: stack' ->
      (* Pop a type_constraint of the stack and unify: *)
      print_pop type_constraint stack' substitutions;
      let (stack, substitutions) = unify type_constraint stack' substitutions in
      visit stack substitutions) in

  (* According to the unification algorithm: start with the empty substitution list: *)
  visit constraints []
*)