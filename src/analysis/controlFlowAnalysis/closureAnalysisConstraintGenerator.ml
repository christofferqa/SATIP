let rec add_var_if_not_mem (var: Cubic.variable) (vars: Cubic.variable list) =
  let rec id_mem = (fun (id: Ast.identifier) (vars: Cubic.variable list) ->
    match vars with
    | [] -> false
    | { Ast.exp = Ast.Identifier id'; Ast.exp_pos = _ } :: vars' -> if Ast.i2s id = Ast.i2s id' then true else id_mem id vars'
    | var :: vars' -> id_mem id vars')
    in
  match var.Ast.exp with
  | Ast.Identifier id -> if id_mem id vars then vars else var :: vars
  | _ -> if List.mem var vars then vars else var :: vars

let add_vars_if_not_mem vars vars' =
  List.fold_right (fun var acc ->
    add_var_if_not_mem var acc
  ) vars vars'

open Printf

module EAst = EnvironmentAst
module Graph = Graph.Make (String)

let rec generate_constraints_invocation_exp_aux (exp: Ast.exp) (arguments: Ast.exp list) (function_name: Ast.identifier) (formals: Ast.identifier list) (instance: Cubic.instance): Cubic.instance =
  match arguments, formals with
  | [], [] -> instance
  | argument :: arguments', formal :: formals' ->
    (* Relation to notes: func_name is &f, exp is E, argument is Ei, formal is ai *)
    let instance = { instance with
      Cubic.variables = add_vars_if_not_mem (exp :: argument :: Ast.i2exp formal :: []) instance.Cubic.variables;
      Cubic.constraints = Cubic.ConditionalInclusion (function_name, exp, argument, Ast.i2exp formal) :: instance.Cubic.constraints } in
    generate_constraints_invocation_exp_aux exp arguments' function_name formals' instance
  | _ -> Error.phase "ClosureAnalysisConstraintGenerator" "Internal error. Expected the argument and formal list to be of the same length."

let generate_constraints_invocation_exp (invocation_exp: Ast.exp) (exp: Ast.exp) (arguments: Ast.exp list) (funcs: EAst.function_decl list) (instance: Cubic.instance): Cubic.instance =
  List.fold_right (fun (func: EAst.function_decl) (acc: Cubic.instance) ->
    if List.length func.EAst.function_decl.EAst.function_formals = List.length arguments
    (* TODO: Increase precision by only considering functions where the formals have the correct type *)
    then
      (* Expression (E)(E1,...En) generates the following two constraints for functions f(a1,...,an):
           (1) &f in [[E]] => [[Ei]] subset [[ai]], and
           (2) &f in [[E]] => [[E']] subset [[(E)(E1,...En)]],
         where E' is the return statement of f.
         Notice that generate_constraints_invocation_exp_aux handles the constraints for i=1,...,n in (1). *)
      let function_name = func.EAst.function_decl.EAst.function_name in
      let return_stm = List.nth func.EAst.function_decl.EAst.function_body ((List.length func.EAst.function_decl.EAst.function_body) - 1) in
      let return_exp =
        match return_stm.Ast.stm with
        | Ast.Return exp -> exp
        | _ -> Error.phase "ClosureAnalysisConstraintGenerator" "Internal error. Expected a return statement."
        in
      let acc = { acc with
        Cubic.variables = add_vars_if_not_mem (exp :: return_exp :: invocation_exp :: []) acc.Cubic.variables;
        Cubic.constraints = Cubic.ConditionalInclusion (function_name, exp, return_exp, invocation_exp) :: acc.Cubic.constraints } in
      generate_constraints_invocation_exp_aux exp arguments function_name func.EAst.function_decl.EAst.function_formals acc
    else
      instance
  ) funcs instance

let rec generate_constraints_exp (exp: Ast.exp) (funcs: EAst.function_decl list) (instance: Cubic.instance): Cubic.instance =
  match exp.Ast.exp with
  | Ast.Binop (exp1, binop, exp2) -> generate_constraints_exp exp1 funcs (generate_constraints_exp exp2 funcs instance)
  | Ast.Unop (unop, exp') -> generate_constraints_exp exp' funcs instance
  | Ast.FunctionInvocation (id, exps) ->
    (* Call recursively, arguments may themselves be invocations *)
    let instance = generate_constraints_from_exps exps funcs instance in
    
    (* And add the contraint of the invocation (if arguments match formals, etc.) *)
    generate_constraints_invocation_exp exp (Ast.i2exp id) exps funcs instance
  | Ast.PointerInvocation (exp', exps) ->
    (* Call recursively, arguments may themselves be invocations *)
    let instance = generate_constraints_from_exps exps funcs instance in
    
    (* And add the contraint of the invocation (if arguments match formals, etc.) *)
    generate_constraints_invocation_exp exp exp' exps funcs instance
  | _ -> instance (* Ast.Identifier id, IntConst c, Input, Malloc, Null *)

and

generate_constraints_from_exps (exps: Ast.exp list) (funcs: EAst.function_decl list) (instance: Cubic.instance) : Cubic.instance =
  List.fold_right (fun (exp: Ast.exp) (acc: Cubic.instance) ->
    generate_constraints_exp exp funcs acc
  ) exps instance

let rec generate_constraints_stm (stm: Ast.stm) (funcs: EAst.function_decl list) (instance: Cubic.instance) : Cubic.instance =
  match stm.Ast.stm with
  | Ast.LocalDecl ids -> instance
  | Ast.Output exp -> generate_constraints_exp exp funcs instance
  | Ast.Return exp -> generate_constraints_exp exp funcs instance
  | Ast.While (exp, stms) ->  generate_constraints_exp exp funcs (generate_constraints_stms stms funcs instance)
  | Ast.IfThen (exp, stms) -> generate_constraints_exp exp funcs (generate_constraints_stms stms funcs instance)
  | Ast.PointerAssignment (exp1, exp2) -> generate_constraints_exp exp1 funcs (generate_constraints_exp exp2 funcs instance) (* TODO: Should we handle this case as VarAssignment? *)
  | Ast.IfThenElse (exp, stms1, stms2) -> generate_constraints_exp exp funcs (generate_constraints_stms stms1 funcs (generate_constraints_stms stms2 funcs instance))
  | Ast.VarAssignment (identifier, exp) ->
    let instance = generate_constraints_exp exp funcs instance in
    { instance with
      (* identifier and exp are variables (see next comment): *)
      Cubic.variables = add_vars_if_not_mem (Ast.i2exp identifier :: exp :: []) instance.Cubic.variables;
      (* For assignments id=E we have the constraint: [[E]] subset [[id]]: *)
      Cubic.constraints = Cubic.VarInclusion (exp, Ast.i2exp identifier) :: instance.Cubic.constraints }

and

generate_constraints_stms (stms: Ast.stm list) (funcs: EAst.function_decl list) (instance: Cubic.instance) : Cubic.instance =
  List.fold_right (fun (stm: Ast.stm) (acc: Cubic.instance) ->
    generate_constraints_stm stm funcs acc
  ) stms instance
  

(**
  * Generates an instance for the cubic algorithm, consisting of a list of tokens, variables and constraints.
  * Tokens are the function identifiers, variables are expressions and the constraints are either inclusion or conditional inclusion constraints. 
  *)

let generate_closure_constraints (prog: EAst.program): Cubic.instance =
  List.fold_right (fun (func: EAst.function_decl) (acc: Cubic.instance) ->
    let function_name = func.EAst.function_decl.EAst.function_name in
    
    let acc = {
      (* The function name is both a token and a variable (see next comment): *)
      Cubic.tokens = function_name :: acc.Cubic.tokens;
      Cubic.variables = add_var_if_not_mem (Ast.i2exp function_name) acc.Cubic.variables;
      (* For a constant function name id we have the constraint: {&id} subset [[id]]: *)
      Cubic.constraints = Cubic.TokenInclusion (function_name :: [], Ast.i2exp function_name) :: acc.Cubic.constraints } in
    
    (* Check statements for further constraints: *)
    generate_constraints_stms func.EAst.function_decl.EAst.function_body prog.EAst.program_decl acc
  ) prog.EAst.program_decl { Cubic.tokens = []; Cubic.variables = []; Cubic.constraints = [] }