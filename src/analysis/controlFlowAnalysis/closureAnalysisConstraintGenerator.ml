open Printf

module EAst = EnvironmentAst
module Graph = Graph.Make (String)

let rec generate_constraints_invocation_exp_aux (exp: Ast.exp) (arguments: Ast.exp list) (function_name: Ast.identifier) (formals: Ast.identifier list) (constraints: Cubic.incl_constraint list): Cubic.incl_constraint list =
  match arguments, formals with
  | [], [] -> constraints
  | argument :: arguments', formal :: formals' ->
    (* Relation to notes: func_name is &f, exp is E, argument is Ei, formal is ai *)
    Cubic.ConditionalInclusion (function_name, exp, argument, Ast.i2exp formal)
      :: generate_constraints_invocation_exp_aux exp arguments' function_name formals' constraints
  | _ -> Error.phase "ClosureAnalysisConstraintGenerator" "Internal error. Expected the argument and formal list to be of the same length."

let generate_constraints_invocation_exp (invocation_exp: Ast.exp) (exp: Ast.exp) (arguments: Ast.exp list) (funcs: EAst.function_decl list) (constraints: Cubic.incl_constraint list): Cubic.incl_constraint list =
  List.fold_right (fun (func: EAst.function_decl) (acc: Cubic.incl_constraint list) ->
    let () = Astpp.pp_identifier func.EAst.function_decl.EAst.function_name in let () = print_newline() in
    if List.length func.EAst.function_decl.EAst.function_formals = List.length arguments
    (* TODO: Increase precision by only considering functions where the formals have the correct type *)
    then
      let () = printf "  ok" in let () = print_newline() in
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
      let acc = Cubic.ConditionalInclusion (function_name, exp, return_exp, invocation_exp) :: acc in
      generate_constraints_invocation_exp_aux exp arguments function_name func.EAst.function_decl.EAst.function_formals acc
    else acc
  ) funcs constraints

let rec generate_constraints_exp (exp: Ast.exp) (funcs: EAst.function_decl list) (constraints: Cubic.incl_constraint list): Cubic.incl_constraint list =
  match exp.Ast.exp with
  | Ast.Identifier id -> constraints
  | Ast.Binop (exp1, binop, exp2) -> generate_constraints_exp exp1 funcs (generate_constraints_exp exp2 funcs constraints)
  | Ast.Unop (unop, exp') -> generate_constraints_exp exp' funcs constraints
  | Ast.FunctionInvocation (id, exps) ->
    (* Call recursively, arguments may themselves be invocations *)
    let constraints = generate_constraints_from_exps exps funcs constraints in
    (* And add the contraint of the invocation (if arguments match formals, etc.) *)
    generate_constraints_invocation_exp exp { Ast.exp_pos = Ast.i2p id; Ast.exp = Ast.Identifier id } exps funcs constraints
  | Ast.PointerInvocation (exp', exps) ->
    (* Call recursively, arguments may themselves be invocations *)
    let constraints = generate_constraints_from_exps exps funcs constraints in
    (* And add the contraint of the invocation (if arguments match formals, etc.) *)
    generate_constraints_invocation_exp exp exp' exps funcs constraints
  | _ -> constraints (* IntConst c, Input, Malloc, Null *)

and

generate_constraints_from_exps (exps: Ast.exp list) (funcs: EAst.function_decl list) (constraints: Cubic.incl_constraint list) : Cubic.incl_constraint list =
  List.fold_right (fun (exp: Ast.exp) (acc: Cubic.incl_constraint list) -> generate_constraints_exp exp funcs acc) exps constraints

let rec generate_constraints_stm (stm: Ast.stm) (funcs: EAst.function_decl list) (constraints: Cubic.incl_constraint list) : Cubic.incl_constraint list =
  match stm.Ast.stm with
  | Ast.LocalDecl ids -> constraints
  | Ast.Output exp -> generate_constraints_exp exp funcs constraints
  | Ast.Return exp -> generate_constraints_exp exp funcs constraints
  | Ast.While (exp, stms) ->  generate_constraints_exp exp funcs
                                (generate_constraints_stms stms funcs constraints)
  | Ast.IfThen (exp, stms) -> generate_constraints_exp exp funcs
                                (generate_constraints_stms stms funcs constraints)
  | Ast.IfThenElse (exp, stms1, stms2) -> generate_constraints_exp exp funcs
                                            (generate_constraints_stms stms1 funcs
                                              (generate_constraints_stms stms2 funcs constraints))
  | Ast.PointerAssignment (exp1, exp2) -> generate_constraints_exp exp1 funcs
                                            (generate_constraints_exp exp2 funcs constraints)
  | Ast.VarAssignment (identifier, exp) ->
    Cubic.VarInclusion (exp, Ast.i2exp identifier)
      :: generate_constraints_exp exp funcs constraints

and

generate_constraints_stms (stms: Ast.stm list) (funcs: EAst.function_decl list) (constraints: Cubic.incl_constraint list) : Cubic.incl_constraint list =
  List.fold_right (fun (stm: Ast.stm) (acc: Cubic.incl_constraint list) -> generate_constraints_stm stm funcs acc) stms constraints

let generate_closure_constraints (prog: EAst.program): Cubic.incl_constraint list =
  List.fold_right (fun (func: EAst.function_decl) (acc: Cubic.incl_constraint list) ->
    let function_name = func.EAst.function_decl.EAst.function_name in
    let acc = Cubic.TokenInclusion (function_name :: [], Ast.i2exp function_name) :: acc in
    generate_constraints_stms func.EAst.function_decl.EAst.function_body prog.EAst.program_decl acc
  ) prog.EAst.program_decl []