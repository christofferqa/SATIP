(*
open Structures

module CFG = ControlFlowGraph

type variable = CFG.node

type df_constraint =
  | Intersection of df_constraint list
  | Union of df_constraint list
  | Variable of variable
  | Constant of ExpSet.t
  | Remove of df_constraint * ExpSet.t

let join = DataFlowAnalysis.join_forward_must

let kill = ()

let exps (exp: Ast.exp): (Ast.exp list) =
  let rec visit = (fun (exp: Ast.exp) (acc: Ast.exp list) ->
    match exp.Ast.exp with
    | Ast.Binop (exp1, binop, exp2) ->
      let (exps, exp1_contains_input) = visit exp1 exps in
      let (exps, exp2_contains_input) = visit exp2 exps in
      if not exp1_contains_input && not exp2_contains_input
      then exp :: exps
      else exps
    | Ast.FunctionInvocation (id, exps) ->
      List.fold_left
        (fun (exps, input) exp ->
          let (exps, exp_contains_input) = visit exp exps in
          if exp_contains_input
          then (exps, true)
          else exp_exps @ )
        ([], false) exps
    | Ast.PointerInvocation (exp, exps) -> List.fold_left (fun acc exp -> visit exp :: acc) [] exps
    | Ast.Input -> ([], true)
    | Ast.Unop (unop, exp) -> exps exp
    | _ -> ([], false)
    ) in
  let (exps, _) = visit exp [] in
  exps

let entry_constraints (preds: CFG.node list) (succs: CFG.node list) =
  ExpSet.empty

let exit_constraints (preds: CFG.node list) (succs: CFG.node list) =
  join preds succs

let expression_constraints (exp: Ast.exp) (preds: CFG.node list) (succs: CFG.node list) =
  Set.intersection 

let statement_constraints (stm: Ast.stm) (preds: CFG.node list) (succs: CFG.node list) =
  match stm.Ast.stm with
  | Ast.VarAssignment (id, exp) ->
    Remove (Union (join preds succs, exps exp), id)
  | Ast.PointerAssignment (exp1, exp2) ->
    let id =
      (match exp1.Ast.exp with
       | Ast.Unop (Ast.Dereference, id) -> id
       | _ -> Error.phase "AvailableExpressionsAnalysis" "Expected identifier inside dereference") in
    Remove (Union (join preds succs, exps exp2), id)
  | Ast.Output exp -> Union (join pred succ, exps e)
  | _ -> join preds succs
*)