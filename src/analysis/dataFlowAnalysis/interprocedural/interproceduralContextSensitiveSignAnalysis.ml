open SignLattice

module EAst = EnvironmentAst
module CFG = ControlFlowGraph
module CFGNodeMap = Map.Make(ControlFlowGraph.CFGNode)
module StringMap = Map.Make(SetUtils.String)
module StringToSignMap =
  struct
    type t = sign StringMap.t
    let compare = compare
    let to_string = (fun map ->
      let rec visit = (fun map acc ->
        try
          begin
            let (id, sign) = StringMap.choose map in
            if StringMap.cardinal map = 1
            then Printf.sprintf "%s%s -> %s" acc id (sign_to_string sign)
            else visit (StringMap.remove id map) (Printf.sprintf "%s%s -> %s, " acc id (sign_to_string sign))
          end
        with
        | Not_found -> acc) in
      Printf.sprintf "(%s)" (visit map ""))
  end
module StringMapMap = Map.Make(StringToSignMap)
module DFA = DataFlowAnalysis.Make(SetUtils.Sign)
module StringToSignDFA = DataFlowAnalysis.Make(StringToSignMap)

(* Mergers for finding least upper bounds *)
let map_merger = InterproceduralContextInsensitiveSignAnalysis.merger
let map_map_merger context value1 value2 =
  match value1, value2 with
  | Some value1, Some value2 ->
    Some
      (StringMap.mapi
        (fun id sign1 ->
          let sign2 = StringMap.find id value2 in
          SignLattice.least_upper_bound sign1 sign2)
        value1)
  | Some value1, None -> Some value1
  | None, Some value2 -> Some value2
  | _, _ -> None

(* Eval function *)
let rec eval sigma exp =
  match exp.Ast.exp with
  | Ast.Identifier id -> StringMap.find id.Ast.identifier sigma
  | Ast.Input -> QuestionMark
  | Ast.IntConst c -> if c = 0 then Zero else if c = 1 then One else if c > 1 then Plus else (* if c < 0 then *) Minus
  | Ast.Binop (exp1, binop, exp2) -> operator_abstract binop (eval sigma exp1) (eval sigma exp2)
  | _ -> Error.phase "Sign analysis" "Internal error: Expression not handled."

(* Constraints *)
let make_lambda_other bottom bottom_formals_sign bottom_vars_sign node cfg =
  (fun node_map ->
    List.fold_left
      (fun acc w ->
        let w_value = CFGNodeMap.find w node_map in
        StringMapMap.fold
          (fun p w_p_value acc ->
            try
              let acc_p_value = StringMapMap.find p acc in
              StringMapMap.add p (StringMap.merge map_merger acc_p_value w_p_value) acc
            with
            | Not_found -> StringMapMap.add p w_p_value acc)
          w_value acc)
      bottom (CFG.pred_all node cfg))

let make_lambda bottom bottom_formals_sign bottom_vars_sign node cfg =
  (fun node_map ->
    match CFG.get_node_content node with
    | CFG.SimpleStm stm ->
      (match stm.Ast.stm with
      | Ast.LocalDecl ids ->
        List.fold_left
          (fun acc w ->
            let w_value = CFGNodeMap.find w node_map in
            StringMapMap.fold
              (fun p w_p_value acc ->
                let updated_w_p_value =
                  List.fold_left
                    (fun acc id ->
                      StringMap.add id.Ast.identifier QuestionMark acc)
                    w_p_value ids in
                try
                  let acc_p_value = StringMapMap.find p acc in
                  StringMapMap.add p (StringMap.merge map_merger acc_p_value updated_w_p_value) acc
                with
                | Not_found -> StringMapMap.add p updated_w_p_value acc)
              w_value acc)
          bottom (CFG.pred_all node cfg)
          
      | Ast.VarAssignment (id, exp) ->
          List.fold_left
            (fun acc w ->
              let w_value = CFGNodeMap.find w node_map in
              StringMapMap.fold
                (fun p w_p_value acc ->
                  let updated_w_p_value = StringMap.add id.Ast.identifier (eval w_p_value exp) w_p_value in
                  try
                    let acc_p_value = StringMapMap.find p acc in
                    StringMapMap.add p (StringMap.merge map_merger acc_p_value updated_w_p_value) acc
                  with
                  | Not_found -> StringMapMap.add p updated_w_p_value acc)
                w_value acc)
            bottom (CFG.pred_all node cfg)
        
      | _ ->
        make_lambda_other bottom bottom_formals_sign bottom_vars_sign node cfg node_map)
        
    | CFG.Entry func ->
      let bs = func.Ast.function_decl.Ast.function_formals in
      
      let result =
        List.fold_left
          (fun acc w ->
            let w_value = CFGNodeMap.find w node_map in
            let es =
              match CFG.get_node_content w with
              | ControlFlowGraph.CallNode (_, exp, _) ->
                (match exp.Ast.exp with
                | Ast.FunctionInvocation (_, exps) -> exps
                | _ -> Error.phase "Interprocedural Context Insensitive Sign Analysis" "Call node must be a function invocation!")
              | _ -> Error.phase "Interprocedural Context Insensitive Sign Analysis" "Entry node can only have call node predecessors" in
            
            StringMapMap.fold
              (fun p w_value_p acc ->
                let (updated_bottom_formals_sign, updated_bottom_vars_sign) = 
                  List.fold_left2
                    (fun (acc_formals_sign, acc_vars_sign) b e ->
                      let eval_res = eval w_value_p e in
                      (StringMap.add b.Ast.identifier eval_res acc_formals_sign, StringMap.add b.Ast.identifier eval_res acc_vars_sign))
                    (bottom_formals_sign, bottom_vars_sign) bs es in
                StringMapMap.add updated_bottom_formals_sign updated_bottom_vars_sign acc)
              w_value acc)
          StringMapMap.empty (CFG.call_nodes node cfg) in
      
      (* Todo: Don't hardcode the following to the main function, but to the last declared function *)
      if StringMapMap.cardinal result = 0 && func.Ast.function_decl.Ast.function_name.Ast.identifier = "main"
      then StringMapMap.add bottom_formals_sign bottom_vars_sign StringMapMap.empty
      else result
      
    | CFG.AfterCallNode (id, exp, cn_id) ->
      let v' = CFG.call_node node cfg in
      let v'_value = CFGNodeMap.find v' node_map in
      let w = List.hd (CFG.pred node cfg) in
      let w_value = CFGNodeMap.find w node_map in
      
      let bs =
        match CFG.get_node_content w with
        | CFG.Exit func -> func.Ast.function_decl.Ast.function_formals
        | _ -> Error.phase "" "" in
      
      let es =
        match exp.Ast.exp with
        | Ast.FunctionInvocation (id, exps) -> exps
        | _ -> Error.phase "" "" in
      
      StringMapMap.mapi
        (fun p _ ->
          let v'_p_value = StringMapMap.find p v'_value in
          let search_context =
            List.fold_left2
              (fun acc b e ->
                StringMap.add b.Ast.identifier (eval p e) acc)
              bottom_formals_sign bs es in
          let w_search_context_value =
            try StringMapMap.find search_context w_value with
            | Not_found -> bottom_vars_sign in
          let w_search_context_result_value = StringMap.find "result" w_search_context_value in
          StringMap.add id.Ast.identifier w_search_context_result_value v'_p_value)
        v'_value
      
    | _ ->
      make_lambda_other bottom bottom_formals_sign bottom_vars_sign node cfg node_map)

let analyze_program prog cfg =
  let (bottom_formals_sign, bottom_vars_sign) =
    List.fold_left
      (fun acc func ->
        StringMap.fold
          (fun id decl (acc_formals_sign, acc_vars_sign) ->
            match decl with
            | EnvironmentStructures.FunctionDecl _ -> (acc_formals_sign, acc_vars_sign)
            | EnvironmentStructures.FormalDecl _ -> (StringMap.add id Bottom acc_formals_sign, StringMap.add id Bottom acc_vars_sign)
            | EnvironmentStructures.LocalDecl _ -> (acc_formals_sign, StringMap.add id Bottom acc_vars_sign))
          func.EAst.function_decl.EAst.function_env acc)
      (StringMap.empty, StringMap.empty) prog.EAst.program_decl in
  
  let bottom = StringMapMap.empty in
  
  let fix = FixedPoint.run_worklist (make_lambda bottom bottom_formals_sign bottom_vars_sign) (DFA.dep DFA.Forwards) bottom cfg in
  StringToSignDFA.pp_map_solution fix StringToSignMap.to_string