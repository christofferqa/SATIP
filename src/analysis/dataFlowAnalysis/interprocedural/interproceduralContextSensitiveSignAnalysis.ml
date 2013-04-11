(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com
  *
  * An interprocedural context sensitive sign analysis, that uses the
  * functional approach. The lattice used is (Formals -> Sign) -> (Vars -> Sign),
  * where Formals is the union of the formal arguments of all functions.
  * 
  * The analysis computes the answers lazily, i.e. it only computes the sign
  * of variables in a function for a given context, if the function is actually
  * called with that particular context.
  *)

open SignLattice

module EAst = EnvironmentAst
module CFG = ControlFlowGraph
module CFGUtils = ControlFlowGraphUtils
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

(* Merge functions for finding least upper bounds between elements of the lattices Vars -> Sign, and (Formals -> Sign) -> (Vars -> Sign). *)
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
let eval = SignAnalysis.eval

(* Constraints *)
let join update_func node cfg node_map =
  let ws = CFG.pred_all node cfg in
  List.fold_left
    (fun acc w ->
      let w_value = CFGNodeMap.find w node_map in
      StringMapMap.fold
        (fun p w_p_value acc ->
          let updated_w_p_value = update_func p w_p_value in
          try
            let acc_p_value = StringMapMap.find p acc in
            StringMapMap.add p (StringMap.merge map_merger acc_p_value updated_w_p_value) acc
          with
          | Not_found -> StringMapMap.add p updated_w_p_value acc)
        w_value acc)
    StringMapMap.empty ws (* use the empty map as accumulator instead of bottom; this ensures that things are computed lazily. *)

let make_lambda bottom_formals_sign bottom_vars_sign node cfg node_map =
  match CFG.get_node_content node with
  | CFG.SimpleStm stm ->
    (match stm.Ast.stm with
    | Ast.LocalDecl ids ->
      join
        (fun p w_p_value ->
          List.fold_left
            (fun acc id -> StringMap.add id.Ast.identifier QuestionMark acc)
            w_p_value ids)
        node cfg node_map
        
    | Ast.VarAssignment (id, exp) ->
      join
        (fun p w_p_value -> StringMap.add id.Ast.identifier (eval w_p_value exp) w_p_value)
        node cfg node_map
      
    | _ ->
      join (fun p w_p_value -> w_p_value) node cfg node_map)
      
  | CFG.Entry func ->
    (* Todo: Don't hardcode the following to the main function, but to the last declared function. *)
    if func.Ast.function_decl.Ast.function_name.Ast.identifier = "main" then
      StringMapMap.add bottom_formals_sign bottom_vars_sign StringMapMap.empty
    else
      (* Only compute signs of variables for a function in a given context, *)
      (* if that function is actually called with that context! *)
      let bs = AstUtils.get_function_formals func in
      List.fold_left
        (fun acc w ->
          let w_value = CFGNodeMap.find w node_map in
          let es = ControlFlowGraphUtils.get_call_node_arguments w in
          
          StringMapMap.fold
            (fun p w_value_p acc ->
              let (updated_bottom_formals_sign, updated_bottom_vars_sign) = 
                List.fold_left2
                  (fun (acc_formals_sign, acc_vars_sign) b e ->
                    let eval_res = eval w_value_p e in
                    (StringMap.add b.Ast.identifier eval_res acc_formals_sign, StringMap.add b.Ast.identifier eval_res acc_vars_sign))
                  (bottom_formals_sign, bottom_vars_sign) bs es in
              
              (* By adding updated_bottom_formals_sign as a context, the analysis (i.e. the other constraint rules) *)
              (* will compute signs of variables for this particular context. *)
              StringMapMap.add updated_bottom_formals_sign updated_bottom_vars_sign acc)
            w_value acc)
        StringMapMap.empty (CFG.call_nodes node cfg)
    
  | CFG.AfterCallNode (id, exp, cn_id) ->
    let v' = CFG.call_node node cfg in
    let v'_value = CFGNodeMap.find v' node_map in
    let w = List.hd (CFG.pred node cfg) in
    let w_value = CFGNodeMap.find w node_map in
    
    let bs = CFGUtils.get_function_formals w in
    let es = AstUtils.get_invocation_arguments exp in
    
    (* Exploit that an after call node only have one predecessor, by using StringMapMap.mapi *)
    (* instead of StringMapMap.fold (as the other cases do). *)
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
    join (fun p w_p_value -> w_p_value) node cfg node_map

let analyze_program prog cfg =
  let (bottom_formals_sign, bottom_vars_sign) =
    List.fold_left
      (fun acc func ->
        StringMap.fold
          (fun id decl (acc_formals_sign, acc_vars_sign) ->
            match decl with
            | EnvironmentAst.FunctionDecl _ -> (acc_formals_sign, acc_vars_sign)
            | EnvironmentAst.FormalDecl _ -> (StringMap.add id Bottom acc_formals_sign, StringMap.add id Bottom acc_vars_sign)
            | EnvironmentAst.LocalDecl _ -> (acc_formals_sign, StringMap.add id Bottom acc_vars_sign))
          func.EAst.function_decl.EAst.function_env acc)
      (StringMap.empty, StringMap.empty) prog.EAst.program_decl in
  
  let bottom = StringMapMap.empty in
  
  let fix = FixedPoint.run_worklist (make_lambda bottom_formals_sign bottom_vars_sign) (DFA.dep DFA.Forwards) bottom cfg in
  StringToSignDFA.pp_map_solution fix StringToSignMap.to_string