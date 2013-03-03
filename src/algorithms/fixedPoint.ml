open Structures

let make_big_F lambdas =
  List.fold_left
    (fun acc lambda -> (fun v -> acc (lambda v)))
    (fun i -> i) lambdas

let rec naive big_F x =
  let x' = big_F x in
  if x = x'
  then x
  else naive big_F x'

let run_worklist constraint_lambda dep_lambda bottom cfg =
  (* worklist: a set consisting of each CFG node,
     lambdas: a map from CFG nodes to their corresponding constraint (a function),
     deps: a map from CFG nodes to a set of CFG nodes the corresponding node depends on,
     bottom_map: a map from CFG nodes to the bottom element of the lattice *) 
  let (worklist, lambdas, deps, bottom_map) = 
    ControlFlowGraph.fold
      (fun node (worklist, lambdas, deps, bottom_map) ->
        (CFGNodeSet.add node worklist,
         CFGNodeMap.add node (constraint_lambda node cfg) lambdas,
         CFGNodeMap.add node (dep_lambda node cfg) deps,
         CFGNodeMap.add node bottom bottom_map))
      (CFGNodeSet.empty, CFGNodeMap.empty, CFGNodeMap.empty, CFGNodeMap.empty) cfg in

  let rec visit =
    (fun worklist solution ->
      if not (CFGNodeSet.is_empty worklist)
      then
        let node_i = CFGNodeSet.choose worklist in
        let worklist' = CFGNodeSet.remove node_i worklist in
        let f_i = CFGNodeMap.find node_i lambdas in
        let y = f_i solution in
        let x_i = CFGNodeMap.find node_i solution in
        if not (y = x_i)
        then
          let worklist' = CFGNodeSet.union worklist' (CFGNodeMap.find node_i deps) in
          let solution' = CFGNodeMap.add node_i y solution in
          visit worklist' solution'
        else
          visit worklist' solution
      else
        solution
    ) in
  
  visit worklist bottom_map