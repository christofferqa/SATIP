(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com
  *)

module CFG = ControlFlowGraph
module CFGNodeSet = Set.Make(ControlFlowGraph.CFGNode)
module CFGNodeMap = Map.Make(ControlFlowGraph.CFGNode)

let run_worklist constraint_lambda dep_lambda bottom cfg =
  (* worklist: a set consisting of each CFG node,
     lambda_map: a map from CFG nodes to their corresponding constraint (a function),
     deps: a map from CFG nodes to a set of CFG nodes the corresponding node depends on,
     bottom_map: a map from CFG nodes to the bottom element of the lattice *)
  let (worklist, lambda_map, dep_map, bottom_map) = 
    CFG.fold
      (fun node (worklist, lambda_map, dep_map, bottom_map) ->
        let dep_map =
          if not (CFGNodeMap.mem node dep_map)
          then CFGNodeMap.add node CFGNodeSet.empty dep_map
          else dep_map in
        let nodes_that_node_depends_on = dep_lambda node cfg in
        let dep_map =
          CFGNodeSet.fold
            (fun dep_node dep_map ->
              (* dep_node depends on node *)
              if CFGNodeMap.mem dep_node dep_map then
                let old = CFGNodeMap.find dep_node dep_map in
                CFGNodeMap.add dep_node (CFGNodeSet.add node old) dep_map
              else
                CFGNodeMap.add dep_node (CFGNodeSet.singleton node) dep_map)
            nodes_that_node_depends_on dep_map in
        
        (CFGNodeSet.add node worklist,
         CFGNodeMap.add node (constraint_lambda node cfg) lambda_map,
         dep_map,
         CFGNodeMap.add node bottom bottom_map))
      (CFGNodeSet.empty, CFGNodeMap.empty, CFGNodeMap.empty, CFGNodeMap.empty) cfg in

  let rec visit =
    (fun worklist solution ->
      if not (CFGNodeSet.is_empty worklist)
      then
        let node_i = CFGNodeSet.choose worklist in
        let worklist' = CFGNodeSet.remove node_i worklist in
        let f_i = CFGNodeMap.find node_i lambda_map in
        let y = f_i solution in
        let x_i = CFGNodeMap.find node_i solution in
        if not (y = x_i)
        then
          let worklist' = CFGNodeSet.union worklist' (CFGNodeMap.find node_i dep_map) in
          let solution' = CFGNodeMap.add node_i y solution in
          visit worklist' solution'
        else
          visit worklist' solution
      else
        solution) in
  visit worklist bottom_map