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

let run_worklist constraint_lambda dep_lambda bottom cfg pp_value =
  (* worklist: a set consisting of each CFG node,
     lambda_map: a map from CFG nodes to their corresponding constraint (a function),
     deps: a map from CFG nodes to a set of CFG nodes the corresponding node depends on,
     bottom_map: a map from CFG nodes to the bottom element of the lattice *)
  let (worklist, lambda_map, dep_map, bottom_map) = 
    ControlFlowGraph.fold
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
      let () = Printf.printf "Input to:\n" in
      let () = pp_value solution in
      print_newline();
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
          let () = Printf.printf "Changed to:\n" in
          let () = pp_value solution' in
          print_newline();
          visit worklist' solution'
        else
          visit worklist' solution
      else
        solution
    ) in
  
  visit worklist bottom_map