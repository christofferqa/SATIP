let solve_closure_constraints (constraints: Cubic.incl_constraint list): (Cubic.variable * Cubic.token list) list =
  Cubic.solve_constraints [] [] constraints