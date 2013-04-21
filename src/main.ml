(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *)

let parse_file file_name =
  try
    let inch = open_in file_name in
    let () = print_endline ("Opening \"" ^ file_name ^ "\"") in
    let lexbuf = Lexing.from_channel inch in
    let lcp = lexbuf.Lexing.lex_curr_p in
    let () = lexbuf.Lexing.lex_curr_p <- { lcp with Lexing.pos_fname = file_name } in
    try
      let sf = Parser.goal Lexer.token lexbuf in
      close_in inch;
      flush stdout;
      sf
    with
    | End_of_file ->
        let curr_pos = lexbuf.Lexing.lex_curr_p in
        close_in inch;
        Error.error curr_pos "Parse error: end of file"
    | Parser.Error ->
        let curr_pos = lexbuf.Lexing.lex_curr_p in
        let curr_tok = Lexing.lexeme lexbuf in
        close_in inch;
        Error.error curr_pos ("Parse error: " ^ curr_tok)
    | Failure msg ->
        let curr_pos = lexbuf.Lexing.lex_curr_p in
        close_in inch;
        Error.error curr_pos msg
  with
  | End_of_file ->
      Error.error Lexing.dummy_pos "Parse error: end of file from lexer"
  | Sys_error msg ->
      Error.error Lexing.dummy_pos ("Unable to open file " ^ msg)

let analyze filename =
  let apply phase ast msg =
    print_string "*** ";
    print_endline msg;
    flush stdout;
    let ast' = phase ast in
    print_newline();
    flush stdout;
    ast' in
  let ()   = print_endline "Applying phases:" in
  let ()   = print_newline() in
  
  let prog = apply parse_file filename "parsing" in
  let ()   = apply Astpp.pp_program prog "pretty printing ast" in
  
  let east = apply Environment.env_program prog "building environment" in
  let ()   = apply EnvironmentAstpp.pp_program east "pretty printing environment ast" in
  
  let tc   = apply TypeConstraintGenerator.generate_type_constraints east "generating type constraints" in
  let ()   = apply TypeConstraintpp.pp_type_constraints tc "pretty printing type constraints" in
  let tcs  = apply TypeConstraintSolver.solve_type_constraints tc "solving type constraints" in
  let ()   = apply TypeConstraintpp.pp_type_constraints tcs "pretty printing solution to type constraints" in
  
  let cc   = apply ClosureAnalysis.generate_closure_constraints east "generating closure constraints" in
  let ()   = apply ClosureAnalysis.CubicAlg.pp_instance cc "pretty printing closure constraints" in
  let ccs  = apply ClosureAnalysis.CubicAlg.solve_instance cc "solving closure constraints" in
  let ()   = apply ClosureAnalysis.CubicAlg.pp_solution ccs "pretty printing solution to closure constraints" in
  
  let cfg  = apply (ControlFlowGraph.generate_cfg_from_function (List.nth prog.Ast.program_decl 0)) false "generating cfg" in
  let ()   = apply ControlFlowGraph.pp cfg "pretty printing cfg" in
  let icfg = apply ControlFlowGraph.generate_cfg_from_program prog "generating interprocedural cfg" in
  let ()   = apply ControlFlowGraph.pp icfg "pretty printing interprocedural cfg" in
  
  (try apply (SignAnalysis.analyze_program east) cfg "analyzing signs" with | Exit -> ());
  (try apply (LivenessAnalysis.analyze_program prog) cfg "analyzing liveness" with | Exit -> ());
  (try apply (InitializedVariablesAnalysis.analyze_program prog) cfg "analyzing initialized variables" with | Exit -> ());
  (try apply (ConstantPropagationAnalysis.analyze_program east) cfg "analyzing constant propagation" with | Exit -> ());
  (try apply (ReachingDefinitionsAnalysis.analyze_program prog) cfg "analyzing reaching definitions" with | Exit -> ());
  (try apply (AvailableExpressionsAnalysis.analyze_program prog) cfg "analyzing available expressions" with | Exit -> ());
  (try apply (VeryBusyExpressionsAnalysis.analyze_program prog) cfg "analyzing very busy expressions" with | Exit -> ());
  (try apply (InterproceduralContextInsensitiveSignAnalysis.analyze_program east) icfg "analyzing signs interprocedurally" with | Exit -> ());
  
  (try apply (InterproceduralContextSensitiveSignAnalysis.analyze_program east) icfg "analyzing signs interprocedurally" with | Exit -> ());
  (*
  let nast = apply NormalizeAst.normalize_program east "normalizing program" in
  let ()   = apply EnvironmentAstpp.pp_program nast "pretty printing normalized ast" in
    
  let ac   = apply AndersensAnalysis.generate_constraints nast "generating andersen constraints" in
  let ()   = apply AndersensAnalysis.C.pp_instance ac "pretty printing andersen constraints" in
  let acs  = apply AndersensAnalysis.C.solve_instance ac "solving andersen constraints" in
  let ()   = apply AndersensAnalysis.C.pp_solution acs "pretty printing solution to andersen constraints" in
  *)
  ()

let _ =
  let filename = ref "" in
  let usagemsg = "Usage: tip <filename>" in
  let argspec = Arg.align
    [("-type", Arg.Set Globals.typeAnalysis,
      " ...");
     ("-closure", Arg.Set Globals.closureAnalysis,
      " ...");
     ("-cfg", Arg.Set Globals.cfg,
      " ...");
     ("-liveness", Arg.Set Globals.livenessAnalysis,
      " ...");
     ("-reachingdefs", Arg.Set Globals.reachingDefsAnalysis,
      " ...") ] in
  Arg.parse argspec (fun s -> filename := s) usagemsg;
  begin
    Printf.printf "The TIP analyzer.\n";
    if !filename = "" then
      (print_endline "Error: No filename provided";
      Arg.usage argspec usagemsg)
    else
      analyze !filename
  end