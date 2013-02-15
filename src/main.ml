(** Commandline interface for the compiler. *)

(** Parse a TIP file *)
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
      Error.error Lexing.dummy_pos  ("Unable to open file " ^ msg)

(* Pretty printing functions *)
(* let pp_ast = Astpp.pp_program *)

(** Compile a set of files *)
let compile filename =
  (* helper to apply a phase *)
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
  let ()   = apply Astpp.pp_program prog "ast pretty printing" in
  let east = apply Environment.env_program prog "building environment" in
  let ()   = apply Environmentpp.pp_env east "pretty printing environment" in
  let tcc  = apply TypeConstraintGenerator.generate_type_constraints east "generating type constraints" in
  let ()   = apply TypeConstraintpp.pp_type_constraints tcc "pretty printing type constraints" in
  let tcs  = apply TypeConstraintSolver.solve_type_constraints tcc "solving type constraints" in
  let ()   = apply TypeConstraintpp.pp_type_constraints tcs "pretty printing solution to type constraints" in
  let () = ClosureAnalysisConstraintGenerator.generate_closure_constraints in
  ()


let _ =
  let filename = ref "" in
  let usagemsg = "Usage: tip <filename>" in
  let argspec = Arg.align [] in
  Arg.parse argspec (fun s -> filename := s) usagemsg;
  begin
    (* We are in the batch compiler *)
    print_endline "The TIP compiler.";
    print_newline ();
    if !filename = "" then
      (print_endline "Error: No filename provided";
      Arg.usage argspec usagemsg)
    else
      compile !filename
  end