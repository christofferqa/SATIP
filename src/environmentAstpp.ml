(**
  * Pretty printer for the AST.
  *)

open Printf
open EnvironmentAst

(**
  * Functions
  *)

let rec pp_function func =
  Astpp.pp_identifier func.function_name;
  printf "(";
  Astpp.pp_identifiers func.function_formals;
  printf ") {";
  print_newline();
  Astpp.pp_stms func.function_body "  ";
  print_newline();
  printf "}";
  print_newline()

and

pp_functions funcs =
  match funcs with
  | [] -> ()
  | func :: [] ->
    pp_function func.function_decl
  | func :: funcs' ->
    pp_function func.function_decl;
    print_newline();
    pp_functions funcs'


(**
  * Programs
  *)

let rec pp_program prog =
  pp_functions prog.program_decl