(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *
  * Pretty printer for the AST.
  *)

open EnvironmentAst

(**
  * Functions:
  *)

let rec pp_function func =
  Printf.printf
    "%s(%s) {\n%s\n}\n"
    (Astpp.identifier_to_string func.function_name)
    (Astpp.identifiers_to_string func.function_formals)
    (Astpp.stms_to_string func.function_body "  ")

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

let pp_program prog =
  pp_functions prog.program_decl