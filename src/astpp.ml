(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *
  * Pretty printer for the AST.
  *)

open Ast


(**
  * To string functions:
  *)

let identifier_to_string id =
  id.identifier

let rec identifiers_to_string ids =
  match ids with
  | [] -> ""
  | id :: [] -> identifier_to_string id
  | id :: ids' -> (identifier_to_string id) ^ ", " ^ (identifiers_to_string ids')

let unop_to_string unop =
  match unop with
  | Pointer -> "&"
  | Dereference -> "*"

let binop_to_string binop =
  match binop with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Gt -> ">"
  | Eq -> "=="

let rec exp_to_string exp =
  match exp.exp with
  | IntConst c -> Printf.sprintf "%d" c
  | Identifier id -> Ast.i2s id
  | Binop (exp1, binop, exp2) -> (exp_to_string exp1) ^ (binop_to_string binop) ^ (exp_to_string exp2)
  | Unop (unop, exp') -> (unop_to_string unop) ^ (exp_to_string exp')
  | Input -> "input"
  | Malloc -> "malloc(" ^ (string_of_int exp.Ast.exp_id) ^ ")"
  | Null -> "null"
  | FunctionInvocation (id, exps) -> (Ast.i2s id) ^ "(" ^ (exps_to_string exps) ^ ")"
  | PointerInvocation (exp', exps) -> "(" ^ (exp_to_string exp') ^ ")(" ^ (exps_to_string exps) ^ ")"

and

exps_to_string exps =
  match exps with
  | [] -> ""
  | exp :: [] -> exp_to_string exp
  | exp :: exps' -> (exp_to_string exp) ^ ", " ^ (exps_to_string exps')

let rec simple_stm_to_string stm =
  match stm.stm with
  | VarAssignment (id, exp) -> (identifier_to_string id) ^ " = " ^ (exp_to_string exp) ^ ";"
  | PointerAssignment (exp1, exp2) -> (exp_to_string exp1) ^ " = " ^ (exp_to_string exp2) ^ ";"
  | Output exp -> "output " ^ (exp_to_string exp)
  | IfThen (exp, stms) -> "if (" ^ (exp_to_string exp) ^ ") { ... }"
  | IfThenElse (exp, stms1, stms2) -> "if (" ^ (exp_to_string exp) ^ ") { ... } else { ... }"
  | While (exp, stms) -> "while (" ^ (exp_to_string exp) ^ ") { ... }"
  | LocalDecl ids -> "var " ^ (identifiers_to_string ids) ^ ";"
  | Return exp -> "return " ^ (exp_to_string exp) ^ ";"

let rec stm_to_string stm prefix =
  match stm.stm with
  | VarAssignment (id, exp) ->
    Printf.sprintf "%s%s = %s;" prefix (identifier_to_string id) (exp_to_string exp)
  | PointerAssignment (exp1, exp2) ->
    Printf.sprintf "%s%s = %s;" prefix (exp_to_string exp1) (exp_to_string exp2)
  | Output exp ->
    Printf.sprintf "%soutput %s;" prefix (exp_to_string exp)
  | LocalDecl ids ->
    Printf.sprintf "%svar %s;" prefix (identifiers_to_string ids)
  | Return exp ->
    Printf.sprintf "%sreturn %s;" prefix (exp_to_string exp)
  | IfThen (exp, stms) ->
    Printf.sprintf
      "%sif (%s) {\n%s\n%s}"
      prefix (exp_to_string exp) (stms_to_string stms (prefix ^ "  ")) prefix
  | While (exp, stms) ->
    Printf.sprintf
      "%swhile (%s) {\n%s\n%s}"
      prefix (exp_to_string exp) (stms_to_string stms (prefix ^ "  ")) prefix
  | IfThenElse (exp, stms1, stms2) ->
    Printf.sprintf
      "%sif (%s) {\n%s\n%s} else {\n%s\n%s}"
      prefix (exp_to_string exp) (stms_to_string stms1 (prefix ^ "  ")) prefix (stms_to_string stms2 (prefix ^ "  ")) prefix

and

stms_to_string stms prefix =
  match stms with
  | [] -> ""
  | stm :: [] -> stm_to_string stm prefix
  | stm :: stms' -> Printf.sprintf "%s\n%s" (stm_to_string stm prefix) (stms_to_string stms' prefix)

(**
  * Pretty printer functions:
  *)

let pp_identifier id =
  Printf.printf "%s" id.Ast.identifier

let pp_identifiers ids =
  Printf.printf "%s" (identifiers_to_string ids)

let pp_binop binop =
  Printf.printf "%s" (binop_to_string binop)

let pp_unop unop =
  Printf.printf "%s" (unop_to_string unop)


(**
  * Expressions:
  *)

let pp_exp exp =
  Printf.printf "%s" (exp_to_string exp)

let pp_exps exps =
  Printf.printf "%s" (exps_to_string exps)


(**
  * Statements
  *)


let rec pp_stm stm prefix =
  Printf.printf "%s" (stm_to_string stm prefix)

and

pp_stms stms prefix =
  Printf.printf "%s" (stms_to_string stms prefix)


(**
  * Functions
  *)

let rec pp_function func =
  pp_identifier func.function_name;
  Printf.printf "(";
  pp_identifiers func.function_formals;
  Printf.printf ") {";
  print_newline();
  pp_stms func.function_body "  ";
  print_newline();
  Printf.printf "}";
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
  * Programs:
  *)

let pp_program prog =
  pp_functions prog.program_decl