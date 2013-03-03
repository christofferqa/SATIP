(**
  * Pretty printer for the AST.
  *)

open Printf
open Ast


(**
  * To string functions.
  *)

let identifier_to_string (id: Ast.identifier) =
  id.Ast.identifier

let rec identifiers_to_string (ids: Ast.identifier list): string =
  match ids with
  | [] -> ""
  | id :: [] -> identifier_to_string id
  | id :: ids' -> identifier_to_string id ^ ", " ^ (identifiers_to_string ids')

let unop_to_string (unop: Ast.unop): string =
  match unop with
  | Pointer -> "&"
  | Dereference -> "*"

let binop_to_string (binop: Ast.binop): string =
  match binop with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Gt -> ">"
  | Eq -> "=="

let rec exp_to_string (exp: Ast.exp): string =
  match exp.exp with
  | IntConst c -> c
  | Identifier id -> Ast.i2s id
  | Binop (exp1, binop, exp2) -> (exp_to_string exp1) ^ (binop_to_string binop) ^ (exp_to_string exp2)
  | Unop (unop, exp') -> (unop_to_string unop) ^ (exp_to_string exp')
  | Input -> "input"
  | Malloc -> "malloc"
  | Null -> "null"
  | FunctionInvocation (id, exps) -> (Ast.i2s id) ^ "(" ^ (exps_to_string exps) ^ ")"
  | PointerInvocation (exp', exps) -> "(" ^ (exp_to_string exp') ^ ")(" ^ (exps_to_string exps) ^ ")"

and

exps_to_string (exps: Ast.exp list): string =
  match exps with
  | [] -> ""
  | exp :: [] -> exp_to_string exp
  | exp :: exps' -> (exp_to_string exp) ^ ", " ^ (exps_to_string exps')

let rec stm_to_string (stm: Ast.stm) =
  match stm.stm with
  | VarAssignment (id, exp) -> (identifier_to_string id) ^ " = " ^ (exp_to_string exp) ^ ";"
  | PointerAssignment (exp1, exp2) -> (exp_to_string exp1) ^ " = " ^ (exp_to_string exp2) ^ ";"
  | Output exp -> "output " ^ (exp_to_string exp)
  | IfThen (exp, stms) -> "if (" ^ (exp_to_string exp) ^ ") { ... }"
  | IfThenElse (exp, stms1, stms2) -> "if (" ^ (exp_to_string exp) ^ ") { ... } else { ... }"
  | While (exp, stms) -> "while (" ^ (exp_to_string exp) ^ ") { ... }"
  | LocalDecl ids -> "var " ^ (identifiers_to_string ids) ^ ";"
  | Return exp -> "return " ^ (exp_to_string exp) ^ ";"


(**
  * Misc.
  *)

let pp_identifier (id: Ast.identifier) =
  printf "%s" id.Ast.identifier

let pp_identifiers (ids: Ast.identifier list) =
  printf "%s" (identifiers_to_string ids)

let pp_binop (binop: Ast.binop) =
  printf "%s" (binop_to_string binop)

let pp_unop (unop: Ast.unop) =
  printf "%s" (unop_to_string unop)


(**
  * Expressions
  *)

let pp_exp (exp: Ast.exp) =
  printf "%s" (exp_to_string exp)

let pp_exps (exps: Ast.exp list) =
  printf "%s" (exps_to_string exps)


(**
  * Statements
  *)


let rec pp_stm (stm: Ast.stm) (prefix: string) =
  match stm.stm with
  | VarAssignment (id, exp) ->
    printf "%s" prefix;
    pp_identifier id;
    printf " = ";
    pp_exp exp;
    printf ";"
  | PointerAssignment (exp1, exp2) ->
    printf "%s" prefix;
    pp_exp exp1;
    printf " = ";
    pp_exp exp2;
    printf ";"
  | Output exp ->
    printf "%soutput " prefix;
    pp_exp exp
  | IfThen (exp, stms) ->
    printf "%sif (" prefix;
    pp_exp exp;
    printf ") {";
    print_newline();
    pp_stms stms (prefix ^ "  ");
    print_newline();
    printf "%s}" prefix
  | IfThenElse (exp, stms1, stms2) ->
    printf "%sif (" prefix;
    pp_exp exp;
    printf ") {";
    print_newline();
    pp_stms stms1 (prefix ^ "  ");
    print_newline();
    printf "%s} else {" prefix;
    print_newline();
    pp_stms stms2 (prefix ^ "  ");
    print_newline();
    printf "%s}" prefix
  | While (exp, stms) ->
    printf "%swhile (" prefix;
    pp_exp exp;
    printf ") {";
    print_newline();
    pp_stms stms (prefix ^ "  ");
    print_newline();
    printf "%s}" prefix
  | LocalDecl ids ->
    printf "%svar " prefix;
    pp_identifiers ids;
    printf ";"
  | Return exp ->
    printf "%sreturn " prefix;
    pp_exp exp;
    printf ";"

and

pp_stms (stms: Ast.stm list) (prefix: string) =
  match stms with
  | [] -> ()
  | stm :: [] ->
    pp_stm stm prefix
  | stm :: stms' ->
    pp_stm stm prefix;
    print_newline();
    pp_stms stms' prefix


(**
  * Functions
  *)

let rec pp_function (func: Ast.function_decl_desc) =
  pp_identifier func.function_name;
  printf "(";
  pp_identifiers func.function_formals;
  printf ") {";
  print_newline();
  pp_stms func.function_body "  ";
  print_newline();
  printf "}";
  print_newline()

and

pp_functions (funcs: Ast.function_decl list) =
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

let rec pp_program (prog: Ast.program) =
  pp_functions prog.program_decl