(**
  * AST type produced by the parser.
  *)

type identifier = { identifier_pos : Lexing.position; identifier : string; identifier_id: int }

(**
  * Operators
  *)

type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Eq

type unop =
  | Pointer
  | Dereference


(**
  * Expressions
  *)

type exp = { exp_pos: Lexing.position; exp: exp_desc; exp_id: int }
and exp_desc =
  | IntConst of string
  | Identifier of identifier
  | Binop of exp * binop * exp
  | Unop of unop * exp
  | Input
  | Malloc
  | Null
  | FunctionInvocation of identifier * exp list
  | PointerInvocation of exp * exp list


(**
  * Statements
  *)

type stm = { stm_pos: Lexing.position; stm: stm_desc; stm_id: int }
and stm_desc =
  | VarAssignment of identifier * exp
  | PointerAssignment of exp * exp
  | Output of exp
  | IfThen of exp * stm list
  | IfThenElse of exp * stm list * stm list
  | While of exp * stm list
  | LocalDecl of identifier list
  | Return of exp


(**
  * Functions
  *)

type function_decl = { function_decl_pos: Lexing.position; function_decl: function_decl_desc }
and function_decl_desc
  = { function_name    : identifier;
      function_formals : identifier list;
      function_body    : stm list }


(**
  * Programs
  *)

type program
  = { program_name : string;
      program_decl : function_decl list }


(**
  * Convenient helper functions
  *)


let i2p (id: identifier): Lexing.position = id.identifier_pos
let i2s (id: identifier): string = id.identifier

let i2exp (id: identifier): exp = { exp_pos = (i2p id); exp = Identifier id; exp_id = id.identifier_id }

let is_identical_identifiers (exp1: exp) (exp2: exp): bool =
  match exp1.exp, exp2.exp with
  | Identifier id1, Identifier id2 -> (i2s id1) = (i2s id2)
  | _, _ -> false

let expressions_equal (exp1: exp) (exp2: exp): bool =
  exp1.exp = exp2.exp

(* Returns true if exp1 contains exp_desc *)
let rec exp_contains (exp1: exp) (exp_desc: exp_desc): bool =
  if exp1.exp = exp_desc
  then true
  else
    match exp1.exp with
    | Binop (exp', _, exp'') -> exp_contains exp' exp_desc || exp_contains exp'' exp_desc
    | Unop (_, exp') -> exp_contains exp' exp_desc
    | FunctionInvocation (id, exps) ->
      List.fold_left
        (fun acc exp' -> (exp_contains exp' exp_desc) || acc)
        false exps
    | PointerInvocation (exp', exps) ->
      List.fold_left
        (fun acc exp' -> (exp_contains exp' exp_desc) || acc)
        (exp_contains exp' exp_desc) exps
    | _ -> false