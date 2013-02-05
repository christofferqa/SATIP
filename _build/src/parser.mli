exception Error

type token = 
  | WHILE
  | VAR
  | STAR
  | SEMICOLON
  | R_PAREN
  | R_BRACE
  | RETURN
  | PLUS
  | OUTPUT
  | NULL
  | MINUS
  | MALLOC
  | L_PAREN
  | L_BRACE
  | INTEGER_LITERAL of (string)
  | INPUT
  | IF
  | IDENTIFIER of (string)
  | GT
  | EQ
  | EOF
  | ELSE
  | DIV
  | COMMA
  | ASSIGN
  | AMP


val goal: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)