/* File parser.mly */
%{
  let make_identifier pos i =
    { Ast.identifier_pos = pos;
		  Ast.identifier = i }

  let make_exp pos e =
    { Ast.exp_pos = pos;
		  Ast.exp = e }
	
  let make_stm pos s =
    { Ast.stm_pos = pos;
		  Ast.stm = s }

  let make_function pos decl =
    { Ast.function_decl_pos = pos;
		  Ast.function_decl = decl }

  let make_function_decl (name, formals, body) =
    { Ast.function_name    = name;
      Ast.function_formals = formals;
      Ast.function_body    = body }

  let make_program pos decl =
    let file_name = pos.Lexing.pos_fname in
    { Ast.program_name = file_name;
      Ast.program_decl = decl }
%}

%token EOF

/* Keywords */

%token ELSE
%token IF
%token INPUT
%token MALLOC
%token NULL
%token OUTPUT
%token RETURN
%token VAR
%token WHILE

/* Delimiters */
%token L_PAREN R_PAREN
%token L_BRACE R_BRACE
%token SEMICOLON
%token COMMA

/* Assignment */
%token ASSIGN

/* Comparison */
%token GT EQ

/* Arithmetic */
%token PLUS MINUS STAR DIV

/* Pointer arithmetic */
%token AMP

/* Literals and identifiers */
%token <string>INTEGER_LITERAL
%token <string>IDENTIFIER

%start <Ast.program> goal

%%

goal
  : function_declaration* EOF
    { make_program $startpos $1 }
  ;

/* Misc. lists */

formal_parameter_list
  :
    { [] }
  | formal_parameter_list_nonempty
    { List.rev $1 }
  ;

formal_parameter_list_nonempty
  : formal_parameter
    { [$1] }
  | formal_parameter_list_nonempty COMMA formal_parameter
    { $3 :: $1 }
  ;

formal_parameter
  : identifier
    { $1 }
  ;

argument_list
  :
     { [] }
  |  argument_list_nonempty
     { List.rev $1 }
  ;

argument_list_nonempty
  :  argument
     { [$1] }
  |  argument_list_nonempty COMMA argument
     { $3 :: $1 }
  ;

argument
  : expression
	  { $1 }
	;

/* ********** Function declarations *********** */

function_declaration
  : identifier function_params function_body
    { make_function $startpos (make_function_decl ($1, $2, $3)) }
  ;

function_params
  : L_PAREN formal_parameter_list R_PAREN
    { $2 }
  ;

function_body
  : L_BRACE statement_list return_statement R_BRACE
    { $2 @ [$3] }
  ;

return_statement
  : RETURN expression SEMICOLON
	  { make_stm $startpos (Ast.Return($2)) }
	;

/* ********** Blocks and statements ********** */

statement_list
  : 
    { [] }
  | statement_list_nonempty
    { List.rev $1 }
  ;

statement_list_nonempty
  : statement
    { [$1] }
	| statement_list_nonempty statement
    { $2 :: $1 }
  ;

statement
  : statement_without_trailing_substatement
    { $1 }
  | if_then_statement
    { $1 }
  | if_then_else_statement
    { $1 }
  | while_statement
    { $1 }
  ;

/*
statement_no_short_if
  : statement_without_trailing_substatement
    { $1 }
  | if_then_else_statement_no_short_if
    { $1 }
  | while_statement_no_short_if
    { $1 }
  ;
*/

statement_without_trailing_substatement
  : assignment SEMICOLON
    { $1 }
  | output_statement SEMICOLON
    { $1 }
	| local_variable_declarations_statement SEMICOLON
	  { $1 }
  ;

assignment
  : identifier ASSIGN expression
    { make_stm $startpos (Ast.VarAssignment($1,$3)) }
  | STAR expression ASSIGN expression
    { make_stm $startpos (Ast.PointerAssignment($2,$4)) }
  ;

output_statement
  : OUTPUT expression
    { make_stm $startpos (Ast.Output($2)) }
  ;

local_variable_declarations_statement
  : VAR local_variable_declaration_list
	  { make_stm $startpos (Ast.LocalDecl($2)) }
	;

local_variable_declaration_list
  : local_variable_declaration_list_nonempty
    { List.rev $1 }
  ;
	
local_variable_declaration_list_nonempty
  : identifier
    { [$1] }
  | local_variable_declaration_list_nonempty COMMA identifier
    { $3 :: $1 }
  ;

block_statement
  : L_BRACE statement_list R_BRACE
	  { $2 }
	;

if_then_statement
  : IF L_PAREN expression R_PAREN block_statement
    { make_stm $startpos (Ast.IfThen($3,$5)) }
  ;

if_then_else_statement
  : IF L_PAREN expression R_PAREN block_statement ELSE block_statement
    { make_stm $startpos (Ast.IfThenElse($3,$5,$7)) }
  ;

/*
if_then_else_statement_no_short_if
  : IF L_PAREN expression R_PAREN statement_no_short_if ELSE statement
    { make_stm $startpos (Ast.IfThenElse($3,$5,$7)) }
  ;
*/

while_statement
  : WHILE L_PAREN expression R_PAREN block_statement
    { make_stm $startpos (Ast.While($3,$5)) }
  ;
	
/*
while_statement_no_short_if
  : WHILE L_PAREN expression R_PAREN statement_no_short_if
    { make_stm $startpos (Ast.While($3,$5)) }
  ;
*/

/* ********** Expressions ********** */

expression
  : equality_expression
    { $1 }
  ;

equality_expression
  : relational_expression
    { $1 }
  | equality_expression EQ relational_expression
    { make_exp $startpos (Ast.Binop($1,Ast.Eq,$3)) }
  ;

relational_expression
  : additive_expression
    { $1 }
  | relational_expression GT additive_expression
    { make_exp $startpos (Ast.Binop($1,Ast.Gt,$3)) }
  ;

additive_expression
  : multiplicative_expression
    { $1 }
  | additive_expression PLUS multiplicative_expression
    { make_exp $startpos (Ast.Binop($1,Ast.Plus,$3)) }
  | additive_expression MINUS multiplicative_expression
    { make_exp $startpos (Ast.Binop($1,Ast.Minus,$3)) }
  ;

multiplicative_expression
  : pointer_expression
    { $1 }
  | multiplicative_expression STAR pointer_expression
    { make_exp $startpos (Ast.Binop($1,Ast.Times,$3)) }
  | multiplicative_expression DIV pointer_expression
    { make_exp $startpos (Ast.Binop($1,Ast.Divide,$3)) }
  ;

pointer_expression
  : primary_expression
    { $1 }
	| AMP identifier
	  { make_exp $startpos (Ast.Unop(Ast.Pointer, make_exp $startpos (Ast.Var($2)))) }
	| STAR pointer_expression
	  { make_exp $startpos (Ast.Unop(Ast.Dereference, $2)) }
  ;

primary_expression
  :  INTEGER_LITERAL
     { make_exp $startpos (Ast.IntConst $1) }
  |  NULL
     { make_exp $startpos (Ast.Null) }
	| identifier
     { make_exp $startpos (Ast.Var($1)) }
	|	INPUT
     { make_exp $startpos (Ast.Input) }
	| MALLOC
     { make_exp $startpos (Ast.Malloc) }
  |  L_PAREN expression R_PAREN
     { $2 }
  |  L_PAREN expression R_PAREN function_arguments
     { make_exp $startpos (Ast.PointerInvocation($2, $4)) }
  |  identifier function_arguments
     { make_exp $startpos (Ast.FunctionInvocation($1, $2)) }
  ;

identifier
  : IDENTIFIER
	  {make_identifier $startpos $1}
	;

function_arguments
	: L_PAREN argument_list R_PAREN
	  { $2 }
	;