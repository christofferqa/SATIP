(* lexer.mll -*- tuareg -*- *)
{
  open Parser
  
  let get = Lexing.lexeme
    
  let keyword_table = Hashtbl.create 53

  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      ["else",   ELSE;
       "if",     IF;
       "input",  INPUT;
       "malloc", MALLOC;
       "null",   NULL;
       "output", OUTPUT;
       "return", RETURN;
       "var",    VAR;
       "while",  WHILE ]

  let tok_as_string t = match t with
	  | EOF -> "eof"
		
    | ELSE -> "else"
    | IF -> "if"
    | INPUT -> "input"
    | MALLOC -> "malloc"
    | NULL -> "null"
    | OUTPUT -> "output"
    | RETURN -> "return"
    | VAR -> "var"
    | WHILE -> "while"
      
    (* Delimiters *)
    | L_PAREN -> "("
    | R_PAREN -> ")"
    | L_BRACE -> "{"
    | R_BRACE -> "}"
    | SEMICOLON -> ";"
    | COMMA -> ","
      
    (* Assignment and logic *)
    | ASSIGN -> "="
      
    (* Comparison *)
    | GT -> ">"
    | EQ -> "=="

    (* Arithmetic *)
    | PLUS -> "+"
    | MINUS -> "-"
    | STAR -> "*"
    | DIV -> "/"

    (* Pointer arithmetic *)
		| AMP -> "&"
		
    (* Literals and identifiers *)
    | INTEGER_LITERAL i -> "INTEGER_LITERAL" ^ i
    | IDENTIFIER s -> s (*"IDENTIFIER("^s^")"*)
}

(*******************************************************************
 * Helpers                                                         *
 *******************************************************************)

let latin1_input_character = ['\000'- '\255']
let ht = '\t'
let lf = '\n'
let ff = '\012'
let cr = '\r'
let sp = ' '

let line_terminator = lf | cr | cr lf 
let input_character = ['\000'-'\255'] # ['\r' '\n'] (* # (lf | cr) *)

let not_star_not_newline = (['\000'-'\255'] # ['\r''\n''*'])
let not_star_not_slash_not_newline = (['\000'-'\255'] # ['\r''\n''*''/'])

let digit = ['0'-'9']
let non_zero_digit = ['1'-'9']
let octal_digit = ['0'-'7']
let zero_to_three = ['0'-'3']

let decimal_numeral = '0' | non_zero_digit digit*

let latin1_letter =
       ['A'-'Z'] | ['a'-'z'] | ['\170'-'\170'] | ['\181'-'\181'] |
       ['\186'-'\186'] | ['\192'-'\214'] | ['\216'-'\246'] | ['\248'-'\255']

let tip_letter = latin1_letter | '$' | '_'
let tip_letter_or_digit = latin1_letter | digit | '$' | '_'

let octal_escape = '\\' (octal_digit octal_digit? | zero_to_three octal_digit octal_digit)
let escape_sequence = "\\b" | "\\t" | "\\n" | "\\f" | "\\r" | "\\" '"' | "\\" "'" | "\\\\" | octal_escape
let single_character = (['\000'-'\255'] # ['\r''\n'''''\\']) | escape_sequence
let string_character = (['\000'-'\255'] # ['\r''\n''"''\\']) | escape_sequence

(*******************************************************************
 * Tokens                                                          *
 *******************************************************************)

rule token = parse
  | eof    { EOF }

(* Whitespace *)
  | (sp | ht | ff )                 { token lexbuf }  (* white_space *)
  | line_terminator                 { Lexing.new_line lexbuf; token lexbuf }
  | "/*" line_terminator            { inside_comment false lexbuf }
  | "/*" not_star_not_newline       { inside_comment false lexbuf }
  | "/**"                           { inside_comment true lexbuf }
  | "//" input_character*
                                    { token lexbuf } (* end_of_line_comment *)
  | "//" input_character* line_terminator
                                    { Lexing.new_line lexbuf; token lexbuf } (* end_of_line_comment *)

(* Delimiters *)
  | '('             { L_PAREN }
  | ')'             { R_PAREN }
  | '{'             { L_BRACE }
  | '}'             { R_BRACE }
  | ';'             { SEMICOLON }
  | ','             { COMMA }

(* Assignment *)
  | '='             { ASSIGN }

(* Comparison *)
  | '>'             { GT }
  | "=="            { EQ }

(* Arithmetic *)
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { DIV }

(* Pointer arithmetic *)
	| "&"							{ AMP }

(* Literals and identifiers *)
  | decimal_numeral as i { INTEGER_LITERAL i }
  | tip_letter tip_letter_or_digit* as id { try Hashtbl.find keyword_table id with Not_found -> IDENTIFIER id }

(* Two-state finite automata for recognizing end-of-comment
    bool laststar represents the state of the automata      *)
and inside_comment laststar = parse
  | '/'                            { if laststar
                                     then token lexbuf
                                     else inside_comment false lexbuf }
  | '*'                            { inside_comment true lexbuf }
  | line_terminator                { Lexing.new_line lexbuf; inside_comment false lexbuf }
  | not_star_not_slash_not_newline { inside_comment false lexbuf }