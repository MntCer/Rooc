(* Ocamllex scanner for Rooc *)

{ open Parser 
}
(* { 
  (* for test *)
type token = 
  ID of string
| SEMI 
| ASSIGN | PLUS | MINUS | TIMES | DIVIDE | LPAREN | RPAREN
| EQ | NEQ | LT | LEQ | GT | GEQ | AND | OR | NOT
| LBRACE | RBRACE | COMMA | COLON | RARROW | DOT
| BLIT of bool
| VAR | LET | FUN | STRUCT | IMPL | TRAIT
| INT | BOOL | FLOAT | STR
| RETURN | IF | ELSE | FOR | WHILE 
| ILIT of int
| FLIT of string
| SLIT of string
| EOF
} *)

let digit  = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
(* comments *)
| "//" [^ '\n' '\r']* ('\r' '\n' | '\n'| eof ) { token lexbuf } 
| "/*"                                   { comment lexbuf }
(* whitespace *)
| [' ' '\t' '\r' '\n'] { token lexbuf } 
(* semicolon *)
| ';'      { SEMI }
(* operator & punctuation *)
| '='      { ASSIGN }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '('      { LPAREN }
| ')'      { RPAREN }
(**)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
(**)
| '{'      { LBRACE }
| '}'      { RBRACE }
| ','      { COMMA }
| ':'      { COLON }
| "->"     { RARROW }
| "."      { DOT }
(* keywords *)
| "const"  { CONST }
| "var"    { VAR }
| "let"    { LET }
| "fun"    { FUN }
| "struct" { STRUCT }
| "impl"   { IMPL }
| "trait"  { TRAIT }
| "self"   { SELF }
(* *)
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "str"    { STR}
(* | "list"   { LIST } *)
(* *)
| "return" { RETURN }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
(* literals *)
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digit+ as int_str { ILIT(int_of_string int_str) }
| digit+ '.' digit* as float_str { FLIT(float_str) }
| '"' { string_processor lexbuf }
(* identifier, should not have higher precedence than keywords. *)
| letter ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id_str { ID(id_str) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

(* recognize string literal in double quotation marks *)
and string_processor = parse
[^'"']* as str_str '"' { SLIT(str_str)}
| _ as char { raise (Failure("illegal character " ^ Char.escaped char ^ "in string literal")) }
