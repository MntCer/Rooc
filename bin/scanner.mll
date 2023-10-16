(* Ocamllex scanner for Rooc *)

{ open Parser }

let digit  = ['0' - '9']
let digits = digit+
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
(* comments *)
| "//" [^ '\n' '\r']* ('\r' '\n' | '\n') { token lexbuf } 
| "/*"                                   { comment lexbuf }
(* whitespace *)
| [' ' '\t' '\r' '\n'] { token lexbuf } 
(* identifier *)
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id_str { ID(id_str) }
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
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ','      { COMMA }
| ':'      { COLON }
(* keywords *)
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "var"    { VAR }
| "let"    { LET }
| "fun"    { FUN }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "str"    { STR}
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "void"   { VOID }
(* literals *)
| digits as int_str { ILIT(int_of_string int_str) }
| digits '.'  digit* as float_str { FLIT(float_str) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
