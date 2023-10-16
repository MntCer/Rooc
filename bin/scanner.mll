(* Ocamllex scanner for Rooc *)

{ open Parser }

let digit  = ['0' - '9']
let digits = digit+
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
(* Comments *)
| "/*"     { comment lexbuf }
(* Whitespace *)
| [' ' '\t' '\r' '\n'] { token lexbuf } 
(* Semicolon *)
| ';'      { SEMI }
(* Operator & punctuation *)
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
(* Keywords *)
| "var"    { VAR }
| "fun"    { FUN }
| "let"    { LET }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
