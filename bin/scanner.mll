
(**
  Ocamllex scanner for Rooc

  Authors: Yuanfei, Mona
*)

{ open Parser 

let unescape str =
  let buff = Buffer.create (String.length str) in
  let rec aux i =
    if i >= String.length str then Buffer.contents buff
    else
      match str.[i] with
      | '\\' when i+1 < String.length str -> begin
          match str.[i+1] with
          | 'n' -> Buffer.add_char buff '\n'; aux (i+2)
          | 't' -> Buffer.add_char buff '\t'; aux (i+2)
          | 'r' -> Buffer.add_char buff '\r'; aux (i+2)
          | _ -> Buffer.add_char buff '\\'; Buffer.add_char buff str.[i+1]; aux (i+2)  (* Keep unknown sequences *)
        end
      | c -> Buffer.add_char buff c; aux (i+1)
  in
  aux 0
}



let digit  = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']

(* recognize string literal in double quotation marks *)

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
| "null"   { NULL }
| "return" { RETURN }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
(* type *)
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "str"    { STR}
(* | "list"   { LIST } *)
(* literals *)
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digit+ as int_str { ILIT(int_of_string int_str) }
| digit+ '.' digit* as float_str { FLIT(float_str) }
| '"' { string_processor "" lexbuf }
(* identifier, should not have higher precedence than keywords. *)
| letter ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id_str { ID(id_str) }

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and string_processor acc = parse
  | '"' { SLIT(unescape acc) }
  | '\\' ['n' 't' 'r'] as esc_seq { string_processor (acc ^ esc_seq) lexbuf }
  | [^ '\\' '"']+ as str_str { string_processor (acc ^ str_str) lexbuf }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char ^ " in string literal")) }
