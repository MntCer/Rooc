/* Ocamlyacc parser for Rooc */

%{
open Ast
%}
/* operator & punctuation */
%token SEMI 
%token ASSIGN PLUS MINUS TIMES DIVIDE LPAREN RPAREN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token LBRACE RBRACE COMMA COLON RARROW DOT
%token <bool> BLIT
%token VAR LET FUN STRUCT IMPL TRAIT
%token INT BOOL FLOAT STR VOID
%token RETURN IF ELSE FOR WHILE 
%token <int> ILIT
%token <string> FLIT SLIT ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [], [], []) }
/*  | decls vdecl { (($2 :: fst $1), snd $1) } */
 | decls fdecl { ($2 :: (get_4_1 $1), get_4_2 $1, get_4_3 $1, get_4_4 $1) }
 | decls tdecl { (get_4_1 $1, $2 :: (get_4_2 $1), get_4_3 $1, get_4_4 $1) }
 | decls idecl { (get_4_1 $1, get_4_2 $1, $2 :: (get_4_3 $1), get_4_4 $1) }
 | decls sdecl { (get_4_1 $1, get_4_2 $1, get_4_3 $1, $2 :: (get_4_4 $1)) } 

fdecl:
    FUN ID LPAREN formals_opt RPAREN RARROW typ LBRACE vdecl_list stmt_list RBRACE SEMI
     { { fd_typ = $7;
	 fd_name = $2;
	 fd_formals = List.rev $4;
	 fd_locals = List.rev $9;
	 fd_body = List.rev $10 } }

fdecl_list:
   \* nothing *\ { [] }
  | fdecl_list fdecl { $2 :: $1 }

fsign: 
   FUN ID LPAREN formals_opt RPAREN RARROW typ SEMI
    { { fs_typ = $7;
        fs_name = $2;
        fs_formals = $4 } }

fsign_list:
   \* nothing *\ { [] }
  | fsign_list fsign { $2 :: $1 }

tdecl: 
  TRAIT ID LBRACE fsign_list RBRACE SEMI
   { { tr_name = $2;
       tr_methods = List.rev $4 } }

idecl:
  IMPL ID FOR ID fdecl_list SEMI
   { { i_name = $2
       i_forstruct = $4
       i_methods = List.rev $5 } }

sdecl: 
  STRUCT ID LBRACE vdecl_list RBRACE SEMI
{ { s_name = $2
       s_fields = List.rev $4 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    ID COLON typ             { [($3,$1)]     }
  | formal_list ID COLON typ { ($4,$2) :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE %prec NOELSE
                                            { If($3, $6, Block([])) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
                                            { If($3, $6, $10)       }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
                                            { For($3, $5, $7, $10)   }
  | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE
                                            { While($3, $6)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	     { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
