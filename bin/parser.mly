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
%token VAR FUN STRUCT IMPL TRAIT
%token INT BOOL FLOAT STR VOID
// %token LET LIST
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE
%token <int> ILIT
%token <string> FLIT SLIT ID
%token EOF

%start roc_module
%type <Ast.roc_module> roc_module

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

roc_module:
  roc_items EOF { $1 }

roc_items:
    /* nothing */ { ([]) }
  | roc_items roc_item { ($2 :: $1) }

roc_item:
    roc_function { FunctionItem($1) }
  // | %TODO

roc_function:
    roc_function_signature roc_block_expression SEMI
      {{ 
        rfun_signature : $1;
        rfun_body : $2 }}

roc_function_signature:
    FUN ID LPAREN roc_function_params RPAREN RARROW type
      { { 
        rfs_name = $2;
        rfs_params = $4;
        rfs_return_type = $7 } }
  | FUN ID LPAREN RPAREN RARROW type
      { { 
        rfs_name = $2;
        rfs_params = None;
        rfs_return_type = $6 } }
    
roc_function_params:
    roc_self_param COMMA roc_ns_params | roc_self_param COMMA roc_ns_params COMMA 
      {
        rfp_self_param = $1;
        rfp_params = $3 }
  | roc_self_param | roc_self_param COMMA 
      {
        rfp_self_param = $1;
        rfp_params = [] }
  | roc_ns_params | roc_ns_params COMMA 
      {
        rfp_self_param = false;
        rfp_params = $1 }
    
roc_self_param:
    SELF  { true}

roc_ns_params:
  // No need to handle "empty list" case here, because it is handled by the roc_function_params rule
    roc_ns_param { [$1] }
  | roc_ns_params COMMA roc_ns_param { $3 :: $1 }
    
roc_ns_param:
    ID COLON type { {
      rv_name = $1;
      rv_type = $3;
      rv_initial_value = None } }

type:
    INT   { T_int    }
  | FLOAT { T_float  }
  | BOOL  { T_bool   }
  | STR   { T_string }
  | VOID  { T_void   }
  | LPAREN RPAREN { T_unit }

// primitive_typ:
//     INT   { Int    }
//   | BOOL  { Bool   }
//   | FLOAT { Float  }
//   | STR   { String }
//   | VOID  { Void   }

// generic_typ:
//     LIST LPAREN typ RPAREN { List($3) }

// typ:
//     primitive_typ { Primitive($1)}
//   | generic_typ     { Generic($1) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   VAR ID COLON typ SEMI { ($4, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE
                                            { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt
                                            { If($3, $5, $7)       }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt
                                            { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    ILIT             { Literal($1)            }
  | FLIT	           { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | SLIT             { Sliteral($1)           }
  | ID DOT ID        { Member($1, $3)         }
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
