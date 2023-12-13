/* Ocamlyacc parser for Rooc */
%{
open Ast
open Util
%}

/* operator & punctuation */
%token SEMI 
%token ASSIGN PLUS MINUS TIMES DIVIDE LPAREN RPAREN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token LBRACE RBRACE COMMA COLON RARROW DOT
%token <bool> BLIT
%token VAR LET FUN STRUCT IMPL TRAIT CONST NEW
%token INT BOOL FLOAT STR MUTREF MUTPTR
// %token LIST
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE SELF
%token <int> ILIT
%token <string> FLIT SLIT ID
%token EOF

%start roc_module
%type <Ast.roc_module> roc_module

%nonassoc LOWEST_PRECEDENCE
%nonassoc RETURN BREAK
%nonassoc ELSE

%right ASSIGN
%left OR
%left AND
%left LT GT LEQ GEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%nonassoc OPERATOR
%nonassoc CALL
%nonassoc FIELD
%nonassoc METHOD_CALL
%nonassoc PATH
%nonassoc HIGHEST_PRECEDENCE

%%

roc_module:
  roc_items EOF {{ 
    rm_items = List.rev $1;
  }}

roc_items:
    /* nothing */ { ([]) }
  | roc_items roc_item { ($2 :: $1) }

roc_item:
    roc_function { FunctionItem($1) }
  | r_struct { StructItem($1) }
  // | %TODO

roc_function:
    FUN ID LPAREN roc_function_params RPAREN RARROW r_type roc_block
    { { 
      rf_name = $2;
      rf_params = Some ($4);
      rf_return_type = $7;
      rf_body = $8 } }

  | FUN ID LPAREN RPAREN RARROW r_type roc_block
      { {
        rf_name = $2;
        rf_params = None;
        rf_return_type = $6;
        rf_body = $7 } }


roc_function_params:
    roc_params | roc_params COMMA { {
      rp_params = List.rev $1; }}

roc_params :
    roc_param  { [$1] }
  | roc_params COMMA roc_param  { $3 :: $1 }

roc_param:
    ID COLON r_type { {
      rv_name = $1;
      rv_type = $3;
      rv_initial_expr = None } }
    
// roc_method_signature:
//     FUN ID LPAREN roc_method_params RPAREN RARROW r_type SEMI
//     { {
//       rms_name = $2;
//       rms_params = $4;
//       rms_return_type = $7 } }

// roc_method:
//     FUN ID LPAREN roc_method_params RPAREN RARROW r_type roc_block
//     { {
//       rm_name = $2;
//       rm_params = $4;
//       rm_return_type = $7;
//       rm_body = $8 } }
    
// roc_method_params:
//     SELF { None }
//   | SELF COMMA roc_params  { Some ({
//     rp_params=List.rev $3}) }

r_struct:
    STRUCT ID LPAREN struct_fields RPAREN 
    { {
      rs_name = $2 ;
      rs_fields = List.rev $4; } }

struct_fields:
    struct_field { [$1] }
  | struct_fields struct_field { $2 :: $1 }

struct_field:
    ID COLON r_type SEMI 
    { {
      rsf_name = $1;
      rsf_type = $3; } }

roc_statement:
  | roc_expr_stmt { $1 }
  | roc_decl_stmt { $1 }
  | roc_block {STMT_block ($1)}
  | roc_loop_stmt {$1}
  | roc_if_stmt {$1}
  | roc_continue_stmt {$1}
  | roc_break_stmt {$1}
  | roc_return_stmt {$1}

roc_decl_stmt:
    VAR ID COLON r_type ASSIGN roc_expr SEMI
    {
      Roc_var_decl_stmt({
        rv_name = $2;
        rv_type = $4;
        rv_initial_expr = Some ($6) }) }
  | VAR ID COLON r_type SEMI
    {
      Roc_var_decl_stmt({
        rv_name = $2;
        rv_type = $4;
        rv_initial_expr = None }) }
  | LET ID COLON r_type ASSIGN roc_expr SEMI
    {
      Roc_let_decl_stmt({
        rv_name = $2;
        rv_type = $4;
        rv_initial_expr = Some ($6) }) }
  | LET ID COLON r_type SEMI
    {
      Roc_let_decl_stmt({
        rv_name = $2;
        rv_type = $4;
        rv_initial_expr = None }) }

roc_expr_stmt:
    roc_expr SEMI { Roc_expr_stmt($1) }

expr_empty:
    /* empty */ { EXPR_null }

expr_nonempty:
  | roc_call_expr %prec CALL {$1}
  | roc_operator_expr %prec OPERATOR {$1}
  | roc_literal_expr {$1}
  | roc_grouped_expr {$1}
  | expr_field_access %prec FIELD {$1}
  | expr_path {$1}
  // | expr_box_init {$1}

roc_expr:
  | expr_empty %prec LOWEST_PRECEDENCE { $1 }
  | expr_nonempty %prec HIGHEST_PRECEDENCE { $1 }

roc_literal_expr:
  | SLIT { Roc_string_literal($1) }
  | ILIT { Roc_int_literal($1) }
  | FLIT 
    { (match safe_float_of_string $1 with
      | Some f -> Roc_float_literal f
      | None -> bug "have syntax error on float literal.") } 
  | BLIT { Roc_bool_literal($1) }

roc_operator_expr:
  | roc_unary_expr {$1}
  | roc_arith_expr {$1}
  | roc_logical_expr {$1}
  | roc_comparison_expr {$1}
  | roc_assignment_expr {$1}

roc_unary_expr:
  | MINUS expr_nonempty { Roc_unary_expr(Neg, $2) }
  | NOT expr_nonempty { Roc_unary_expr(Not, $2) }

roc_arith_expr:
    expr_nonempty PLUS expr_nonempty { Roc_arith_expr(Add, $1, $3) }
  | expr_nonempty MINUS expr_nonempty { Roc_arith_expr(Sub, $1, $3) }
  | expr_nonempty TIMES expr_nonempty { Roc_arith_expr(Mult, $1, $3) }
  | expr_nonempty DIVIDE expr_nonempty { Roc_arith_expr(Div, $1, $3) }

roc_logical_expr:
  | expr_nonempty AND expr_nonempty { Roc_logical_expr(And, $1, $3) }
  | expr_nonempty OR expr_nonempty { Roc_logical_expr(Or, $1, $3) }

roc_comparison_expr:
    expr_nonempty EQ expr_nonempty { Roc_comparison_expr(Equal, $1, $3) }
  | expr_nonempty NEQ expr_nonempty { Roc_comparison_expr(Neq, $1, $3) }
  | expr_nonempty LT expr_nonempty { Roc_comparison_expr(Less, $1, $3) }
  | expr_nonempty LEQ expr_nonempty { Roc_comparison_expr(Leq, $1, $3) }
  | expr_nonempty GT expr_nonempty { Roc_comparison_expr(Greater, $1, $3) }
  | expr_nonempty GEQ expr_nonempty { Roc_comparison_expr(Geq, $1, $3) }

roc_assignment_expr:
    expr_nonempty ASSIGN expr_nonempty { Roc_assignment_expr($1, $3) }

roc_grouped_expr:
    LPAREN expr_nonempty RPAREN { Roc_grouped_expr($2) }

roc_call_expr:    
    ID LPAREN roc_call_params RPAREN { Roc_call_expr($1, $3) }
  | ID LPAREN RPAREN { Roc_call_expr($1, []) }

roc_call_params:
    roc_call_params_no_comma optional_comma { List.rev $1 }

roc_call_params_no_comma:
    expr_nonempty { [$1] }
  | roc_call_params_no_comma COMMA expr_nonempty { $3 :: $1 }

optional_comma:
    /* empty */ { () }
  | COMMA { () }

expr_field_access:
    // #TODO: it's a simplified version, but the complele version has some unsolvable problems now.
    ID DOT ID { EXPR_field_access ($1, $3) } 

// expr_box_init:
//     BOX COLON COLON NEW LPAREN expr_nonempty RPAREN 
//       { EXPR_box_init ($6) }

expr_path:
    ID { EXPR_path ($1) }

roc_continue_stmt:
    CONTINUE SEMI { Roc_continue_stmt }

roc_break_stmt:
    BREAK SEMI { Roc_break_stmt }

roc_return_stmt:
    RETURN roc_expr SEMI { Roc_return_stmt($2) }


roc_block:
    LBRACE roc_statements RBRACE {{
      rb_stmts = List.rev $2;
    }}
  | LBRACE RBRACE {{
      rb_stmts = [];
    }}

roc_statements:
    roc_statement { [$1] }
  | roc_statements roc_statement { $2 :: $1 }

roc_if_stmt:
    IF LPAREN expr_nonempty roc_block ELSE roc_block { Roc_if_stmt($3, $4, $6) }

roc_loop_stmt:
  | roc_while_stmt { $1 }


roc_while_stmt:
    WHILE LPAREN expr_nonempty RPAREN roc_block { Roc_while_stmt($3, $5) }


r_type:
  | INT   { T_int    }
  | FLOAT { T_float  }
  | BOOL  { T_bool   }
  | STR   { T_string }
  | LPAREN RPAREN { T_unit } // () serves as void in Rooc
  | MUTREF r_type_expr       { T_mutref ($2) }
  | MUTPTR r_type_expr       { T_mutptr ($2) }
  // | BOX LT r_type_expr GT    { T_box ($3) } 

r_type_expr:
  | r_type                  { R_type_expr ($1) }
  | ID                      { R_user_defined_type ($1) }

