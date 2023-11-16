
open Ast

type s_type =
    ST_int
  | ST_float
  | ST_string
  | ST_bool
  | ST_void
  | ST_unit

type s_expr = 
    S_string_literal of string
  | S_int_literal of int
  | S_float_literal of string
  | S_bool_literal of bool

  | S_unary_expr of unary_op * s_expr
  | S_arith_logical_expr of arith_logical_op * s_expr * s_expr
  | S_comparison_expr of comparison_op * s_expr * s_expr
  | S_assign_expr of s_expr * s_expr
  | S_call_expr of s_call_expr
  | S_grouped_expr of s_expr

  | S_return_expr of s_expr

  | S_break_expr
  | S_continue_expr

  | S_null_expr


  
and s_call_expr ={
  (* function which be called needs to be declared before *)
  sce_function_called: s_function;
  sce_param: s_expr list;
}

and s_for_expr = {
  sfe_init : s_expr;
  sfe_condition : s_expr;
  sfe_update : s_expr;
  sfe_body : s_block_expr;
}

and s_while = {
  sw_condition : s_expr;
  sw_body : s_expr;
}

and s_if_expr = {
  sie_condition: s_expr ;
  sie_true_branch : s_block_expr;
  sie_false_branch : s_block_expr;
}

and s_variable =
  {
    sv_name: string;
    sv_type: s_type;
    sv_initial_value : s_expr option;
  }

and s_stmt =
    TODO of string

and s_block_expr = {
  sb_stmts : s_stmt list;
  sb_scope : s_symbol_table;
}

and s_function_param = {
  sfp_name : string;
  sfp_type : s_type;
}

and s_function = {
  sf_name : string;
  sfp_self: bool;
  sf_params : s_function_param list;
  sf_return_type : s_type;
  sf_body : s_block_expr;
}

type s_symbol_table_entry =
    FuncEntry of s_function
  | VarEntry of s_variable

type s_symbol_table = {
  sst_parent : s_symbol_table option;
  sst_symbols: (string, s_symbol_table_entry) Hashtbl.t
}

type s_module = {
  sm_scope: s_symbol_table;
}
