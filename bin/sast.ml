
open Ast

type s_type =
    ST_int
  | ST_float
  | ST_string
  | ST_bool
  | ST_unit
  | ST_function of s_function_type
  | ST_error

and s_function_type = {
  sft_params_type: s_type list;
  sft_return_type: s_type;
  (* sft_generics: string list;  *)
  (* Future use: Names of generic type parameters *)
}

type s_expr = {
    se_type: s_type;
    se_content: s_expr_content;
  }

and s_expr_content =
    S_string_literal of string
  | S_int_literal of int
  | S_float_literal of string
  | S_bool_literal of bool

  | S_unary_expr of unary_op * s_expr
  | S_arith_expr of arith_op * s_expr * s_expr
  | S_logical_expr of logical_op * s_expr * s_expr
  | S_comparison_expr of comparison_op * s_expr * s_expr
  | S_assignment_expr of s_expr * s_expr
  | S_path_expr of s_path_expr
  | S_call_expr of s_call_expr
  | S_grouped_expr of s_expr

  | S_return_expr of s_expr

  | S_break_expr
  | S_continue_expr

  | S_for_expr of s_for_expr
  | S_while_expr of s_while_expr
  | S_if_expr of s_if_expr
  | S_block_expr of s_block_expr

  | S_null_expr

and s_path_expr = {
  spe_path: string list;
}
  
and s_call_expr ={
  sce_callee: s_path_expr;
  sce_arguments: s_expr list;
}

and s_for_expr = {
  sfe_init : s_expr;
  sfe_condition : s_expr;
  sfe_update : s_expr;
  sfe_body : s_block_expr;
}

and s_while_expr = {
  swe_condition : s_expr;
  swe_body : s_expr;
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
    S_expr_stmt of s_expr
  | S_var_decl_stmt of s_variable
  | S_let_decl_stmt of s_variable

and s_block_expr = {
  sbe_stmts : s_stmt list;
  sbe_scope : s_symbol_table;
}

and s_params = {
  sp_params : s_variable list;
}

and s_function_body =
  | UserDefined of s_block_expr
  | BuiltIn

and s_function = {
  sf_name : string;
  sf_self: bool;
  sf_params : s_params option;
  sf_type : s_function_type;
  sf_body : s_function_body;
}

and s_symbol_table_entry =
    FuncEntry of s_function
  | VarEntry of s_variable

and s_symbol_table = {
  sst_parent : s_symbol_table option;
  sst_symbols: (string, s_symbol_table_entry) Hashtbl.t
}

type s_module = {
  sm_namespace: s_symbol_table;
}

let string_of_module = function
  _ -> "TODO"
