
open Ast

type roc_symbol_table_entry =


type roc_symbol_table = {
  rst_parent : roc_symbol_table option;
  rst_symbols : roc_symbol list;
}

type roc_s_stmt =
    TODO of string

type roc_s_block = {
  rsb_stmts : roc_s_stmt list;
  rsb_scope : roc_s_scope;

}

type roc_s_function_param = {
  rsfp_name : string;
  rsfp_type : roc_type;
}

type roc_s_function = {
  rsf_name : string;
  rsfp_self: bool;
  rsf_params : roc_s_function_param list;
  rsf_return_type : roc_type;
  rsf_body : roc_s_block;
}


type roc_s_module = {

}