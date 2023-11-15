
open Ast

type roc_s_type=
    ST_int
  | ST_float
  | ST_string
  | ST_bool
  | ST_void
  | ST_unit



type roc_s_expr =


and roc_s_block = {
  rsb_stmts : roc_s_stmt list;
  rsb_scope : roc_s_scope;

}

and roc_stmt =
    Roc_expr_stmt of roc_expr
  | Roc_var_decl_stmt of roc_variable
  | Roc_let_decl_stmt of roc_variable


type roc_s_function = {
  (* rfs_self: some type of refer *)
  rsf_params : roc_s_variable list;
  rsf_return_type : roc_s_type;
  rsf_body : roc_s_block;

}

type symbol_table_entry =
    FuncEntry of roc_s_function
  | VarEntry of roc_s_variable



(* 

type roc_symbol_table = {
  rst_parent : roc_symbol_table option;
  rst_symbols : roc_symbol list;
}


type symbol_table_entry =
  VarEntry of asdadasd
  FunEntry of asdadasd

let check_symtable tab =

type symbol_table = {
    entries: (string, symbol_table_entry) Hashtbl.t;
    parent: symbol_table option;
    scope_type: scope_type;
}


type roc_s_stmt =
    TODO of string



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

} *)