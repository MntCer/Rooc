
open Ast
open Util

type s_type =
  | ST_unit
  | ST_int
  | ST_float
  | ST_string
  | ST_bool

  | ST_function of s_function_type
  | ST_struct of s_struct_type
  | ST_sequence of s_sequence_type

  | ST_mutref of s_type
  | ST_mutptr of s_type

  | ST_error


and s_sequence_type = 
  | T_list of s_type


and s_struct_type = {
  st_name : string;
  st_field_type_list : s_type list;
}

and s_function_type = {
  sft_params_type: s_type list;
  sft_return_type: s_type;
  (* sft_generics: string list;  *)
  (* Future use: Names of generic type parameters *)
}

type s_expr = {
    se_type: s_type;
    se_expr: s_structual_expr;
  }

and s_structual_expr =
  | S_EXPR_null
(* literal expr *)
  | S_string_literal of string
  | S_int_literal of int
  | S_float_literal of float
  | S_bool_literal of bool
(* unary expr *)
  | S_unary_expr of unary_op * s_expr
(* binary expression: arith; logical; comparison *)
  | S_arith_expr of arith_op * s_expr * s_expr
  | S_logical_expr of logical_op * s_expr * s_expr
  | S_comparison_expr of comparison_op * s_expr * s_expr
  | S_assignment_expr of s_expr * s_expr
  | S_EXPR_call of sexpr_call
  | S_grouped_expr of s_expr
  | S_EXPR_field_access of string * string
  | S_EXPR_path of string
  (* | S_EXPR_box_init of s_expr *)



and sexpr_call= {
  sc_callee: string;
  sc_arguments: s_expr list;
}

and s_for_stmt = {
  sfe_init : s_expr;
  sfe_condition : s_expr;
  sfe_update : s_expr;
  sfe_body : s_block;
}

and s_while_stmt = {
  swe_condition : s_expr;
  swe_body : s_expr;
}

and s_if_stmt = {
  sie_condition: s_expr ;
  sie_true_branch : s_block;
  sie_false_branch : s_block;
}

and s_variable = {
    sv_name: string;
    sv_type: s_type;
    sv_mutable: bool;
    sv_initial_value : s_expr option;
  }

and s_stmt =
  | S_expr_stmt of s_expr
  | S_var_decl_stmt of s_variable
  | S_let_decl_stmt of s_variable
  | S_STMT_return of s_expr
  | S_STMT_break
  | S_STMT_continue

  | S_STMT_block of s_block
  | S_STMT_for of s_for_stmt
  | S_STMT_while of s_while_stmt
  | S_STMT_if of s_if_stmt


and s_block = {
  sb_stmts : s_stmt list;
  sb_scope : s_symbol_table;
}

and s_params = {
  sp_params : s_variable list;
}

and s_function_body =
  | UserDefined of s_block
  | BuiltIn

and s_function = {
  sf_name : string;
  sf_params : s_params option;
  sf_type : s_function_type;
  sf_body : s_function_body;
}

and s_method_signature = {
  sms_name : string;
  sms_params : s_params option;
  sms_type : s_function_type;
}

and s_method = {
  sm_name : string;
  sm_params : s_params option;
  sm_type : s_function_type;
  sm_body : s_block;
}

and s_function_signature = {
  sfs_name : string;
  sfs_params : s_params option;
  sfs_type : s_function_type;
}

and s_struct_field = {
  ssf_name : string;
  ssf_type : s_type;
}

and s_struct_sig = {
  sss_name : string;
}

and s_struct = {
  ss_name : string;
  ss_fields : s_struct_field list;
}

and s_symbol_table_entry =
  | FuncSigEntry of s_function_signature
  | FuncEntry of s_function
  | StructSigEntry of s_struct_sig
  | StructEntry of s_struct
  | VarEntry of s_variable

and s_symbol_table = {
  sst_parent : s_symbol_table option;
  sst_symbols: (string, s_symbol_table_entry) Hashtbl.t
}

type s_module = {
  sm_namespace: s_symbol_table;
  sm_type_env: (string, s_type) Hashtbl.t;
}

(* ************************************************************ *)
(* Some helper functions *)
(* ************************************************************ *)

exception SymbolTableError of string

let init_symbol_table ?parent () : s_symbol_table =
  let symbol_table = Hashtbl.create 10 in  (* Arbitrary initial size *)
  { sst_parent = parent; sst_symbols = symbol_table }

(**
  * Lookup a symbol in the symbol table. If not found, lookup in the parent table.
  * If not found in the parent table, return None.

*)
let rec lookup_symbol identifier symbol_table =
  match Hashtbl.find_opt symbol_table.sst_symbols identifier with
  | Some entry -> Some entry
  | None ->(
      match symbol_table.sst_parent with
      | Some parent_table -> lookup_symbol identifier parent_table  
      | None -> None  )


let insert_symbol symbol_table identifier entry =
  (* Check for existence in the current scope only *)
  if Hashtbl.mem symbol_table.sst_symbols identifier then
    raise (SymbolTableError ("Symbol already exists in the same scope: " ^ identifier))
  else
    Hashtbl.add symbol_table.sst_symbols identifier entry

let update_symbol_table symbol_table identifier new_entry =
  if Hashtbl.mem symbol_table.sst_symbols identifier then
    Hashtbl.replace symbol_table.sst_symbols identifier new_entry
  else
    raise (SymbolTableError ("Symbol not found for update: " ^ identifier))


let lookup_type 
  type_env 
  identifier = 
  match Hashtbl.find_opt type_env identifier with
  | Some entry -> entry
  | None -> bug "Type not found in this type environment"

let update_type_env 
  type_env 
  identifier 
  new_entry =
  if Hashtbl.mem type_env identifier then
    Hashtbl.replace type_env identifier new_entry
  else
    raise (SymbolTableError ("Type not found for update: " ^ identifier))

(* ************************************************************ *)
(* Pretty-printing functions *)
(* ************************************************************ *)

let string_of_s_module = function
  _ -> "TODO"

let rec string_of_s_type = function
  | ST_unit -> "unit"
  | ST_int -> "int"
  | ST_float -> "float"
  | ST_string -> "string"
  | ST_bool -> "bool"
  | ST_function ft -> string_of_s_function_type ft
  | ST_struct st -> string_of_s_struct_type st
  | ST_sequence st -> string_of_s_sequence_type st
  | ST_error -> "type error"

and string_of_s_function_type ft =
  todo "string_of_s_function_type"

and string_of_s_struct_type st =
  todo "string_of_s_struct_type"

and string_of_s_sequence_type st =
  todo "string_of_s_sequence_type"


