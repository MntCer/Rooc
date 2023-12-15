(* 
 * Abstract Syntax Tree and functions for printing it 
 *)

(* type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or *)


type arith_op = Add | Sub | Mult | Div
type logical_op = And | Or
type comparison_op = Equal | Neq | Less | Leq | Greater | Geq 
type unary_op = Not | Neg


(* Define the possible types in Rooc *)
type r_type = 
  | T_unit
  | T_int
  | T_float
  | T_string
  | T_bool
  | T_typex of r_type_expr


and r_type_expr =
  | R_user_defined_type of string


(* type typ = Primitive of primitive_typ | Generic of generic_typ
and generic_typ = List of typ
and primitive_typ = Int | Float | String | Bool *)


type roc_expr =
  | EXPR_null
(* literal expr *)
  (* | Roc_unit_literal #TODO *)
  | Roc_string_literal of string
  | Roc_int_literal of int
  | Roc_float_literal of float
  | Roc_bool_literal of bool
(* operator expr *)
  | Roc_unary_expr of unary_op * roc_expr
  | Roc_arith_expr of  arith_op * roc_expr * roc_expr
  | Roc_logical_expr of logical_op * roc_expr * roc_expr
  | Roc_comparison_expr of  comparison_op * roc_expr * roc_expr
  | Roc_assignment_expr of roc_expr * roc_expr

  | Roc_grouped_expr of roc_expr

  | Roc_call_expr of string * ( roc_expr list ) (* expr, callParams*)
  | EXPR_field_access of string * roc_expr
  | EXPR_path of string
  | EXPR_struct of roc_expr * (struct_field_expr list)

and struct_field_expr = 
  {
    esf_name : string;
    esf_expr : roc_expr; }

and roc_variable =
  { rv_name : string;
    rv_type : r_type;
    rv_initial_expr : roc_expr option; }

and roc_stmt =
  | Roc_expr_stmt of roc_expr
  | Roc_var_decl_stmt of roc_variable
  | Roc_let_decl_stmt of roc_variable
  | STMT_block of roc_block
  | Roc_while_stmt of roc_expr * roc_block
  | Roc_if_stmt of roc_expr * roc_block * roc_block
  | Roc_break_stmt
  | Roc_continue_stmt
  | Roc_return_stmt of roc_expr

and roc_block =
  {
    rb_stmts : roc_stmt list;
  }


type roc_params = {
  rp_params : roc_variable list;
}

type roc_function = {
  rf_name : string;
  rf_params : roc_params option;
  rf_return_type : r_type;
  rf_body : roc_block;
}

type roc_method_signature = {
  rms_name : string;
  rms_params : roc_params option;
  rms_return_type : r_type;
}

type roc_method = {
  rm_name : string;
  rm_params : roc_params option;
  rm_return_type : r_type;
  rm_body : roc_block;
}

type r_struct_field ={
  rsf_name : string;
  rsf_type : r_type;
}

type r_struct = {
  rs_name : string;
  rs_fields : r_struct_field list;
}

type roc_trait = {
  rt_name : string;
  rt_methods : roc_method_signature list;
}


type roc_item = 
    FunctionItem of roc_function
  | StructItem of r_struct
  (* | ConstantItem of roc_constant *)
  (* | ConstantItem of roc_constant *)
  | TraitItem of roc_trait

type roc_module = {
  rm_items: roc_item list;
}

 

(* for trait *)
(*
type roc_func_sig = {
  rfs_return_type : r_type;
  rfs_name : string;
  rfs_params : roc_params option;
}
*)

(*
type impl_decl = {
  i_name : string;
  i_forstruct : string;
  i_methods : func_decl list;
} *)

(* ************************************************************ *)
(* Pretty-printing functions *)
(* ************************************************************ *)

let rec string_of_module 
  roc_module = 
  let items = roc_module.rm_items in
  let items_str = List.map string_of_item items in
  String.concat "\n" items_str


and string_of_arith_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"

and string_of_logical_op = function
    And -> "&&"
  | Or -> "||"

and string_of_comparison_op = function
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

and string_of_unary_op = function
    Neg -> "-"
  | Not -> "!"

and string_of_r_type = function
  T_int -> "int"
| T_bool -> "bool"
| T_float -> "float"
| T_string -> "string"
| T_unit -> "unit"
| T_typex t -> "typex" ^ (string_of_r_type_expr t)

and string_of_r_type_expr = function
  R_user_defined_type t -> t

and string_of_item = function
  FunctionItem f -> string_of_function f
| StructItem s -> string_of_struct s
| TraitItem t -> string_of_trait t

and string_of_function f = 
  let name = f.rf_name in
  let params = f.rf_params in
  let return_type = f.rf_return_type in
  let body = f.rf_body in
  let params_str = match params with
    | None -> ""
    | Some p -> string_of_params p in
  let return_type_str = string_of_r_type return_type in
  let body_str = string_of_block body in
  "fn " ^ name ^ params_str ^ " -> " ^ return_type_str ^ " {\n" ^ body_str ^ "\n}"

and string_of_params p = 
  "TODO: params"

and string_of_block b = 
  let stmts = b.rb_stmts in
  let stmts_str = List.map string_of_stmt stmts in
  String.concat "\n" stmts_str

and string_of_stmt s = 
  "TODO: stmt"

and string_of_struct s = 
  "TODO: struct str"

and string_of_trait t =
  "TODO: trait str"
