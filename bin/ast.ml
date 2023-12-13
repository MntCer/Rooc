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

  | T_mutref of r_type_expr   (* Mutable reference*)
  | T_mutptr of r_type_expr   (* Mutable pointer*)
  (* | T_box of r_type_expr *)


and r_type_expr =
  | R_type_expr of r_type
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
  | EXPR_field_access of string * string
  | EXPR_path of string
  (* | EXPR_box_init of roc_expr *)


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

type roc_item = 
    FunctionItem of roc_function
  | StructItem of r_struct
  (* | ConstantItem of roc_constant *)
  (* | ConstantItem of roc_constant *)
  (* | TraitItem of roc_trait *)

type roc_module = {
  rm_items: roc_item list;
}

(* 

(* for trait *)
type func_sig = {
  fs_typ : typ;
  fs_name : string;
  fs_formals : bind list;
}

type trait_decl = {
  tr_name : string;
  tr_methods : func_sig list;
}

type impl_decl = {
  i_name : string;
  i_forstruct : string;
  i_methods : func_decl list;
} *)

(* ************************************************************ *)
(* Pretty-printing functions *)
(* ************************************************************ *)

let string_of_module = function
    _ -> "TODO"

let string_of_arith_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"

let string_of_logical_op = function
    And -> "&&"
  | Or -> "||"

let string_of_comparison_op = function
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

let string_of_unary_op = function
    Neg -> "-"
  | Not -> "!"

let string_of_r_type = function
  T_int -> "int"
| T_bool -> "bool"
| T_float -> "float"
| T_string -> "string"
| T_unit -> "unit"
