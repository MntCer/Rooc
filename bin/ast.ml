(* Abstract Syntax Tree and functions for printing it *)

(* type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or *)


type arith_op = Add | Sub | Mult | Div
type logical_op = And | Or
type comparison_op = Equal | Neq | Less | Leq | Greater | Geq 
type unary_op = Not | Neg


(* Define the possible types in Rooc *)
type roc_type = 
    T_int
  | T_float
  | T_string
  | T_bool
  | T_unit
  (*%TODO:*)

(* Define the runtime values in Rooc *)
type roc_value =
    V_int of int
  | V_float of float
  | V_bool of bool
  | V_string of string
  | V_null

(* type typ = Primitive of primitive_typ | Generic of generic_typ
and generic_typ = List of typ
and primitive_typ = Int | Float | String | Bool | Void *)


type roc_expr =
    (* literal expr *)
    Roc_string_literal of string
  | Roc_int_literal of int
  | Roc_float_literal of string
  | Roc_bool_literal of bool
    (* operator expr *)
  | Roc_unary_expr of unary_op * roc_expr
  | Roc_arith_expr of  arith_op * roc_expr * roc_expr
  | Roc_logical_expr of logical_op * roc_expr * roc_expr
  | Roc_comparison_expr of  comparison_op * roc_expr * roc_expr
  | Roc_assignment_expr of roc_expr * roc_expr
  | Roc_grouped_expr of roc_expr
  | Roc_path_expr of string list
  | Roc_call_expr of roc_expr * ( roc_expr list ) (* expr, callParams*)
  | Roc_return_expr of roc_expr
  | Roc_block_expr of roc_stmt list
  | Roc_for_expr of roc_expr * roc_expr * roc_expr * roc_expr
  | Roc_while_expr of roc_expr * roc_expr
  | Roc_break_expr
  | Roc_continue_expr
    (* if expr *)
  | Roc_if_expr of roc_expr * roc_expr * roc_expr

and roc_variable =
    { rv_name : string;
      rv_type : roc_type;
      rv_initial_expr : roc_expr option; }

and roc_stmt =
    Roc_expr_stmt of roc_expr
  | Roc_var_decl_stmt of roc_variable
  | Roc_let_decl_stmt of roc_variable
  | Roc_empty_stmt


(* ... other entry types as needed ... *)


type roc_params = {
  rp_params : roc_variable list;
}

type roc_function = {
  rf_name : string;
  rf_params : roc_params option;
  rf_return_type : roc_type;
  rf_body : roc_expr;
}

type roc_method_signature = {
  rms_name : string;
  rms_params : roc_params option;
  rms_return_type : roc_type;
}

type roc_method = {
  rm_name : string;
  rm_params : roc_params option;
  rm_return_type : roc_type;
  rm_body : roc_expr;
}

type roc_item = 
    FunctionItem of roc_function
  (* | ConstantItem of roc_constant *)
  (* | TypeItem of %TODO *)
  (* ... other item types ... *)

type roc_module = {
  rm_items: roc_item list;
    (* %TODO: *)
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

type struct_decl = {
  s_name : string;
  s_fields : bind list;
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

let string_of_roc_type = function
  T_int -> "int"
| T_bool -> "bool"
| T_float -> "float"
| T_string -> "string"
| T_unit -> "unit"

(* let rec string_of_typ = function
  Primitive(t) -> string_of_ptyp t
| Generic(t) -> string_of_gtyp t

and string_of_gtyp = function
  List(t) -> "list<" ^ string_of_typ t ^ ">"

and string_of_ptyp = function
  Int -> "int"
| Bool -> "bool"
| Float -> "float"
| String -> "string"
*)