(* Abstract Syntax Tree and functions for printing it *)

(* type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or *)


type arith_logical_op = Add | Sub | Mult | Div | And | Or  
type comparison_op = Equal | Neq | Less | Leq | Greater | Geq 
type unary_op = Not | Neg


(* Define the possible types in Rooc *)
type roc_type = 
    T_int
  | T_float
  | T_string
  | T_bool
  | T_void
  | T_unit
  (* | TFunction of roc_type list * roc_type  *)
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
  | Roc_arith_logical_expr of  arith_logical_op * roc_expr * roc_expr
  | Roc_comparison_expr of  comparison_op * roc_expr *roc_expr
  | Roc_assignment_expr of roc_expr * roc_expr
    (* grouped expr *)
  | Roc_grouped_expr of roc_expr
    (* call expr *)
  | Roc_call_expr of string * ( roc_expr list ) (* expr, callParams*)
  | Roc
    (* return expr *)
  | Roc_return_expr of roc_expr
    (* block expr *)
  | Roc_block_expr of roc_stmt list
    (* loop expr *)
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


type roc_function_params = {
  rfp_self : bool;
  rfp_not_self_params : roc_variable list;
}

type roc_function_signature = {
  rfs_name : string;
  rfs_params : roc_function_params option;
  rfs_return_type : roc_type;
}

type roc_function = {
  rf_signature : roc_function_signature;
  rf_body : roc_expr;
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

let string_of_arith_logical_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | And -> "&&"
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
| T_void -> "void" 
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
| Void -> "void" *)

(* ************************************************************ *)
(* print function of old ast *)
(* ************************************************************ *)

(* let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Sliteral(s) -> s
  | Member(s1, s2) -> s1 ^ "." ^ s2
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s



let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fsig fsig = 
  string_of_typ fsig.fs_typ ^ " " ^
  fsig.fs_name ^ "(" ^ String.concat ", " (List.map snd fsig.fs_formals) ^ ")\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.fd_typ ^ " " ^
  fdecl.fd_name ^ "(" ^ String.concat ", " (List.map snd fdecl.fd_formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.fd_locals) ^
  String.concat "" (List.map string_of_stmt fdecl.fd_body) ^
  "}\n"

let string_of_tdecl tdecl =
  "trait " ^ tdecl.tr_name ^ "\n" ^
  String.concat "" (List.map string_of_fsig tdecl.tr_methods)

let string_of_sdecl sdecl =
  "struct " ^ sdecl.s_name ^ "\n" ^
  String.concat "" (List.map string_of_vdecl sdecl.s_fields)

let string_of_idecl idecl =
  "impl " ^ idecl.i_name ^ " for " ^ idecl.i_forstruct ^
  String.concat "" (List.map string_of_fdecl idecl.i_methods)

let string_of_program (fdecls, tdecls, sdecls, idecls) =
  String.concat "\n" (List.map string_of_fdecl fdecls) ^ "\n" ^
  String.concat "\n" (List.map string_of_tdecl tdecls) ^ "\n" ^
  String.concat "\n" (List.map string_of_sdecl sdecls) ^ "\n" ^
  String.concat "\n" (List.map string_of_idecl idecls)

let get_4_1 (a, _, _, _) = a
let get_4_2 (_, a, _, _) = a
let get_4_3 (_, _, a, _) = a
let get_4_4 (_, _, _, a) = a *)

