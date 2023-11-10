(* Abstract Syntax Tree and functions for printing it *)

(* type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or *)


type arith_logical_op = Add | Sub | Mult | Div | And | Or  
type comparison_op = Equal | Neq | Less | Leq | Greater | Geq 
type unary_op = Not | Neg


(* Define the possible types in Rooc *)
type roc_type =
    TInt
  | TFloat
  | TBool
  | TString
  (* | TFunction of roc_type list * roc_type  *)
  | TUnit
  | TVoid
  (*%TODO:*)

(* Define the runtime values in Rooc *)
type roc_value =
    VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | Null

(* type typ = Primitive of primitive_typ | Generic of generic_typ
and generic_typ = List of typ
and primitive_typ = Int | Float | String | Bool | Void *)

type name_type_bind = string * roc_type

type roc_expr =
    (* literal expr *)
    Roc_string_literal of string
  | Roc_int_literal of int
  | Roc_float_literal of float
  | Roc_bool_literal of bool
    (* operator expr *)
  | Roc_unary_expr of unary_op * roc_expr
  | Roc_arith_logical_expr of roc_expr * arith_logical_op * roc_expr
  | Roc_comparison_expr of roc_expr * comparison_op * roc_expr
  | Roc_assign_expr of roc_expr * roc_expr
    (* grouped expr *)
  | Roc_grouped_expr of roc_expr
    (* call expr *)
  | Roc_call_expr of string * ( roc_expr list ) (* expr, callParams*)
    (* return expr *)
  | Roc_return_expr of roc_expr
    (* loop expr *)
  | Roc_for_expr of roc_expr * roc_expr * roc_expr * roc_block_expr
  | Roc_while_expr of roc_expr * roc_block_expr
  | Roc_break_expr
  | Roc_continue_expr
    (* if expr *)
  | Roc_if_expr of roc_expr * roc_block_expr * roc_block_expr
  | Roc_null_expr

and roc_block_expr =
    Roc_Block_expr of roc_stmt list

and roc_stmt =
    Roc_expr_stmt of roc_expr
  | Roc_var_decl_stmt of name_type_bind * roc_expr
  | Roc_let_decl_stmt of name_type_bind * roc_expr

(* ... other entry types as needed ... *)


type roc_function_params = {
  rfp_self : bool;
  rfp_params : name_type_bind list;
}

type roc_function_signature = {
  rfs_name : string;
  rfs_params : roc_function_params;
  rfs_return_type : roc_type;
}

type roc_function = {
    rfun_signature : roc_function_signature;
    rfun_body : roc_block_expr;
}

type roc_module = {
    rm_functions: roc_function list;
    rm_constants: name_type_bind list;
    (* %TODO: *)
}

let get_2_1 (a, _) = a
let get_2_2 (_, a) = a

(* type program = func_decl list * trait_decl list * struct_decl list
               * impl_decl list *)


(* type expr =
    Literal of int
  | Fliteral of string (*float*)
  | Sliteral of string (*string*)
  | Member of string * string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt *)

(* type roc_function = *)



(* type func_decl = {
    fd_typ : roc_type;
    fd_name : string;
    fd_formals : name_type_bind list;
    fd_locals : name_type_bind list;
    fd_body : roc_block_expr;
  }

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

(* Pretty-printing functions *)

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
  TInt -> "int"
| TBool -> "bool"
| TFloat -> "float"
| TVoid -> "void" 
| TString -> "string"
| TUnit -> "unit"

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

(* *************************************** *)
(* print function of old ast *)
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

