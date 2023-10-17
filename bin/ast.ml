(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Not | Neg

type typ = Int | Float | String | Bool | Void

(* type typ = Primitive of primitive_typ | Generic of generic_typ
and generic_typ = List of typ
and primitive_typ = Int | Float | String | Bool | Void *)

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string (*float*)
  | Sliteral of string (*string*)
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
  | While of expr * stmt

type func_decl = {
    fd_typ : typ;
    fd_name : string;
    fd_formals : bind list;
    fd_locals : bind list;
    fd_body : stmt list;
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
}

type program = func_decl list * trait_decl list * struct_decl list
               * impl_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

 let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Sliteral(s) -> s
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

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void" 
  | String -> "string"


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

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fsig fsig = 
  string_of_typ fsig.fs_typ ^ " " ^
  fsig.fs_name ^ "(" ^ String.concat ", " (List.map snd fsig.fs_formals)

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
let get_4_4 (_, _, _, a) = a

