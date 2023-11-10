

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string (*float*)
  | SSliteral of string (*string*)
  | SBoolLit of bool
  | SMember of string * string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt *sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
  sfd_typ : typ;
  sfd_name : string;
  sfd_formals : bind list;
  sfd_locals : bind list;
  sfd_body : sstmt list;
}

(* for trait *)
type sfunc_sig = {
  sfs_typ : typ;
  sfs_name : string;
  sfs_formals : bind list;
}

type strait_decl = {
  str_name : string;
  str_methods : sfunc_sig list;
}

type sstruct_decl = {
  ss_name : string;
  ss_fields : bind list;
}

type simpl_decl = {
  si_name : string;
  si_forstruct : string;
  si_methods : sfunc_decl list;
}

type sprogram = sfunc_decl list * strait_decl list * sstruct_decl list * simpl_decl list