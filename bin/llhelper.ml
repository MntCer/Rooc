(*
 * 
 *)
open Llvm

let the_global_context = global_context ()

type ir_variable = {
  iv_name : string;
  iv_value_addr : llvalue;
  iv_type : lltype;
}

and ir_function ={
  if_return_type : lltype;
  if_param_types : lltype array;
  if_function_type : lltype;
  if_function : llvalue;
  if_builder : llbuilder option;
  if_scope : ir_local_scope option;
}

and ir_scope = 
  | GlobalScope of ir_global_scope
  | LocalScope of ir_local_scope

and ir_local_scope = {
  ils_variables: (string, ir_scope_entry) Hashtbl.t;
  ils_parent : ir_scope;
}

and ir_scope_entry =
  | IRFuncEntry of ir_function
  | IRVarEntry of ir_variable


and ir_global_scope = {
  igs_items: (string, ir_scope_entry) Hashtbl.t;
}

let rec lookup (identifier: string) (scope: ir_scope) : ir_scope_entry option =
  match scope with
  | LocalScope ls ->
    ( match Hashtbl.find_opt ls.ils_variables identifier with
    | Some var -> Some var
    | None -> lookup identifier ls.ils_parent  
    )
  | GlobalScope gs ->
    Hashtbl.find_opt gs.igs_items identifier

let insert_local_variable (identifier: string) (variable: ir_variable) (local_scope: ir_local_scope) : unit =
  Hashtbl.add local_scope.ils_variables identifier (IRVarEntry variable)

let insert_global_function (identifier: string) (f: ir_function ) (global_scope: ir_global_scope) : unit =
  Hashtbl.add global_scope.igs_items identifier (IRFuncEntry f)

let init_global_scope () : ir_global_scope =
  {
    igs_items = Hashtbl.create 10;  (* or any suitable initial size *)
  }
  
let init_local_scope (parent: ir_scope) : ir_local_scope =
  {
    ils_variables = Hashtbl.create 10;  (* or any suitable initial size *)
    ils_parent = parent;
  }
  
    