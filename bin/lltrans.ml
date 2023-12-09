(* Code generating for Rooc *)

open Sast
open Util
open Llhelper
open Builtins
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(**
  translate the whole module from sast node `s_module` to corresponding LLVM module.
  Should organize helper functions in the order of `from bottom to top`.
    *)
let trans_module 
    (to_trans: s_module) 
    (the_context: L.llcontext)
    : L.llmodule =

  let the_module = L.create_module the_context "theRoocProgram" in

  let (item_to_llvalue : (string, L.llvalue) Hashtbl.t) = Hashtbl.create 10 in

  let the_namespace = init_global_scope () in (* #TODO: should be cleaned*)

  (* decl corresponding LL types *)
  (* #TODO: these part should be refactored out, be independent. *)
  let i32_t     = L.i32_type    the_context 
  and i8_t      = L.i8_type     the_context
  and i1_t       = L.i1_type     the_context
  and float_t    = L.double_type the_context
  in

  (** 
    Convert SAST types to LLVM types 
  *)
  let trans_type = function
    | ST_int -> i32_t
    | ST_bool -> i1_t
    | ST_float -> float_t
    (* //TODO: *)
    | _ -> todo "not supported. in ltype_of_stype"
  in

  (**
    #TODO: add docstring
  *)
  let trans_expr (e:s_expr) builder (scope:ir_local_scope): unit = 
    let e_content = e.se_expr in
    match e_content with
    | S_int_literal i -> 
        ignore ( L.const_int i32_t i)
    | _ -> ()
  in

  (**
    #TODO: add docstring
  *)
  let trans_var_decl (v: s_variable) builder (scope:ir_local_scope) : unit =
    let local_name = v.sv_name in
    let local_type = trans_type v.sv_type in
    let alloca = L.build_alloca local_type local_name builder in
    let local_var = {
      iv_name = local_name;
      iv_type = local_type;
      iv_value_addr = alloca;
    } in
    insert_local_variable local_name local_var scope
  in

  (**
    #TODO: add docstring
  *)
  let rec trans_stmt (s: s_stmt) builder (scope:ir_local_scope) : unit =
    (match s with
    | S_expr_stmt e -> 
      (trans_expr e builder scope)
    | S_var_decl_stmt v | S_let_decl_stmt v -> 
      (trans_var_decl v builder scope)
    )
  in

  (** 
    Declare built-in functions 
  *)
  (* #TODO *)

  (**
    take a s_function in and get a LLVM function in llvalue type
  *)
  let trans_function (f: s_function) =
    match f.sf_body with
    | UserDefined body -> 
      (* Step 1: Determine LLVM function type *)
      (
        let llvm_return_type = trans_type f.sf_type.sft_return_type in
        let llvm_param_types = Array.of_list (List.map trans_type f.sf_type.sft_params_type) in
        let llvm_function_type = L.function_type llvm_return_type llvm_param_types in

        let llvm_function = L.define_function f.sf_name llvm_function_type the_module in

        (* Builder for function body *)
        let builder = L.builder_at_end the_context (L.entry_block llvm_function) in

        let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  
        (* Process formals: Set names and allocate space *)
        let local_scope = init_local_scope (GlobalScope the_namespace) in
        let add_formal (param: s_variable) llvm_param =
          let local_name = param.sv_name in
          let local_type = trans_type param.sv_type in
          let () = L.set_value_name local_name llvm_param in
          let alloca = L.build_alloca local_type local_name builder in
          let _ = L.build_store llvm_param alloca builder in
          let local_var = {
            iv_name = local_name;
            iv_type = local_type;
            iv_value_addr = alloca;
          } in
          insert_local_variable local_name local_var local_scope;
        in
        (match f.sf_params with
        | Some params -> 
          List.iter2 add_formal params.sp_params
                            (Array.to_list (L.params llvm_function))
        | None -> ());
        (* ... translate the function body ... *)
        let stmts=body.sb_stmts
        in
        List.iter (fun s -> ignore( trans_stmt s builder local_scope)) stmts;
      )

    | BuiltIn -> ()

  in

  (**
    #TODO: add docstring     
  *)
  let trans_item (key: string) (item: s_symbol_table_entry) : unit =
    match item with
    | FuncEntry f -> trans_function f
    | _ -> todo "not supported. in translate_item"
  in

  try
    (* #TODO: *)
    Hashtbl.iter trans_item to_trans.sm_namespace.sst_symbols;
    the_module
  with e -> L.dispose_module the_module; raise e