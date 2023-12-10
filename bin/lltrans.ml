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

  let the_namespace = init_global_scope () in 

  (* decl corresponding LL types *)
  (* #TODO: these part should be refactored out, be independent. *)
  let i32_t     = L.i32_type    the_context 
  and i8_t      = L.i8_type     the_context
  and i1_t       = L.i1_type     the_context
  and float_t    = L.double_type the_context
  and void_t     = L.void_type   the_context
  in

  (** 
    Convert SAST types to LLVM types 
  *)
  let trans_type = function
    | ST_int -> i32_t
    | ST_bool -> i1_t
    | ST_float -> float_t
    (* //TODO: *)
    | ST_unit -> void_t (* #TODO: ad hoc solution, need re reclear it *)
    | _ -> todo "other type"
  in
  
  (**
    #TODO: add docstring
  *)
  let rec trans_expr 
    (e:s_expr) 
    the_builder 
    (the_scope:ir_local_scope)
    : L.llvalue = 
    let e_type = e.se_type in
    let structual_e = e.se_expr in
    (match structual_e with
    | S_int_literal i -> 
        L.const_int i32_t i
    | S_float_literal f -> 
      todo "float literal"
    | S_bool_literal b ->
        L.const_int i1_t (if b then 1 else 0)
    | S_string_literal s -> 
      todo "string literal"
    | S_unary_expr (op, e) -> 
      todo "unary expression"
    | S_arith_expr (op, e1, e2) -> 
      let operand1 = trans_expr e1 the_builder the_scope in
      let operand2 = trans_expr e2 the_builder the_scope in
      let op_instr = 
        match e_type with
        | ST_int -> 
          (match op with
          | A.Add -> L.build_add 
          | A.Sub -> L.build_sub 
          | A.Mult -> L.build_mul
          | A.Div -> L.build_sdiv)
        | _ -> todo "other type in arith expr than int" in 
      op_instr operand1 operand2 "tmp" the_builder
    | S_EXPR_call call_e -> todo "call expression"
    | S_EXPR_field_access (e, field_name) -> todo "field access"
    | _ -> todo "trans_expr")
  in

  (**
    #TODO: add docstring
  *)
  let trans_var_decl 
    (v: s_variable) 
    the_builder 
    (the_scope:ir_local_scope) 
    : L.llbuilder =
    let local_name = v.sv_name in
    let local_type = trans_type v.sv_type in
    let alloca = L.build_alloca local_type local_name the_builder in
    let local_var = {
      iv_name = local_name;
      iv_type = local_type;
      iv_value_addr = alloca;
    } in
    let () = insert_local_variable local_name local_var the_scope in 
    the_builder
  in

  (** 
    Invoke "goto_instr builder" if the current block 
    doesn't already have a terminator 
  *)
  let add_terminator builder goto_instr =
    match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
    | None -> ignore (goto_instr builder) in


  (**
    Translate the code for the given statement; 
    return the builder for the statement's successor.
  *)
  let rec trans_stmt 
    (s: s_stmt) 
    builder 
    (scope:ir_local_scope) 
    : L.llbuilder =
    (match s with
    | S_expr_stmt e -> 
      let _ = trans_expr e builder scope in builder
    | S_var_decl_stmt v | S_let_decl_stmt v -> 
      trans_var_decl v builder scope
    | S_STMT_return e ->
      let the_v = trans_expr e builder scope in
      let _ = L.build_ret the_v builder in builder
    | _ -> todo "trans_stmt"
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
    let the_name=f.sf_name in
    let search_result =
      let the_entry = lookup the_name (IRGlobalScope the_namespace) in
      match the_entry with
      | Some(IRFuncEntry (IRRoocFunction (f))) -> f
      | _ -> bug "Want if_function but get others."
    in
    let the_function = search_result.if_function in
    let the_scope = search_result.if_scope in
    let the_builder = L.builder_at_end the_context (L.entry_block the_function) in

    (* Process formals: Set names and allocate space *)
    let add_formal (param: s_variable) llvm_param =
      let local_name = param.sv_name in
      let local_type = trans_type param.sv_type in
      let () = L.set_value_name local_name llvm_param in
      let alloca = L.build_alloca local_type local_name the_builder in
      let _ = L.build_store llvm_param alloca the_builder in
      let local_var = {
        iv_name = local_name;
        iv_type = local_type;
        iv_value_addr = alloca;
      } in
      insert_local_variable local_name local_var the_scope;
    in
    (* get the params of `the_function` and apply `add_formal` on them. *)
    (match f.sf_params with
    | Some params -> 
      List.iter2 add_formal params.sp_params
                        (Array.to_list (L.params the_function))
    | None -> ());

    (* translate the function body *)
    let the_builder = 
      match f.sf_body with
      | UserDefined body -> (
          let stmts=body.sb_stmts in
          List.fold_left  (* #TODO: this line is a little ugly *)
          (fun the_builder s -> 
            trans_stmt s the_builder the_scope) 
            the_builder stmts)
      | BuiltIn -> (
        let translate_builtin_func 
          the_name 
          the_builder 
          the_scope
          the_namespace =
          try
            let trans_func = Hashtbl.find builtins_map the_name in
            trans_func the_builder the_scope the_namespace
          with
          | Not_found -> bug "not supported built-in function" 
        in 
        translate_builtin_func the_name the_builder the_scope the_namespace;
      )
    in

    (* add the final terminator *)
    add_terminator 
    the_builder 
    (match search_result.if_return_type with
    | i32_t -> L.build_ret (L.const_int i32_t 0)
    | _ -> todo "other return type"
    );

  in

  (**
    #TODO: add docstring     
  *)
  let trans_item 
  (key: string) 
  (item: s_symbol_table_entry) 
  : unit =
    match item with
    | FuncEntry f -> trans_function f
    | _ -> todo "not supported. in translate_item"
  in

  (**
    pre-fill `the namespace` for following code generation.
  *)
  let register_item 
    (key: string) 
    (to_register:s_symbol_table_entry)
    : unit =
    match to_register with
    (**
      put the type, llvm function value and the scope into the wrapper.     
    *)
    | FuncEntry f ->         
      (* let () = print_string(string_of_stype (f.sf_type.sft_return_type)) in (* #DEBUG *) *)
      let llvm_return_type = trans_type f.sf_type.sft_return_type in
      let llvm_param_types = Array.of_list (List.map trans_type f.sf_type.sft_params_type) in
      let llvm_function_type = L.function_type llvm_return_type llvm_param_types in
      let llvm_function = L.define_function f.sf_name llvm_function_type the_module in
      let llvm_function_scope = init_local_scope (IRGlobalScope the_namespace) in
      let the_function = {
        if_return_type=llvm_return_type;
        if_param_types=llvm_param_types;
        if_function_type=llvm_function_type;
        if_function=llvm_function;  
        if_scope=llvm_function_scope;
      }
      in insert_global_function f.sf_name (IRRoocFunction the_function) the_namespace
    | _ -> bug "Unallowed thing is in the highest-level." 
  in 
    declare_printf the_context the_module the_namespace;
    Hashtbl.iter register_item to_trans.sm_namespace.sst_symbols;
    Hashtbl.iter trans_item to_trans.sm_namespace.sst_symbols;
    the_module
