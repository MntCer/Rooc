(* Code generating for Rooc *)

open Sast
open Util
open Llirwrapper
open Builtins
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

type lltrans_cxt ={
  cur_function: L.llvalue;
}

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
  let i32_t     = L.i32_type    the_context in
  let i8_t      = L.i8_type     the_context in
  let i1_t       = L.i1_type     the_context in
  let float_t    = L.double_type the_context in
  let void_t     = L.void_type   the_context
  in

  (** 
    Convert SAST types to LLVM types 
  *)
  let trans_type = function
    | ST_int -> i32_t
    | ST_bool -> i1_t
    | ST_float -> float_t
    (* | ST_string -> char pointer *)
    | ST_unit -> void_t (* #TODO: ad hoc solution, need re reclear it *)
    | _ -> todo "other type: ST_function & ST_error"
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
        L.const_float float_t f

    | S_bool_literal b ->
        L.const_int i1_t (if b then 1 else 0)
    (* TODO: | S_string_literal s -> L.const_string or L.const_stringz (null terminated)*)

    | S_EXPR_null ->
      L.const_null i8_t

    | S_unary_expr (op, e) -> 
      let operand = trans_expr e the_builder the_scope in
      let op_instr = 
        match op with
         | A.Neg -> 
          (match e_type with
           | ST_float -> L.build_fneg
           | _ -> L.build_neg)
         | A.Not -> L.build_not in
         op_instr operand "tmp" the_builder

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
        | ST_float ->
          (match op with
          | A.Add -> L.build_fadd 
          | A.Sub -> L.build_fsub 
          | A.Mult -> L.build_fmul
          | A.Div -> L.build_fdiv)
        | _ -> raise (type_err_failure "Arithmetic expression not supported for other types than int and float") in 
      op_instr operand1 operand2 "tmp" the_builder

    | S_logical_expr (op, e1, e2) -> 
      let operand1 = trans_expr e1 the_builder the_scope in
      let operand2 = trans_expr e2 the_builder the_scope in
      let op_instr = 
        match op with
        | A.And -> L.build_and
        | A.Or -> L.build_or
        | _ -> raise (type_err_failure "Logical expression only support && and || for bool") in 
      op_instr operand1 operand2 "tmp" the_builder

    | S_comparison_expr (op, e1, e2) -> 
      let operand1 = trans_expr e1 the_builder the_scope in
      let operand2 = trans_expr e2 the_builder the_scope in
      let op_instr = 
        match e1.se_type with
        | ST_int -> 
          (match op with
          | A.Equal   -> L.build_icmp L.Icmp.Eq
	        | A.Neq     -> L.build_icmp L.Icmp.Ne
	        | A.Less    -> L.build_icmp L.Icmp.Slt
	        | A.Leq     -> L.build_icmp L.Icmp.Sle
	        | A.Greater -> L.build_icmp L.Icmp.Sgt
	        | A.Geq     -> L.build_icmp L.Icmp.Sge)
        | ST_float ->
          (match op with
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge)
        | ST_bool ->
          (match op with
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | _ -> raise (type_err_failure "Comparison expression not supported for other operations than eql and neq for bool"))
        | _ -> raise (type_err_failure "Comparison expression not supported for other types than int, float, and bool") in
      op_instr operand1 operand2 "tmp" the_builder

    | S_assignment_expr (e1, e2) -> 
      let the_value = trans_expr e2 the_builder the_scope in
      let the_assignee_name = 
        match e1.se_expr with
        | S_EXPR_path var_name -> var_name
        | _ -> todo "not supported assignee category"
      in 
      let the_assignee = 
        match lookup the_assignee_name (IRLocalScope the_scope) with
        | Some (IRVarEntry v) -> v
        | _ -> bug "variable not found"
      in 
      L.build_store the_value the_assignee.iv_value_addr the_builder 

    | S_EXPR_call call_e -> 
      let the_callee_name = call_e.sc_callee in 
      let the_wrapped_callee = 
        match lookup the_callee_name (IRGlobalScope the_namespace) with
        | Some (IRFuncEntry (IRRoocFunction f)) -> f
        | _ -> bug "callee not found"
      in
      let the_callee = the_wrapped_callee.irf_function in 
      let args = 
        Array.of_list (List.map 
        (fun arg -> trans_expr arg the_builder the_scope) call_e.sc_arguments )
      in
      let the_instr_name = 
        match the_wrapped_callee.irf_return_type with
          | ST_unit -> ""
          | _ -> the_callee_name ^ "_result"
      in
      L.build_call the_callee args the_instr_name the_builder

    | S_grouped_expr e -> trans_expr e the_builder the_scope

    | S_EXPR_field_access (e, field_name) -> todo "field access"

    | S_EXPR_path (var_name) ->
      let the_var = 
        match lookup var_name (IRLocalScope the_scope) with
        | Some (IRVarEntry v) -> v
        | _ -> bug "variable not found"
      in
      L.build_load the_var.iv_value_addr var_name the_builder

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
    let initial_value =
      match v.sv_initial_value with
      | Some (e) -> trans_expr e the_builder the_scope
      | None -> todo "deal with non intial value" 
    in
    let _ = L.build_store initial_value alloca the_builder in
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
    | Some _ -> ()
    | None -> ignore (goto_instr builder) in


  (**
    Translate the code for the given statement; 
    return the builder for the statement's successor.
  *)
  let rec trans_stmt 
    (s: s_stmt) 
    the_builder 
    (the_scope:ir_local_scope) 
    (the_cxt:lltrans_cxt)
    : L.llbuilder =
    let the_function = the_cxt.cur_function in
    (match s with
    | S_expr_stmt e -> 
      let _ = trans_expr e the_builder the_scope in the_builder

    | S_var_decl_stmt v -> (* #TODO: need to treat seperately. *)
      trans_var_decl v the_builder the_scope

    | S_let_decl_stmt v ->
      todo "let decl"

    | S_STMT_return e ->
      let the_return_value = trans_expr e the_builder the_scope in
      let _ = L.build_ret the_return_value the_builder in 
      the_builder

    | S_STMT_block b ->
      (* #TODO: need new block? *)
      let new_scope = init_local_scope (IRLocalScope the_scope) in
      let the_stmts = b.sb_stmts in
      let the_builder = 
        List.fold_left (
          fun the_builder stmt ->
            trans_stmt stmt the_builder new_scope the_cxt)
          the_builder the_stmts
      in the_builder

    | S_STMT_while w -> 
      (* append_block c name f creates a new basic block named name at the end of function f in the context c *)
      (* create a bb for predicate and branch to it. *)
      let predicate_bb = L.append_block the_context "while" the_function in
      let _ = L.build_br predicate_bb the_builder in
      (* build while body*)
      let body_bb = L.append_block the_context "while_body" the_function in
      let while_builder = 
        trans_stmt (S_STMT_block w.sws_body) (L.builder_at_end the_context body_bb) the_scope the_cxt in
      (* set loop back to predicate_bb*)
      let () = add_terminator while_builder (L.build_br predicate_bb) in
      (* generate ll code for predicate_bb*)
      let pred_builder = L.builder_at_end the_context predicate_bb in
      (* evaluate predicate*)
      let bool_val = trans_expr w.sws_condition pred_builder the_scope in
      (*create merge block*)
      let merge_bb = L.append_block the_context "merge" the_function in
      (* depends on bool_val, branches to body_bb or merge_bb*)
      let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
      L.builder_at_end the_context merge_bb
    
    | S_STMT_if s -> todo "210 in codegen"
    (* s.sie_condition
    s.sie_true_branch
    s.sie_false_branch *)
    
    | _ -> todo "trans_stmt"
    )
  in

  (**
    take a s_function in and get a LLVM function in llvalue type
  *)
  let trans_function (f: s_function) =
    let the_name=f.sf_name in
    (* let () = print_string the_name in *)
    let search_result =
      let the_entry = lookup the_name (IRGlobalScope the_namespace) in
      match the_entry with
      | Some(IRFuncEntry (IRRoocFunction (f))) -> f
      | _ -> bug "Want irf_function but get others."
    in
    let the_function = search_result.irf_function in
    let the_scope = search_result.irf_scope in
    let the_builder = L.builder_at_end the_context (L.entry_block the_function) in
    let the_cxt={
      cur_function=the_function;
    } in

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
          (fun the_builder stmt -> 
            trans_stmt stmt the_builder the_scope the_cxt) 
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
    (match search_result.irf_return_type with
    | ST_int -> 
      L.build_ret (L.const_int i32_t 0)
    | ST_unit -> 
      L.build_ret_void
    | ST_float ->
      L.build_ret (L.const_float float_t 0.0)
    | ST_bool ->
      L.build_ret (L.const_int i1_t 0)
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
        irf_return_type=f.sf_type.sft_return_type;
        irf_param_types=f.sf_type.sft_params_type;
        irf_function_type=llvm_function_type;
        irf_function=llvm_function;  
        irf_scope=llvm_function_scope;
      }
      in insert_global_function f.sf_name (IRRoocFunction the_function) the_namespace
    | StructEntry s -> ()
    | _ -> bug "Unallowed thing is in the highest-level." 
  in 
    declare_printf the_context the_module the_namespace;
    Hashtbl.iter register_item to_trans.sm_namespace.sst_symbols;
    Hashtbl.iter trans_item to_trans.sm_namespace.sst_symbols;
    the_module
