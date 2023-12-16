(* 
    translate sast ir to llvm ir.
    
    author: Yuanfei, Mona, Xinyang
*)

open Sast
open Util
open Llirwrapper
open Builtins
module L = Llvm
module A = Ast

type llir_cxt ={
  cur_function: L.llvalue;
  cur_stack_base_ptr: L.llvalue option;
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

  let the_global_scope = init_global_scope () in 
  let the_type_env:llir_type_env = Hashtbl.create 10 in

  (* decl corresponding LL types *)
  (* #TODO: these part should be refactored out, be independent. *)
  let i32_t     = L.i32_type    the_context in
  let i8_t      = L.i8_type     the_context in
  let i1_t       = L.i1_type     the_context in
  let float_t    = L.double_type the_context in
  let void_t     = L.void_type   the_context in
  let str_t     = L.pointer_type (L.i8_type the_context)
  in

  (** 
    Convert SAST types to LLVM types.
    the principle to decide which lltype to use is:
    - if want to create a value in this type, which llvalue you will get, what is its type?
    - for immidiate value, we will get its high lvl type, like `int` for `int`, so just i32_type;
    - for string, we will use build_global_stringptr to get a pointer to i8 array, and that's the type here.
    - for struct, we will use build_alloca to get a struct*, so we also need to use `pointer_type` here.
  *)
  let trans_type = function
    | ST_int -> i32_t
    | ST_bool -> i1_t
    | ST_float -> float_t
    | ST_string -> str_t
    | ST_unit -> void_t (* #TODO: ad hoc solution, need re reclear it *)
    | ST_struct s -> 
      (match lookup_type s the_type_env with
      | Some t -> L.pointer_type t 
      (* #NOTE: return it's pointer here? 
         must be here, cannot change call function. *)
      | None -> raise (LLIRError "struct type used before defined")) (* This seems to be convered by SemanticError("type not registered") already *)
    | _ -> todo "other type: ST_function & ST_error"
  in

  (**
    take a struct and a field name list, resolve it's recursive call and
    get the target field's pointer, for following store and load.

    author: Yuanfei
  *)
  let rec get_field_pointer 
    (the_struct:llir_struct)
    (the_struct_ptr:L.llvalue)
    (field_name_list:string list)
    (the_builder:L.llbuilder)
    : L.llvalue =
    let struct_name = the_struct.ls_name in
      match field_name_list with
      | [field_name] ->
        let the_field = 
          match Hashtbl.find_opt the_struct.ls_llfields field_name with
          | Some f -> f
          | None -> raise (LLIRError "field not found") in
        let the_field_index = the_field.lsf_index in
          L.build_struct_gep the_struct_ptr the_field_index (struct_name^"."^field_name^"_ptr") the_builder

      | field_name::tl ->
        let the_field = 
          match Hashtbl.find_opt the_struct.ls_llfields field_name with
          | Some f -> f
          | None -> raise (LLIRError "field not found") in
        let the_field_stype = the_field.lsf_stype in
        let the_new_struct_name = 
          match the_field_stype with
          | ST_struct s -> s
          | _ -> raise (LLIRError "want to access a non-struct field") in
        let the_field_index = the_field.lsf_index in
        let the_new_struct_ptrptr = L.build_struct_gep the_struct_ptr the_field_index (struct_name^"."^field_name^"_ptr") the_builder in
        let the_new_struct_ptr = L.build_load the_new_struct_ptrptr (struct_name^"."^field_name) the_builder in
        let the_new_struct = 
          match lookup_struct the_new_struct_name the_global_scope with
          | Some s -> s
          | None -> raise (LLIRError "struct def info not found") in
          get_field_pointer the_new_struct the_new_struct_ptr tl the_builder in

  (**
    translate a s_expr to a LLVM IR

    author: Yuanfei, Mona
  *)
  let rec trans_expr 
    (e:s_expr) 
    the_builder 
    (the_scope:ir_local_scope)
    (the_cxt:llir_cxt)
    : L.llvalue = 
    let e_type = e.se_type in
    let structual_e = e.se_expr in
      (match structual_e with
      (* generate a immediate number. *)
      | S_int_literal i -> 
          L.const_int i32_t i
      (* generate a immediate number. *)
      | S_float_literal f ->
          L.const_float float_t f
      (* generate a immediate number. *)
      | S_bool_literal b ->
          L.const_int i1_t (if b then 1 else 0)
      (* will generate a stringptr *)
      | S_string_literal s -> 
        L.build_global_stringptr s "str" the_builder
      (* #TODO: need to test, i8_t isn't right. *)
      | S_EXPR_null ->
        L.const_null i8_t

      | S_unary_expr (op, e) -> 
        let operand = trans_expr e the_builder the_scope the_cxt in
        let op_instr = 
          match op with
           | A.Neg -> 
            (match e_type with
             | ST_float -> L.build_fneg
             | _ -> L.build_neg)
           | A.Not -> L.build_not in
           op_instr operand "tmp" the_builder

      | S_arith_expr (op, e1, e2) -> 
        let operand1 = trans_expr e1 the_builder the_scope the_cxt in
        let operand2 = trans_expr e2 the_builder the_scope the_cxt in
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
        let operand1 = trans_expr e1 the_builder the_scope the_cxt in
        let operand2 = trans_expr e2 the_builder the_scope the_cxt in
        let op_instr = 
          match op with
          | A.And -> L.build_and
          | A.Or -> L.build_or
          | _ -> raise (type_err_failure "Logical expression only support && and || for bool") in 
        op_instr operand1 operand2 "tmp" the_builder

      | S_comparison_expr (op, e1, e2) -> 
        let operand1 = trans_expr e1 the_builder the_scope the_cxt in
        let operand2 = trans_expr e2 the_builder the_scope the_cxt in
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
          | ST_struct _ -> 
            (match op with
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | _ -> raise (type_err_failure "Comparison expression not supported for other operations than eql and neq for struct"))
          | ST_string -> 
            (match op with
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | _ -> raise (type_err_failure "Comparison expression not supported for other operations than eql and neq for string"))
          | _ -> raise (type_err_failure "Comparison expression not supported for other types than int, float, and bool") in
        op_instr operand1 operand2 "tmp" the_builder

      | S_EXPR_call call_e -> 
        let the_callee_name = call_e.sc_callee in 
        let the_wrapped_callee = 
          match lookup the_callee_name (IRGlobalScope the_global_scope) with
          | Some (IRFuncEntry (IRRoocFunction f)) -> f
          | _ -> bug "callee not found" in
        let the_callee = the_wrapped_callee.irf_function in 
        let args = 
          Array.of_list (List.map 
          (fun arg -> trans_expr arg the_builder the_scope the_cxt) call_e.sc_arguments ) in
        let the_instr_name = 
          match the_wrapped_callee.irf_return_type with
            | ST_unit -> ""
            | _ -> the_callee_name ^ "_result" in
          L.build_call the_callee args the_instr_name the_builder

      | S_grouped_expr e -> trans_expr e the_builder the_scope the_cxt

      (*
         will return the "value" of the var, which mean a Llvm.load's result.
      *)
      | S_EXPR_field_access (var_name, field_name_list) ->
        (* the first string must be a var, need to search in local scope. *)
        let the_struct_var =
          match lookup var_name (IRLocalScope the_scope) with
          | Some (IRVarEntry v) -> v
          | _ -> raise (LLIRError "struct variable not found") in
        let the_struct_name = 
          match the_struct_var.iv_stype with
          | ST_struct s -> s
          | _ -> raise (LLIRError "struct type not found") in
        let the_struct = 
          match lookup_struct the_struct_name the_global_scope with
          | Some s -> s
          | None -> 
            let () = print_string the_struct_name in
            raise (LLIRError "struct def info not found") in
        let the_struct_ptrptr = the_struct_var.iv_value_addr in
        let the_struct_ptr = L.build_load the_struct_ptrptr var_name the_builder in
        let the_ptr = get_field_pointer the_struct the_struct_ptr field_name_list the_builder in
        L.build_load the_ptr "deference_target_field_ptr" the_builder

      (* for a var, return its value. *)
      | S_EXPR_path (var_name) ->
        let the_var = 
          match lookup var_name (IRLocalScope the_scope) with
          | Some (IRVarEntry v) -> v
          | _ -> 
            Printf.printf "var name: %s\n" var_name;
            bug "variable not found"
        in
        L.build_load the_var.iv_value_addr var_name the_builder

      (* return alloca is always right, standard operation in our project. *)
      (* #TODO: after refactor "var" in S_EXPR, rename here. *)
      | S_EXPR_struct (the_struct_name, var_list) -> 
        let the_struct_type = 
          match lookup_type the_struct_name the_type_env with
          | Some t -> t
          | None -> raise (LLIRError "struct type not found")
        in
        let the_fields_index_map = 
          match lookup_struct the_struct_name the_global_scope with
          | Some s -> s.ls_fields_index_map
          | None -> raise (LLIRError "struct not found")
        in
        let alloca = L.build_alloca the_struct_type (the_struct_name^"_instance") the_builder in
        let ()=List.iter (fun var -> 
          let the_init_val=
            match var.sv_initial_value with
            | Some (e) -> trans_expr e the_builder the_scope the_cxt
            | None -> raise (LLIRError "instantiate field without initial value") in
          let the_field_index = match get_field_index var.sv_name the_fields_index_map with
          | Some i -> i
          | None -> raise (LLIRError "field corresponding index not found") in
          let the_field_ptr = L.build_struct_gep alloca the_field_index var.sv_name the_builder in
          let _ = L.build_store the_init_val the_field_ptr the_builder in
            ()) 
          var_list
        (* insertion of symbol will happens in outside call. *)
        in alloca 

      | S_EXPR_nullstruct struct_name ->
        let the_struct_type = 
          match lookup_type struct_name the_type_env with
          | Some t -> t
          | None -> raise (LLIRError "struct type not found") in
        let struct_ptr_type = L.pointer_type the_struct_type in 
          L.const_null struct_ptr_type

      | _ -> todo "trans_expr") in

  (**
    for any var's type `t`, generate a ptr in type `*t`, which is `alloca`
    and store the llvalue of initial value in `&alloca`.

    author: Yuanfei
  *)
  let trans_var_decl 
    (v: s_variable) 
    the_builder 
    (the_scope:ir_local_scope)
    (the_cxt:llir_cxt)
    : L.llbuilder =
    let local_name = v.sv_name in
    let local_type = trans_type v.sv_type in
    (**)
    let alloca = L.build_alloca local_type local_name the_builder in
    let initial_value =
      match v.sv_initial_value with
      | Some (e) -> trans_expr e the_builder the_scope the_cxt
      | None -> todo "deal with non intial value" 
    in
    let _ = L.build_store initial_value alloca the_builder in
    let local_var = {
      iv_name = local_name;
      iv_type = local_type;
      iv_value_addr = alloca;
      iv_stype=v.sv_type;
    } in
    let () = insert_variable_local local_name local_var the_scope in 
      the_builder in

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

    author: Yuanfei, Mona and Xinyang
  *)
  let rec trans_stmt 
    (s: s_stmt) 
    the_builder 
    (the_scope:ir_local_scope) 
    (the_cxt:llir_cxt)
    : L.llbuilder =
    let the_function = the_cxt.cur_function in
    (match s with
    | S_expr_stmt e -> 
      let _ = trans_expr e the_builder the_scope the_cxt in the_builder

    | S_var_decl_stmt v -> (* #TODO: need to treat seperately. *)
      trans_var_decl v the_builder the_scope the_cxt

    | S_let_decl_stmt v ->
      trans_var_decl v the_builder the_scope the_cxt

    (* 
      lhs needs a mutable ptr, rhs needs a llvalue, type matched. 
      lvalue: a mutable variable, a field;
    *)
    | S_STMT_assignment (e1, e2) -> (
      let the_value = trans_expr e2 the_builder the_scope the_cxt in
      match e1.se_expr with
      | S_EXPR_path the_assignee_name ->
        let the_assignee = 
          match lookup the_assignee_name (IRLocalScope the_scope) with
          | Some (IRVarEntry v) -> v
          | _ -> bug "variable not found" in 
        let _ = L.build_store the_value the_assignee.iv_value_addr the_builder in
          the_builder
      | S_EXPR_field_access (var_name, field_name_list) -> 
        let the_struct_var =
          match lookup var_name (IRLocalScope the_scope) with
          | Some (IRVarEntry v) -> v
          | _ -> raise (LLIRError "struct variable not found") in
        let the_struct_name = 
          match the_struct_var.iv_stype with
          | ST_struct s -> s
          | _ -> raise (LLIRError "struct type not found") in
        let the_struct = 
          match lookup_struct the_struct_name the_global_scope with
          | Some s -> s
          | None -> 
            let () = print_string the_struct_name in
            raise (LLIRError "struct def info not found") in
        let the_struct_ptrptr = the_struct_var.iv_value_addr in
        let the_struct_ptr = L.build_load the_struct_ptrptr var_name the_builder in
        let the_ptr = get_field_pointer the_struct the_struct_ptr field_name_list the_builder in
        let _ = L.build_store the_value the_ptr the_builder in
          the_builder)

    | S_STMT_return e ->
      let the_return_value = trans_expr e the_builder the_scope the_cxt in
      (* #TODO: null expr not right. *)
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
      let bool_val = trans_expr w.sws_condition pred_builder the_scope the_cxt in
      (*create merge block*)
      let merge_bb = L.append_block the_context "merge" the_function in
      (* depends on bool_val, branches to body_bb or merge_bb*)
      let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
      L.builder_at_end the_context merge_bb
    
    | S_STMT_if s -> 
      let pred_e = s.sie_condition in
      let true_block =s.sie_true_branch in
      let false_block = s.sie_false_branch in
      let pred_val = trans_expr pred_e the_builder the_scope the_cxt in
      let merge_bb = L.append_block the_context "merge" the_function in
      let branch_instr = L.build_cond_br pred_val in
      let true_bb = L.append_block the_context "true" the_function in
      let true_builder = trans_stmt (S_STMT_block true_block) (L.builder_at_end the_context true_bb) the_scope the_cxt in
      let () = add_terminator true_builder (L.build_br merge_bb) in
      let false_bb = L.append_block the_context "false" the_function in
      let false_builder = trans_stmt (S_STMT_block false_block) (L.builder_at_end the_context false_bb) the_scope the_cxt in
      let () = add_terminator false_builder (L.build_br merge_bb) in
      let _ = L.build_cond_br pred_val true_bb false_bb the_builder in
      L.builder_at_end the_context merge_bb

    | _ -> todo "trans_stmt") in

  (**
    take a s_function in and get a LLVM function in llvalue type

    author: Yuanfei
  *)
  let trans_function 
    (key: string)
    (the_entry: s_symbol_table_entry) =
    match the_entry with
    | FuncEntry f ->
      let the_name=f.sf_name in
      (* let () = print_string the_name in *)
      let search_result =
        let the_entry = lookup the_name (IRGlobalScope the_global_scope) in
        match the_entry with
        | Some(IRFuncEntry (IRRoocFunction (f))) -> f
        | _ -> bug "Want irf_function but get others."
      in
      let the_function = search_result.irf_function in
      let the_scope = search_result.irf_scope in
      let the_builder = L.builder_at_end the_context (L.entry_block the_function) in
      let the_cxt={
        cur_function=the_function;
        cur_stack_base_ptr=None;
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
          iv_stype=param.sv_type;
        } in
        (* Printf.fprintf stdout "inserting %s in %s\n" local_name the_name; *)
          insert_variable_local local_name local_var the_scope;
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
            the_global_scope =
            try
              let trans_func = Hashtbl.find builtins_map the_name in
              trans_func the_context the_builder the_scope the_global_scope
            with
            | Not_found -> bug "not supported built-in function" 
          in 
          translate_builtin_func the_name the_builder the_scope the_global_scope;) in

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
      | ST_struct s_name ->
        let the_struct_type = 
          match lookup_type s_name the_type_env with
          | Some t -> t
          | None -> raise (LLIRError "struct type not found") in
        L.build_ret (L.const_null the_struct_type)
      | ST_string ->
        L.build_ret (L.const_null str_t)
      | _ -> todo "other return type")

    | _ -> ()
  in

  (**
    from s_struct_fields to array of lltype;
    construct the mapping from field name to the type in struct body
    construcrt all the info in our ir type: llir_struct.

    author: Yuanfei
  *)
  let trans_struct
    (key: string)
    (the_entry: s_symbol_table_entry)
    : unit =
    match the_entry with
    | StructEntry sast_struct ->
      let the_struct_name = sast_struct.ss_name in
      let the_struct_type = 
        match lookup_type the_struct_name the_type_env with
        | Some t -> t
        | None -> raise (LLIRError "struct type not found")
      in
      let the_field_list = sast_struct.ss_fields in
      (* construct the field to index map. *)
      let create_fields_index_map 
      (field_list: s_struct_field list) 
      : (string,int) Hashtbl.t =
        let map : (string, int) Hashtbl.t = Hashtbl.create 10 in
        let () = List.iteri (fun index field ->
                 Hashtbl.add map field.ssf_name index) field_list in 
          map in 
      let the_fields_index_map = create_fields_index_map the_field_list in
      let trans_field 
        ({ssf_name=name;
          ssf_type=stype; }:s_struct_field)
        : llir_struct_field =
        let ltype=trans_type stype in
        let ind = match Hashtbl.find_opt the_fields_index_map name with
          | Some i -> i
          | None -> raise (LLIRError "field corresponding index not found") in
          { lsf_name=name;
            lsf_type=ltype;
            lsf_stype = stype;
            lsf_index=ind; } in
      let llfields = Hashtbl.create 10 in 
      let () = List.iter (fun field -> Hashtbl.add llfields 
               field.ssf_name (trans_field field))the_field_list in
      (* get the field_type *)
      let get_struct_body_types
        (field_list: s_struct_field list)
        : L.lltype list =
        List.map 
        (fun field -> let the_ty=field.ssf_type in trans_type the_ty) 
        field_list in 
      let the_struct_body_types = get_struct_body_types the_field_list in
      (* define struct body in LLVM IR *)
      let the_struct_body = Array.of_list the_struct_body_types in
      let () = L.struct_set_body the_struct_type the_struct_body false in
      (* create the llir_struct type *)
      let the_llir_struct = {
        ls_name = the_struct_name;
        ls_fields_index_map = the_fields_index_map;
        ls_llfields = llfields; } in
        insert_struct the_struct_name the_llir_struct the_global_scope
    | _ -> () in

  (**
    register struct    

    author: Yuanfei
  *)
  let register_struct
    (key: string)
    (to_register: s_symbol_table_entry)
    : unit =
    match to_register with
    | StructEntry s -> 
      let the_struct_type = L.named_struct_type the_context s.ss_name in
      register_type s.ss_name the_struct_type the_type_env
    | _ -> ()
  in

  (**
    pre-fill `the namespace` for following code generation.

    author: Yuanfei
  *)
  let register_function 
    (key: string) 
    (to_register:s_symbol_table_entry)
    : unit =
    match to_register with
    (* put the type, llvm function value and the scope into the wrapper. *)
    | FuncEntry f ->         
      (* let () = print_string(string_of_stype (f.sf_type.sft_return_type)) in (* #DEBUG *) *)
      let llvm_return_type = trans_type f.sf_type.sft_return_type in
      let llvm_param_types = Array.of_list (List.map trans_type f.sf_type.sft_params_type) in
      let llvm_function_type = L.function_type llvm_return_type llvm_param_types in
      let llvm_function = L.define_function f.sf_name llvm_function_type the_module in
      let llvm_function_scope = init_local_scope (IRGlobalScope the_global_scope) in
      let the_function = {
        irf_return_type=f.sf_type.sft_return_type;
        irf_param_types=f.sf_type.sft_params_type;
        irf_function_type=llvm_function_type;
        irf_function=llvm_function;  
        irf_scope=llvm_function_scope;
      }
      in insert_function f.sf_name (IRRoocFunction the_function) the_global_scope
    | _ -> () 
  in 
    List.iter (fun f -> f the_context the_module the_global_scope) external_functions;
    (* need to register struct first. *)
    Hashtbl.iter register_struct to_trans.sm_namespace.sst_symbols;
    Hashtbl.iter trans_struct to_trans.sm_namespace.sst_symbols;

    Hashtbl.iter register_function to_trans.sm_namespace.sst_symbols;
    Hashtbl.iter trans_function to_trans.sm_namespace.sst_symbols;
    the_module