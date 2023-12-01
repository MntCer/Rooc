open Ast
open Sast
open Util
open Builtins

exception SymbolTableError of string

let init_symbol_table ?parent () : s_symbol_table =
  let symbol_table = Hashtbl.create 10 in  (* Arbitrary initial size *)
  { sst_parent = parent; sst_symbols = symbol_table }

let rec lookup_symbol identifier symbol_table =
  match Hashtbl.find_opt symbol_table.sst_symbols identifier with
  | Some entry -> Some entry
  | None ->(
      match symbol_table.sst_parent with
      | Some parent_table -> lookup_symbol identifier parent_table  
      | None -> None  )

let insert_symbol symbol_table identifier entry =
  (* Check for existence in the current scope only *)
  if Hashtbl.mem symbol_table.sst_symbols identifier then
    raise (SymbolTableError ("Symbol already exists in the same scope: " ^ identifier))
  else
    Hashtbl.add symbol_table.sst_symbols identifier entry

let update_symbol_table symbol_table identifier new_entry =
  if Hashtbl.mem symbol_table.sst_symbols identifier then
    Hashtbl.replace symbol_table.sst_symbols identifier new_entry
  else
    raise (SymbolTableError ("Symbol not found for update: " ^ identifier))

let get_block_from_expr (expr: s_expr) : s_block_expr =
  match expr.se_content with
  | S_block_expr block -> block
  | _ -> raise (Failure "Not a block expression")


let analyse_module (ast_root:roc_module) : s_module =
  let analyse_type (raw_type: roc_type) : s_type =
    match raw_type with
    | T_int -> ST_int
    | T_float -> ST_float
    | T_bool -> ST_bool
    | T_string -> ST_string
    | T_unit -> ST_unit
  in
  let rec analyse_expr (raw_expr: roc_expr) (symbol_table: s_symbol_table) : s_expr =
    match raw_expr with
    | Roc_string_literal strl -> {se_type = ST_string; se_content = S_string_literal strl}
    | Roc_int_literal intl -> { se_type = ST_int; se_content = S_int_literal intl }
    | Roc_float_literal fltl -> { se_type = ST_float; se_content = S_float_literal fltl }
    | Roc_bool_literal booll -> { se_type = ST_bool; se_content = S_bool_literal booll }

    (* Unary Expression *)
    | Roc_unary_expr (op, e) ->
      let analysed_expr = analyse_expr e symbol_table in
      let result_type = match (op, analysed_expr.se_type) with
        | (Neg, ST_int) -> ST_int
        | (Neg, ST_float) -> ST_float
        | (Not, ST_bool) -> ST_bool
        | _ -> raise (type_err_failure "Unary expression type mismatch")
          (* ST_error  *)
      in
      { se_type = result_type; se_content = S_unary_expr (op, analysed_expr) }

    (* Arithmetic and Logical Expressions *)
    | Roc_arith_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 symbol_table in
        let analysed_e2 = analyse_expr e2 symbol_table in
        let result_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_int, ST_int) -> ST_int
          | (ST_float, ST_float) -> ST_float
          | _ -> raise (type_err_failure "Arithmetic expression type mismatch")
            (* ST_error   *)
        in
        { se_type = result_type; se_content = S_arith_expr (op, analysed_e1, analysed_e2) }
    | Roc_logical_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 symbol_table in
        let analysed_e2 = analyse_expr e2 symbol_table in
        let result_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_bool, ST_bool) -> ST_bool
          | _ -> raise (type_err_failure "Logical expression type mismatch")
            (* ST_error *)
        in
        { se_type = result_type; se_content = S_logical_expr (op, analysed_e1, analysed_e2) }

    | Roc_comparison_expr (op, e1, e2) ->
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let result_type = match (analysed_e1.se_type, analysed_e2.se_type) with
        | (ST_int, ST_int) | (ST_float, ST_float) -> ST_bool
        | _ -> raise (type_err_failure "Comparison expression type mismatch")
          (* ST_error *)
      in
      { se_type = result_type; se_content = S_comparison_expr (op, analysed_e1, analysed_e2) }
        
    | Roc_assignment_expr (e1, e2) ->
      (* //TODO: more check for left hand side*)
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let result_type = if analysed_e1.se_type = analysed_e2.se_type then analysed_e1.se_type else ST_error
      in
      { se_type = result_type; se_content = S_assignment_expr (analysed_e1, analysed_e2) }

    | Roc_path_expr path -> 
      raise (todo_failure "path expr")
    
    | Roc_call_expr (callee, arg_list) ->
      raise (todo_failure "call expr")
    
    | Roc_grouped_expr e ->
        let analysed_expr = analyse_expr e symbol_table in
        { se_type = analysed_expr.se_type; se_content = S_grouped_expr analysed_expr }
    
    | Roc_return_expr e ->
        let analysed_expr = analyse_expr e symbol_table in
        { se_type = analysed_expr.se_type; se_content = S_return_expr analysed_expr }
    
    | Roc_block_expr stmts ->
        let analysed_block = analyse_block_expr stmts symbol_table true in
        { se_type = ST_unit; se_content = S_block_expr analysed_block }

    | Roc_for_expr (init, cond, update, body) ->
        raise (todo_failure "for expr")

    | Roc_while_expr (cond, body) ->
        raise (todo_failure "while expr")

    | Roc_break_expr ->
        { se_type = ST_unit; se_content = S_break_expr }
    
    | Roc_continue_expr ->
        { se_type = ST_unit; se_content = S_continue_expr }

    | Roc_if_expr (cond, then_branch, else_branch) ->
        raise (todo_failure "if expr")
    
    (* //TODO: redesign how to deal with this one.
    | Roc_null_expr ->
        { se_type = ST_unit; se_content = S_null_expr } *)
  and analyse_block_expr (raw_stmts: roc_stmt list) (symbol_table: s_symbol_table) (init_new_table:bool) : s_block_expr =
    let table_for_the_block = 
      if init_new_table then 
        init_symbol_table ~parent:symbol_table () 
      else symbol_table 
    in
    let analysed_stmts = List.rev (List.fold_left (fun acc stmt ->
      match stmt with
      | Roc_expr_stmt expr -> 
          let analysed_expr = analyse_expr expr table_for_the_block in
          (S_expr_stmt analysed_expr)::acc 
      | Roc_var_decl_stmt var_decl -> 
        let var_name = var_decl.rv_name in
        let analysed_var = analyse_variable var_decl true table_for_the_block in
        (* Add to symbol table *)
        insert_symbol table_for_the_block var_name (VarEntry analysed_var);
        let analysed_var = S_var_decl_stmt analysed_var in 
        analysed_var::acc
      | Roc_let_decl_stmt let_decl ->
        let let_name = let_decl.rv_name in
        let analysed_let = analyse_variable let_decl false table_for_the_block in
        (* Add to symbol table *)
        insert_symbol table_for_the_block let_name (VarEntry analysed_let);
        let analysed_let = S_let_decl_stmt analysed_let in
        analysed_let::acc
      | Roc_empty_stmt -> 
          acc  (* Skip empty statements *)
    ) [] raw_stmts)
    in
    { sbe_stmts = analysed_stmts; sbe_scope = table_for_the_block }

  and analyse_variable (raw_variable: roc_variable) (is_mutable: bool) (symbol_table: s_symbol_table) : s_variable =
    let analysed_name = raw_variable.rv_name in
    let analysed_type = analyse_type raw_variable.rv_type in
    let analysed_initial_value = 
      Option.map (fun expr -> analyse_expr expr symbol_table) raw_variable.rv_initial_expr in
    { sv_name = analysed_name; 
      sv_type = analysed_type; 
      sv_mutable = is_mutable;
      sv_initial_value = analysed_initial_value }
  in

  let analyse_params (raw_params: roc_params) (symbol_table: s_symbol_table) : s_params =
    let analysed_params = List.map (fun param -> analyse_variable param true symbol_table) raw_params.rp_params in
    { sp_params = analysed_params }
  in

  let analyse_params_type (raw_params: roc_params) (symbol_table: s_symbol_table) : s_type list =
    List.map (fun param -> analyse_type param.rv_type) raw_params.rp_params
  in

  let register_function (raw_func: roc_function) (symbol_table: s_symbol_table) : s_function_signature =
    let analysed_name = raw_func.rf_name in
    let raw_params =  raw_func.rf_params in
    let analysed_params = 
      (match raw_params with
      | None -> None
      | Some params -> Some (analyse_params params symbol_table))
    in
    let analysed_type = 
      (match raw_params with
      | None -> 
        { sft_params_type = []; 
          sft_return_type = analyse_type raw_func.rf_return_type } 
      | Some params -> 
        let analysed_params_type = analyse_params_type params symbol_table in
        { sft_params_type = analysed_params_type; 
          sft_return_type = analyse_type raw_func.rf_return_type; } )
    in
    { sfs_name = analysed_name;
      sfs_params = analysed_params;
      sfs_type = analysed_type; }
  in

  let register_items (ast_root: roc_module) (symbol_table: s_symbol_table) : unit =
    List.iter (fun item ->
      (match item with
      | FunctionItem func -> 
        let func_sig = register_function func symbol_table in
        insert_symbol symbol_table func_sig.sfs_name (FuncSigEntry func_sig)
      | _ -> raise (todo_failure "not yet supported item."))
    ) ast_root.rm_items
  in

  let analyse_function (raw_func: roc_function) (symbol_table: s_symbol_table) : s_function =
    let analysed_name = raw_func.rf_name in

    let raw_params =  raw_func.rf_params in
    let analysed_params = 
      (match raw_params with
      | None -> None
      | Some params -> Some (analyse_params params symbol_table))
    in
    let analysed_type = 
      (match raw_params with
      | None -> 
        { sft_params_type = []; 
          sft_return_type = analyse_type raw_func.rf_return_type } 
      | Some params -> 
        let analysed_params_type = analyse_params_type params symbol_table in
        { sft_params_type = analysed_params_type; 
          sft_return_type = analyse_type raw_func.rf_return_type; } )
    in
    let param_vars = 
      (match analysed_params with
      | None -> []
      | Some params -> params.sp_params) in 
    let function_scope = init_symbol_table ~parent:symbol_table () in
    List.iter (fun param -> 
      let param_name = param.sv_name in
      let param_entry = VarEntry param in
      insert_symbol function_scope param_name param_entry) param_vars;
    let block_content = match raw_func.rf_body with
      | Roc_block_expr block -> block
      | _ -> raise (todo_failure "function body is not a block") in
    let analysed_body = UserDefined (analyse_block_expr block_content function_scope false) in
    { sf_name = analysed_name;
      sf_params = analysed_params;
      sf_type = analysed_type;
      sf_body = analysed_body; }
  in

  let analyse_items (ast_root:roc_module) (symbol_table: s_symbol_table) : unit =
    List.iter (fun item ->
      (match item with
      | FunctionItem func -> 
        let analysed_func = analyse_function func symbol_table in
        update_symbol_table symbol_table analysed_func.sf_name (FuncEntry analysed_func)
      | _ -> raise (todo_failure "not yet supported item."))
    ) ast_root.rm_items
  in

  let the_namespace = init_symbol_table () in
  (* Insert builtins *)
  List.iter (fun builtin -> 
    let name = builtin.sf_name in
    let entry = FuncEntry builtin in
    insert_symbol the_namespace name entry) builtins;
  register_items ast_root the_namespace;
  (* special treatment for main*)
  analyse_items ast_root the_namespace;

  
  { sm_namespace = the_namespace}



