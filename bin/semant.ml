open Ast
open Sast
open Util
open Builtins


let analyse_module (ast_root:roc_module) : s_module =
  (**
    #TODO: add docstring
      *)
  let analyse_type (raw_type: roc_type) : s_type =
    match raw_type with
    | T_unit -> ST_unit
    | T_int -> ST_int
    | T_float -> ST_float
    | T_bool -> ST_bool
    | T_string -> ST_string
  in

  (** 
    take a ast node `roc_expr` and current scope's symbol table, 
    do the semantic analysis on it. return the analysed sast node `s_expr`.
  *)
  let rec analyse_expr (raw_expr: roc_expr) (symbol_table: s_symbol_table) : s_expr =
    match raw_expr with
    (* empty expression *)
    | EXPR_null -> { se_type = ST_unit; se_expr = SEXPR_null }

    (* literals *)
    | Roc_string_literal strl -> {se_type = ST_string; se_expr = S_string_literal strl}
    | Roc_int_literal intl -> { se_type = ST_int; se_expr = S_int_literal intl }
    | Roc_float_literal fltl -> { se_type = ST_float; se_expr = S_float_literal fltl }
    | Roc_bool_literal booll -> { se_type = ST_bool; se_expr = S_bool_literal booll }

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
      { se_type = result_type; se_expr = S_unary_expr (op, analysed_expr) }

    (* Binary Expression *)
    | Roc_arith_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 symbol_table in
        let analysed_e2 = analyse_expr e2 symbol_table in
        let result_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_int, ST_int) -> ST_int
          | (ST_float, ST_float) -> ST_float
          | _ -> raise (type_err_failure "Arithmetic expression type mismatch")
            (* ST_error   *)
        in
        { se_type = result_type; se_expr = S_arith_expr (op, analysed_e1, analysed_e2) }
    | Roc_logical_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 symbol_table in
        let analysed_e2 = analyse_expr e2 symbol_table in
        let result_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_bool, ST_bool) -> ST_bool
          | _ -> raise (type_err_failure "Logical expression type mismatch")
            (* ST_error *)
        in
        { se_type = result_type; se_expr = S_logical_expr (op, analysed_e1, analysed_e2) }
    | Roc_comparison_expr (op, e1, e2) ->
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let result_type = match (analysed_e1.se_type, analysed_e2.se_type) with
        | (ST_int, ST_int) | (ST_float, ST_float) -> ST_bool
        | _ -> raise (type_err_failure "Comparison expression type mismatch")
          (* ST_error *)
      in
      { se_type = result_type; se_expr = S_comparison_expr (op, analysed_e1, analysed_e2) }
        
    | Roc_assignment_expr (e1, e2) ->
      (* //TODO: more check for left hand side*)
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let result_type = if analysed_e1.se_type = analysed_e2.se_type then analysed_e1.se_type else ST_error
      in
      { se_type = result_type; se_expr = S_assignment_expr (analysed_e1, analysed_e2) }

    | Roc_path_expr path -> 
      todo "path expr"
    
    | Roc_call_expr (callee, arg_list) ->
      todo "call expr"
    
    | Roc_grouped_expr e ->
        let analysed_expr = analyse_expr e symbol_table in
        { se_type = analysed_expr.se_type; se_expr = S_grouped_expr analysed_expr }
    
  (**
    #TODO: add docstring
      *)
  and analyse_stmt 
  (to_analyse: roc_stmt) 
  (the_table: s_symbol_table) 
  : s_stmt = 
    match to_analyse with
    | Roc_expr_stmt expr -> 
      let analysed_expr = analyse_expr expr the_table in
      S_expr_stmt analysed_expr
    | Roc_var_decl_stmt var_decl -> 
      let var_name = var_decl.rv_name in
      let analysed_var = analyse_variable var_decl true the_table in
      (* Add to symbol table *)
      let () = insert_symbol the_table var_name (VarEntry analysed_var) in
      S_var_decl_stmt analysed_var
    | Roc_let_decl_stmt let_decl ->
      let let_name = let_decl.rv_name in
      let analysed_let = analyse_variable let_decl false the_table in
      (* Add to symbol table *)
      let () = insert_symbol the_table let_name (VarEntry analysed_let) in
      S_let_decl_stmt analysed_let

    | Roc_return_stmt e ->
        let analysed_expr = analyse_expr e the_table in
        SSTMT_return analysed_expr

    
    | STMT_block b ->
        let analysed_block = analyse_block b the_table true in
        SSTMT_block analysed_block

    | Roc_for_stmt (init, cond, update, body) ->
      todo "for expr"

    | Roc_while_stmt (cond, body) ->
      todo "while expr"

    | Roc_break_stmt ->
      SSTMT_break
    
    | Roc_continue_stmt ->
      SSTMT_continue

    | Roc_if_stmt (cond, then_branch, else_branch) ->
      todo "if expr"
    
    (* //TODO: redesign how to deal with this one.
    | Roc_null_expr ->
        { se_type = ST_unit; se_expr = S_null_expr } *)

  (**
    if the `init_new_table` is true, the function will create a new symbol table with `table_in` as its parent,
    otherwise, the function will use the `table_in` as the table for the block.
      *)
  and analyse_block 
    ({rb_stmts:roc_stmt list}) 
    (table_in: s_symbol_table) 
    (init_new_table:bool) 
    : s_block =
    let the_table = 
      if init_new_table then 
        init_symbol_table ~parent:table_in () 
      else table_in
    in
    let to_analyse = rb_stmts in
    (* #TODO: also need a function wrapper here *)
    let analysed_stmts = List.map (fun stmt -> analyse_stmt stmt the_table) to_analyse in
    { sb_stmts = analysed_stmts; 
      sb_scope = the_table }

  and analyse_variable 
    (raw_variable: roc_variable) 
    (is_mutable: bool) 
    (symbol_table: s_symbol_table) 
    : s_variable =
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
      | _ -> 
        todo "not yet supported item.")
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
    let body_to_analyse = raw_func.rf_body in
    let analysed_body = UserDefined (analyse_block body_to_analyse function_scope false) in
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
      | _ -> 
        todo "not yet supported item.")
    ) ast_root.rm_items
  in

  let the_namespace = init_symbol_table () in
  (* Insert builtins *)
  List.iter (fun builtin -> 
    let name = builtin.sf_name in
    let entry = FuncEntry builtin in
    insert_symbol the_namespace name entry) builtins_semant;
  (* register items *)
  register_items ast_root the_namespace;
  (match lookup_symbol "main" the_namespace with
  | None -> raise (SymbolTableError "main function not found")
  | _ -> ());
  (* special check for main *)
  (* analyse items *)
  analyse_items ast_root the_namespace;

  { sm_namespace = the_namespace}

