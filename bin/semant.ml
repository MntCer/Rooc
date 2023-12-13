open Ast
open Sast
open Util
open Builtins


let analyse_module (the_module:roc_module) : s_module =
  (**
    map each raw type to its corresponding semantic type
      *)
  let analyse_type (raw_type: r_type) : s_type =
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
  let rec analyse_expr 
    (raw_expr: roc_expr) 
    (symbol_table: s_symbol_table) 
    : s_expr =
    match raw_expr with
    (* empty expression *)
    | EXPR_null -> { se_type = ST_unit; se_expr = S_EXPR_null }

    (* literals *)
    | Roc_string_literal strl -> {se_type = ST_string; se_expr = S_string_literal strl}
    | Roc_int_literal intl -> { se_type = ST_int; se_expr = S_int_literal intl }
    | Roc_float_literal fltl -> { se_type = ST_float; se_expr = S_float_literal fltl }
    | Roc_bool_literal booll -> { se_type = ST_bool; se_expr = S_bool_literal booll }

    (* Unary Expression *)
    | Roc_unary_expr (op, e) ->
      let analysed_expr = analyse_expr e symbol_table in
      let analysed_type = match (op, analysed_expr.se_type) with
        | (Neg, ST_int) -> ST_int
        | (Neg, ST_float) -> ST_float
        | (Not, ST_bool) -> ST_bool
        | _ -> raise (type_err_failure "Unary expression type mismatch")
          (* ST_error  *)
      in
      { se_type = analysed_type; 
        se_expr = S_unary_expr (op, analysed_expr) }

    (* Binary Arithmetic Expression *)
    | Roc_arith_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 symbol_table in
        let analysed_e2 = analyse_expr e2 symbol_table in
        let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_int, ST_int) -> ST_int
          | (ST_float, ST_float) -> ST_float
          | _ -> raise (type_err_failure "Arithmetic expression type mismatch")
            (* ST_error   *)
            (*TODO: string concatenation as a built-in function?*)
        in
        { se_type = analysed_type; 
          se_expr = S_arith_expr (op, analysed_e1, analysed_e2) }

    (* Binary Logical Expression *)
    | Roc_logical_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 symbol_table in
        let analysed_e2 = analyse_expr e2 symbol_table in
        let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_bool, ST_bool) -> ST_bool
          | _ -> raise (type_err_failure "Logical expression type mismatch")
            (* ST_error *)
        in
        { se_type = analysed_type; 
          se_expr = S_logical_expr (op, analysed_e1, analysed_e2) }

    (* Binary Comparison Expression *)
    | Roc_comparison_expr (op, e1, e2) ->
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
        | (ST_int, ST_int) | (ST_float, ST_float) | (ST_bool, ST_bool) -> ST_bool (*handle string?*)
        | _ -> raise (type_err_failure "Comparison expression type mismatch")
          (* ST_error *)
      in
      { se_type = analysed_type; 
        se_expr = S_comparison_expr (op, analysed_e1, analysed_e2) }
        
    | Roc_assignment_expr (e1, e2) ->
      let check_lval (expr: roc_expr) : unit =
        match expr with
        (* #TODO: are there some another possiblities? *)
        | EXPR_path _ -> ()
        | _ -> raise (SymbolTableError "left hand side of assignment is not a lvalue")
      in
      let () = check_lval e1 in
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let analysed_type = 
        if analysed_e1.se_type = analysed_e2.se_type 
        then analysed_e1.se_type 
        else raise (type_err_failure "Assignment type mismatch")
      in
      { se_type = analysed_type; 
        se_expr = S_assignment_expr (analysed_e1, analysed_e2) }

    (**
      Analyse the params, and ensure the callee is a existing function. 
      Use the function's return type as the type of this call expression.     
    *)
    | Roc_call_expr (callee, arg_list) ->
      let analysed_args = 
        List.map (fun arg -> analyse_expr arg symbol_table) arg_list 
      in
      let search_result = lookup_symbol callee symbol_table in
      (match search_result with
      | None -> raise (SymbolTableError "callee not found")
      | Some(VarEntry v) -> raise (SymbolTableError "callee is a variable")
      | Some(FuncEntry f) -> 
        (* function arguments type checking *)
        let expected_param_types = f.sf_type.sft_params_type in
        (* if the number of arguments matches the number of parameters *)
          if List.length expected_param_types <> List.length analysed_args then
            raise (type_err_failure "Incorrect number of arguments in function call")
        (* if each argument type matches the expected parameter type *)
          List.iter2 (fun expected_type expr ->
            if expected_type <> expr.se_type then
              raise (type_err_failure "Argument type mismatch in function call")
              ) expected_param_types analysed_args;
        (* extract return type*)
        let analysed_type = f.sf_type.sft_return_type in
        let analysed_callee = callee in
        { se_type = analysed_type; 
          se_expr = S_EXPR_call({
            sc_callee = analysed_callee;
            sc_arguments = analysed_args; }) }
      | Some (FuncSigEntry fs) ->
        let analysed_type = fs.sfs_type.sft_return_type in
        let analysed_callee = callee in 
        { se_type = analysed_type; 
          se_expr = S_EXPR_call({
            sc_callee = analysed_callee;
            sc_arguments = analysed_args; }) } )
    
    | Roc_grouped_expr e ->
      let analysed_expr = analyse_expr e symbol_table in
      { se_type = analysed_expr.se_type; 
        se_expr = S_grouped_expr analysed_expr}

    | EXPR_field_access (struct_name, field_name) ->
      let () = todo "field access expr" in 
      { se_type = ST_unit; (* #TODO: struct type *)
        se_expr = S_EXPR_field_access (struct_name, field_name); }

    | EXPR_path (var_name) ->
      match lookup_symbol var_name symbol_table with
      | None -> raise (SymbolTableError "variable not found in symbol table")
      | Some (VarEntry v) -> 
        let analysed_type = v.sv_type in
        { se_type = analysed_type; 
          se_expr = S_EXPR_path var_name; }
      | _ -> raise (SymbolTableError "Not a variable in symbol table")

    
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
        S_STMT_return analysed_expr

    
    | STMT_block b ->
        let analysed_block = analyse_block b the_table true in
        S_STMT_block analysed_block

    | Roc_for_stmt (init, cond, update, body) ->
      todo "for stmt"

    | Roc_while_stmt (cond, body) ->
      todo "while stmt"

    | Roc_break_stmt ->
      S_STMT_break
    
    | Roc_continue_stmt ->
      S_STMT_continue

    | Roc_if_stmt (cond, then_branch, else_branch) ->
      todo "if stmt"
    
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
      match raw_variable.rv_initial_expr with
      | None -> None
      | Some expr -> Some (analyse_expr expr symbol_table)
    in
    let () = 
      match analysed_initial_value with
      | None -> ()
      | Some expr -> 
        if expr.se_type <> analysed_type then
          raise (type_err_failure "variable initial value type mismatch")
    in
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

  (**
    Analyse a function's name, params and return type and construct them into a `s_function_signature` type. 
    Register this signature into symbol table for the following semantic analysis.
  *)
  let register_function 
  (raw_func: roc_function) 
  (symbol_table: s_symbol_table) 
  : s_function_signature =
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

  let register_struct 
  (raw_struct: r_struct) 
  (symbol_table: s_symbol_table) 
  : s_struct_sig =
    let analysed_name = raw_struct.rs_name in
    (* Not register type here, because needs inner analysis. *)
    { sss_name = analysed_name;}

  in

  let register_items (the_module: roc_module) (symbol_table: s_symbol_table) : unit =
    List.iter (fun item ->
      (match item with
      | FunctionItem the_function -> 
        let func_sig = register_function the_function symbol_table in
        insert_symbol symbol_table func_sig.sfs_name (FuncSigEntry func_sig)
      | StructItem the_struct ->
        let struct_sig = register_struct the_struct symbol_table in
        insert_symbol symbol_table struct_sig.sss_name (StructSigEntry struct_sig)
        
      | _ -> 
        todo "not yet supported item.")
    ) the_module.rm_items
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

  let analyse_struct 
    (raw_struct: r_struct) 
    (symbol_table: s_symbol_table) 
    : s_struct =
    let analysed_name = raw_struct.rs_name in
    (* register type here. *)

    todo "analyse struct"
  in

  let analyse_items (the_module:roc_module) (symbol_table: s_symbol_table) : unit =
    List.iter (fun item ->
      (match item with
      | FunctionItem func -> 
        let analysed_func = analyse_function func symbol_table in
        update_symbol_table symbol_table analysed_func.sf_name (FuncEntry analysed_func)

      | StructItem the_struct ->
        let analysed_struct = analyse_struct the_struct symbol_table in
        update_symbol_table symbol_table analysed_struct.ss_name (StructEntry analysed_struct)

      | _ -> 
        todo "not yet supported item.")
    ) the_module.rm_items
  in

  let the_namespace = init_symbol_table () in

  (* Insert builtins *)
  List.iter (fun builtin -> 
    let name = builtin.sf_name in
    let entry = FuncEntry builtin in
    insert_symbol the_namespace name entry) builtins_semant;

  (* register items *)
  register_items the_module the_namespace;

  (* special check for main *)
  (match lookup_symbol "main" the_namespace with
  | None -> raise (SymbolTableError "main function not found")
  | Some (FuncSigEntry f) -> 
    let the_return_type = f.sfs_type.sft_return_type in
    (match the_return_type with
    | ST_int -> ()
    | _ -> bug "main function's return type is not int")
  | _ -> raise (SymbolTableError "main is not a function"));

  
  (* analyse items *)
  analyse_items the_module the_namespace;

  { sm_namespace = the_namespace}

