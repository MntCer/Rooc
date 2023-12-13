open Ast
open Sast
open Util
open Builtins


type semant_cxt ={
  sc_namespace: s_symbol_table;
  sc_type_env: (string, s_type) Hashtbl.t;
  sc_current_sb: s_symbol_table;
}


let analyse_module 
  (the_module:roc_module) 
  : s_module =

  (**************************************************************
     Some helper functions.
  **************************************************************)


  (**
    #TODO: add docstring
      *)
  let analyse_type 
  (raw_type: r_type)
  (the_cxt: semant_cxt)
  : s_type =
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
    (the_cxt: semant_cxt) 
    : s_expr =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
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
      let analysed_expr = analyse_expr e the_cxt in
      let analysed_type = match (op, analysed_expr.se_type) with
        | (Neg, ST_int) -> ST_int
        | (Neg, ST_float) -> ST_float
        | (Not, ST_bool) -> ST_bool
        | _ -> raise (type_err_failure "Unary expression type mismatch")
          (* ST_error  *)
      in
      { se_type = analysed_type; 
        se_expr = S_unary_expr (op, analysed_expr) }

    (* Binary Expression *)
    | Roc_arith_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 the_cxt in
        let analysed_e2 = analyse_expr e2 the_cxt in
        let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_int, ST_int) -> ST_int
          | (ST_float, ST_float) -> ST_float
          | _ -> raise (type_err_failure "Arithmetic expression type mismatch")
            (* ST_error   *)
        in
        { se_type = analysed_type; 
          se_expr = S_arith_expr (op, analysed_e1, analysed_e2) }

    | Roc_logical_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 the_cxt in
        let analysed_e2 = analyse_expr e2 the_cxt in
        let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_bool, ST_bool) -> ST_bool
          | _ -> raise (type_err_failure "Logical expression type mismatch")
            (* ST_error *)
        in
        { se_type = analysed_type; 
          se_expr = S_logical_expr (op, analysed_e1, analysed_e2) }

    | Roc_comparison_expr (op, e1, e2) ->
      let analysed_e1 = analyse_expr e1 the_cxt in
      let analysed_e2 = analyse_expr e2 the_cxt in
      let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
        | (ST_int, ST_int) | (ST_float, ST_float) | (ST_bool, ST_bool) -> ST_bool
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
      let analysed_e1 = analyse_expr e1 the_cxt in
      let analysed_e2 = analyse_expr e2 the_cxt in
      let analysed_type = 
        if analysed_e1.se_type = analysed_e2.se_type 
        then analysed_e1.se_type 
        else bug "assignment type mismatch"
      in
      { se_type = analysed_type; 
        se_expr = S_assignment_expr (analysed_e1, analysed_e2) }

    (**
      Analyse the params, and ensure the callee is a existing function. 
      Use the function's return type as the type of this call expression.     
    *)
    | Roc_call_expr (callee, arg_list) ->
      let analysed_args = 
        List.map (fun arg -> analyse_expr arg the_cxt) arg_list 
      in
      let search_result = lookup_symbol callee the_symbol_table in
      (match search_result with
      | None -> raise (SymbolTableError "callee not found")
      | Some(VarEntry v) -> raise (SymbolTableError "callee is a variable")
      (* #TODO: need to do arguments type checking. *)
      | Some(FuncEntry f) -> 
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
      let analysed_expr = analyse_expr e the_cxt in
      { se_type = analysed_expr.se_type; 
        se_expr = S_grouped_expr analysed_expr}

    | EXPR_field_access (struct_name, field_name) ->
      let () = todo "field access expr" in 
      { se_type = ST_unit; (* #TODO: struct type *)
        se_expr = S_EXPR_field_access (struct_name, field_name); }

    | EXPR_path (var_name) ->
      match lookup_symbol var_name the_symbol_table with
      | None -> raise (SymbolTableError "variable not found")
      | Some (VarEntry v) -> 
        let analysed_type = v.sv_type in
        { se_type = analysed_type; 
          se_expr = S_EXPR_path var_name; }
      | _ -> bug "not a variable"

    
  (**
    #TODO: add docstring
      *)
  and analyse_stmt 
  (to_analyse: roc_stmt)
  (the_cxt: semant_cxt)
  : s_stmt = 
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    match to_analyse with
    | Roc_expr_stmt expr -> 
      let analysed_expr = analyse_expr expr the_cxt in
      S_expr_stmt analysed_expr

    | Roc_var_decl_stmt var_decl -> 
      let var_name = var_decl.rv_name in
      let analysed_var = analyse_variable var_decl true the_cxt in
      (* Add to symbol table *)
      let () = insert_symbol the_symbol_table var_name (VarEntry analysed_var) in
      S_var_decl_stmt analysed_var

    | Roc_let_decl_stmt let_decl ->
      let let_name = let_decl.rv_name in
      let analysed_let = analyse_variable let_decl false the_cxt in
      (* Add to symbol table *)
      let () = insert_symbol the_symbol_table let_name (VarEntry analysed_let) in
      S_let_decl_stmt analysed_let

    | Roc_return_stmt e ->
        let analysed_expr = analyse_expr e the_cxt in
        S_STMT_return analysed_expr

    
    | STMT_block b ->
        let analysed_block = analyse_block b the_cxt true in
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
    if the `init_new_table` is true, the function will create a new symbol table with current symbol table in context as its parent,
    otherwise, the function will use the current symbol table as the table for the block.
      *)
  and analyse_block 
    ({rb_stmts:roc_stmt list}) 
    (the_cxt: semant_cxt)
    (init_new_table:bool) 
    : s_block =
    (* determine which is "the table" *)
    let table_in = the_cxt.sc_current_sb in
    let the_namesapce = the_cxt.sc_namespace in
    let the_type_env = the_cxt.sc_type_env in
    let the_cxt = 
      if init_new_table then 
        let the_new_scope = init_symbol_table ~parent:table_in () in
        { sc_namespace = the_namesapce;
          sc_type_env = the_type_env;
          sc_current_sb = the_new_scope; }
      else the_cxt
    in
    (* Analyse *)
    let to_analyse = rb_stmts in
    (* #TODO: also need a function wrapper here *)
    let analysed_stmts = List.map (fun stmt -> analyse_stmt stmt the_cxt) to_analyse in
    { sb_stmts = analysed_stmts; 
      sb_scope = the_cxt.sc_current_sb; }

  and analyse_variable 
    (raw_variable: roc_variable) 
    (is_mutable: bool) 
    (the_cxt: semant_cxt)
    : s_variable =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_name = raw_variable.rv_name in
    let analysed_type = analyse_type raw_variable.rv_type the_cxt in
    let analysed_initial_value = 
      match raw_variable.rv_initial_expr with
      | None -> None
      | Some expr -> Some (analyse_expr expr the_cxt)
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

  let analyse_params 
    (raw_params: roc_params) 
    (the_cxt: semant_cxt)
    : s_params =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_params = 
      List.map 
        (fun param -> analyse_variable param true the_cxt) 
        raw_params.rp_params in
    { sp_params = analysed_params }
  in

  let analyse_params_type 
    (raw_params: roc_params) 
    (the_cxt: semant_cxt)
    : s_type list =
    List.map 
      (fun param -> analyse_type param.rv_type the_cxt) 
      raw_params.rp_params
  in

  (**
    Analyse a function's name, params and return type and construct them into a `s_function_signature` type. 
    Register this signature into symbol table for the following semantic analysis.
  *)
  let register_function 
  (raw_func: roc_function) 
  (the_cxt: semant_cxt)
  : unit =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_name = raw_func.rf_name in
    let raw_params =  raw_func.rf_params in
    let analysed_params = 
      (match raw_params with
      | None -> None
      | Some params -> Some (analyse_params params the_cxt))
    in
    (* #TODO: should I analyse type here? *)
    let analysed_type = 
      (match raw_params with
      | None -> 
        { sft_params_type = []; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } 
      | Some params -> 
        let analysed_params_type = analyse_params_type params the_cxt in
        { sft_params_type = analysed_params_type; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } )
    in
    let analysed_func_sig = 
      { sfs_name = analysed_name;
        sfs_params = analysed_params;
        sfs_type = analysed_type; }
    in
    insert_symbol the_symbol_table analysed_name (FuncSigEntry analysed_func_sig)
  in

  let register_struct 
  (raw_struct: r_struct) 
  (the_cxt: semant_cxt)
  : unit =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_name = raw_struct.rs_name in
    (* Not register type here, because needs inner analysis. *)
    let analysed_struct_sig = 
      { sss_name = analysed_name; }
    in
    insert_symbol the_symbol_table analysed_struct_sig.sss_name (StructSigEntry analysed_struct_sig)


  in

  let register_items 
    (the_module: roc_module) 
    (the_cxt: semant_cxt)
    : unit =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    List.iter (fun item ->
      (match item with
      | FunctionItem the_function -> 
        register_function the_function the_cxt
      | StructItem the_struct ->
        register_struct the_struct the_cxt
      | _ -> 
        todo "not yet supported item.")
    ) the_module.rm_items
  in

  let analyse_function 
    (raw_func: roc_function) 
    (the_cxt: semant_cxt)
    : s_function =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in

    let analysed_name = raw_func.rf_name in
    let raw_params =  raw_func.rf_params in
    let analysed_params = 
      (match raw_params with
      | None -> None
      | Some params -> Some (analyse_params params the_cxt))
    in
    let analysed_type = 
      (match raw_params with
      | None -> 
        { sft_params_type = []; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } 
      | Some params -> 
        let analysed_params_type = analyse_params_type params the_cxt in
        { sft_params_type = analysed_params_type; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } )
    in
    let param_vars = 
      (match analysed_params with
      | None -> []
      | Some params -> params.sp_params) in 
    (* create a new scope and accordingly new cxt *)
    let func_local_scope = init_symbol_table ~parent:the_symbol_table () in
    List.iter (fun param -> 
      let param_name = param.sv_name in
      let param_entry = VarEntry param in
      insert_symbol func_local_scope param_name param_entry) param_vars;
    let the_new_cxt = {
      sc_namespace = the_cxt.sc_namespace;
      sc_type_env = the_cxt.sc_type_env;
      sc_current_sb = func_local_scope;
    } in
    let body_to_analyse = raw_func.rf_body in
    let analysed_body = UserDefined (analyse_block body_to_analyse the_new_cxt false) in
    { sf_name = analysed_name;
      sf_params = analysed_params;
      sf_type = analysed_type;
      sf_body = analysed_body; }
  in

  let analyse_field 
    (raw_field: r_struct_field) 
    (the_cxt: semant_cxt)
    : s_struct_field =
    let analysed_name = raw_field.rsf_name in
    let analysed_type = analyse_type raw_field.rsf_type the_cxt in
    { ssf_name = analysed_name;
      ssf_type = analysed_type; }
  in

  let analyse_struct 
    (raw_struct: r_struct) 
    (the_cxt: semant_cxt)
    : s_struct =
    let analysed_name = raw_struct.rs_name in
    let raw_fields = raw_struct.rs_fields in
    let analysed_fields = 
      List.map 
      (fun field -> analyse_field field the_cxt) 
      raw_fields 
    in
    { ss_name = analysed_name;
      ss_fields = analysed_fields; }

    (* register type here. *)

  in

  let analyse_items 
    (the_module:roc_module) 
    (the_cxt: semant_cxt)
    : unit =
    let the_namesapce = the_cxt.sc_namespace in
    List.iter (fun item ->
      (match item with
      | FunctionItem func -> 
        let analysed_func = analyse_function func the_cxt in
        update_symbol_table the_namesapce analysed_func.sf_name (FuncEntry analysed_func)

      | StructItem the_struct ->
        let analysed_struct = analyse_struct the_struct the_cxt in
        update_symbol_table the_namesapce analysed_struct.ss_name (StructEntry analysed_struct)

      | _ -> 
        todo "not yet supported item.")
    ) the_module.rm_items
  in

  (**************************************************************
     the helper functions end, the analysis begins. 
  **************************************************************)

  (* create the context *)
  let the_namespace = init_symbol_table () in
  let the_type_env = Hashtbl.create 10 in
  let the_cxt = {
    sc_namespace = the_namespace;
    sc_type_env = the_type_env;
    sc_current_sb = the_namespace;
  } in

  (* Insert builtins *)
  List.iter (fun builtin -> 
    let name = builtin.sf_name in
    let entry = FuncEntry builtin in
    insert_symbol the_namespace name entry) builtins_semant;

  (* register items *)
  register_items the_module the_cxt;

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
  analyse_items the_module the_cxt;

  { sm_namespace = the_cxt.sc_namespace;
    sm_type_env = the_cxt.sc_type_env; }

