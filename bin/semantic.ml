open Ast
open Sast

let analyse_module (ast:roc_module) : s_module =
  let get_item_name (x:roc_item) : string = match x with
  | FunctionItem x_fun -> x_fun.rf_signature.rfs_name
  (* //TODO: add more *)
  in
  let get_var_name (x:roc_variable) : string =
    x.rv_name
  in
  let rec analyse_expr (raw_expr: roc_expr) (symbol_table: s_symbol_table) : s_expr =
    match expr with
    | Roc_string_literal strl -> {se_type = ST_string; se_content = S_string_literal strl}
      ST_string
    | Roc_int_literal intl -> { se_type = ST_int; se_content = S_int_literal intl }
    | Roc_float_literal fltl -> { se_type = ST_float; se_content = S_float_literal fltl }
    | Roc_bool_literal booll -> { se_type = ST_bool; se_content = S_bool_literal booll }
    
    | Roc_unary_expr (op, e) ->
        let analysed_expr = analyse_expr e symbol_table in
        let result_type = match (op, analysed_expr.se_type) with
        | (Negate, ST_int) -> ST_int   (* Negating an integer *)
        | (Negate, ST_float) -> ST_float (* Negating a float *)
        | (Not, ST_bool) -> ST_bool   (* Logical NOT on a boolean *)
        | _ -> ST_error (* Type mismatch or unsupported operation *)
      in
        { se_type = result_type;
          se_content = S_unary_expr (op, analysed_expr) }
    
    | Roc_arith_logical_expr (op, e1, e2) ->
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let result_type = match (analysed_e1.se_type, analysed_e2.se_type) with
        | (ST_int, ST_int) -> ST_int
        | (ST_float, ST_float) -> ST_float
        | _ -> ST_error  (* Type mismatch *)
      in
      { se_type = result_type; se_content = S_arith_logical_expr (op, analysed_e1, analysed_e2) }
        
    
    | Roc_comparison_expr (op, e1, e2) ->
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      (* Assuming comparison is valid only for certain types *)
      let is_valid_comparison = match (analysed_e1.se_type, analysed_e2.se_type) with
        | (ST_int, ST_int) | (ST_float, ST_float) -> true
        | _ -> false
      in
      let result_type = if is_valid_comparison then ST_bool else ST_error
      in
      { se_type = result_type; se_content = S_comparison_expr (op, analysed_e1, analysed_e2) }
        
    
    | Roc_assign_expr (e1, e2) ->
      (* //TODO: more check for left hand side*)
      let analysed_e1 = analyse_expr e1 symbol_table in
      let analysed_e2 = analyse_expr e2 symbol_table in
      let result_type = if analysed_e1.se_type = analysed_e2.se_type then analysed_e1.se_type else ST_error
      in
      { se_type = result_type; se_content = S_assign_expr (analysed_e1, analysed_e2) }
        
    
    | Roc_call_expr call_expr ->
      raise (todo_failure "call expr")
    
    | Roc_grouped_expr e ->
        let analysed_expr = analyse_expr e symbol_table in
        { se_type = analysed_expr.se_type; se_content = S_grouped_expr analysed_expr }
    
    | Roc_return_expr e ->
        let analysed_expr = analyse_expr e symbol_table in
        { se_type = analysed_expr.se_type; se_content = S_return_expr analysed_expr }
    
    | Roc_break_expr ->
        { se_type = ST_unit; se_content = S_break_expr }
    
    | Roc_continue_expr ->
        { se_type = ST_unit; se_content = S_continue_expr }
    
    (* //TODO: redesign how to deal with this one.
    | Roc_null_expr ->
        { se_type = ST_unit; se_content = S_null_expr } *)
    
  in
  let item_add (x:roc_item) = 
    let x_name = get_item_name (x) in

    let check_function (x_fun: roc_function ) =
      let 
    in
    (* //TODO: add checker for other item *)

    match x with
  | FunctionItem x_fun -> ()
    (* //TODO: add more *)
    in
  in
  
in


  (* assure there is no item with different name *)



