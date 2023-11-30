open Ast
open Sast

let analyse ast_module =
  let get_item_name (x:roc_item) : string = match x with
  | FunctionItem x_fun -> x_fun.rf_signature.rfs_name
  (* //TODO: add more *)
  in
  let get_var_name (x:roc_variable) : string =
    x.rv_name
  in
  let rec infer_expr_type (expr: s_expr) (symbol_table: s_symbol_table) : s_type =
    match expr with
    | S_string_literal _ -> ST_string
    | S_int_literal _ -> ST_int
    | S_float_literal _ -> ST_float
    | S_bool_literal _ -> ST_bool
  
    | S_unary_expr (op, e) -> infer_unary_expr_type op e
    | S_arith_logical_expr (op, e1, e2) -> infer_binary_expr_type op e1 e2
    | S_comparison_expr (op, e1, e2) -> infer_binary_expr_type op e1 e2
    | S_assign_expr (e1, e2) -> infer_assign_expr_type e1 e2
  
    | S_call_expr call_expr -> infer_call_expr_type call_expr
    | S_grouped_expr e -> infer_expr_type e
  
    | S_return_expr e -> infer_expr_type e
  
    | S_break_expr | S_continue_expr | S_null_expr -> ST_void
    (* //TODO: need to assure those three kinds' return types *)
  
  
  and infer_unary_expr_type op e =
    (* //TODO: ... *)
  
  and infer_binary_expr_type op e1 e2 =
    (* //TODO: ... *)
  
  and infer_assign_expr_type e1 e2 =
    (* Ensure the types of e1 and e2 match *)
    (* //TODO: ... *)
  
  and infer_call_expr_type call_expr =
    (* The type is the return type of the function being called *)
    (* TODO: ... *)
  
    
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

  (* assure there is no item with different name *)



