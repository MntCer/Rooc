open Ast
open Sast

let analyse ast_module =
  let get_item_name (x:roc_item) : string = match x with
  | FunctionItem x_fun -> x_fun.rfun_signature.rfs_name
  (* //TODO: add more *)
  in
  let get_var_name (x:roc_variable) : string =
    x.rv_name
  in
  let get_expr_type (e: roc_expr) : s_type =
    
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



