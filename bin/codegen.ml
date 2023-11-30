(* Code generating for Rooc *)

open Sast
module L = Llvm
module A = Ast


let generate_module (sast: s_module) =
  let context    = L.global_context () in

  (* Add types *)
  let i32_t     = L.i32_type    context 
  and i8_t      = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  in
  
  (* Create an LLVM module *)
  let the_module = L.create_module context "Rooc" in

  (* Convert SAST types to LLVM types *)
  let ltype_of_stype = function
    | ST_int -> i32_t
    | ST_bool -> i1_t
    | ST_float -> float_t
    | ST_void -> void_t
    (* //TODO: *)
    | _ -> (* handle other types *) in

  (* Declare built-in functions *)
  let printf_t: L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in 
  let printf_func: L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Function to translate s_function to LLVM IR *)
  let translate_function (f: s_function) =
    let ret_type = ltype_of_stype f.sf_return_type in
    let param_types = Array.of_list (List.map (fun p -> ltype_of_stype p.sfp_type) f.sf_params) in
    let func_type = L.function_type ret_type param_types in
    let llfunc = L.define_function f.sf_name func_type the_module in
    (* //TODO: *)
    in

  (* Function to translate s_block_expr to LLVM IR *)
  let rec translate_block_expr (block: s_block_expr) builder =
    (* ??TODO: *)
    (* ... iterate through statements, translate each statement ... *)
    in

  (* Function to translate s_stmt to LLVM IR *)
  let translate_stmt (stmt: s_stmt) builder =
    match stmt with
    | S_expr_stmt e -> (* ... translate expression ... *)
    | S_var_decl_stmt v -> (* ... translate variable declaration ... *)
    | S_let_decl_stmt v -> (* ... translate let declaration ... *)
    (* //TODO: *)
    in

  (* Function to translate s_expr to LLVM IR *)
  let translate_expr (expr: s_expr) builder =
    match expr with
    | S_string_literal s -> (* ... translate string literal ... *)
    | S_int_literal i -> (* ... translate int literal ... *)
    | S_float_literal f -> (* ... translate float literal ... *)
    | S_bool_literal b -> (* ... translate bool literal ... *)
    (* //TODO: *)
    | _ -> (* ... handle other expressions ... *)
    in

  (* //TODO *)


  the_module





