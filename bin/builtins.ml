open Sast
open Util
module L = Llvm
open Llhelper


let print_int = {
  sf_name = "print_int";
  sf_params = Some ({
    sp_params=[{
    sv_name = "i";
    sv_type = ST_int;
    sv_mutable = true;
    sv_initial_value = None;
  }]});
  sf_type = {
    sft_params_type = [ST_int];
    sft_return_type = ST_unit;
  };
  sf_body = BuiltIn;
}

let builtins_semant = [
  print_int;
]

(* let trans_builtins (the_module:L.llmodule) =
  let the_context = Llhelper.the_global_context in

  let i32_t = L.i32_type the_context 
  and i8_t  = L.i8_type  the_context in

  let printf_type: L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in 

  let printf_func: L.llvalue = L.declare_function "printf" printf_type the_module in
  (* Create the printf format string for integers *)
  insert_global_function "printf" {
      if_return_type = i32_t;
      if_param_types = [| L.pointer_type i8_t |];
      if_function_type = printf_type;
      if_function = printf_func;
      if_builder = None;
      if_scope = None;
    } the_namespace;

  (* Declare print_int as a wrapper around printf *)
  let print_int_type = L.function_type i32_t [| i32_t |] in
  let print_int_func = L.declare_function "print_int" print_int_type the_module in
  insert_global_function "print_int" {
      if_return_type = i32_t;
      if_param_types = [| i32_t |];
      if_function_type = print_int_type;
      if_function = print_int_func;
      if_builder = None;
      if_scope = None;
    } the_namespace; *)