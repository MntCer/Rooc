open Sast
open Llirwrapper

open Util
module L = Llvm


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
    sft_return_type = ST_int;
  };
  sf_body = BuiltIn;
}

let println = {
  sf_name = "println";
  sf_params = None;
  sf_type = {
    sft_params_type = [];
    sft_return_type = ST_int;
  };
  sf_body = BuiltIn;
}


let builtins_semant = [
  print_int;
  println;
]


let declare_printf 
  (the_context:L.llcontext)
  (the_module:L.llmodule) 
  (the_scope:ir_global_scope)
  : unit =
  let i32_t = L.i32_type the_context in
  let i8_t = L.i8_type the_context in
  let printf_type = 
    L.var_arg_function_type 
    (i32_t) 
    [| L.pointer_type i8_t |] in
  let the_printf = L.declare_function "printf" printf_type the_module in
  let wrapped_printf = {
    ief_function_type = printf_type;
    ief_function = the_printf;
  } in
  insert_function "printf" (IRExternFunction wrapped_printf) the_scope;


type builtin_translator =  L.llbuilder -> ir_local_scope -> ir_global_scope -> L.llbuilder

(**
    translate the built-in print_int into LLVM IR.

*)
let trans_print_int
  (the_builder:L.llbuilder)
  (the_scope:ir_local_scope)
  (the_namespace: ir_global_scope)
  : L.llbuilder =

  let the_printf = 
    match lookup "printf" (IRGlobalScope the_namespace) with
    | Some (IRFuncEntry (IRExternFunction (f))) -> f.ief_function
    | None | _ -> bug "printf is not declared"
  in
  let format_str = L.build_global_stringptr "%d" "fmt" the_builder in  
  let the_param =
    let search_result=lookup "i" (IRLocalScope the_scope) in
    match search_result with
    | Some (IRVarEntry (v)) -> v
    | None | _ -> bug "print_int's pamameter is not right in its scope"
  in
  (* #NOTE: I guess here is abstraction leak. *)
  let i = L.build_load the_param.iv_value_addr "i" the_builder in
  let _ = L.build_call the_printf [| format_str; i |] "" the_builder in
  the_builder

let trans_println
  (the_builder:L.llbuilder)
  (the_scope:ir_local_scope)
  (the_namespace: ir_global_scope)
  : L.llbuilder =

  let the_printf = 
    match lookup "printf" (IRGlobalScope the_namespace) with
    | Some (IRFuncEntry (IRExternFunction (f))) -> f.ief_function
    | None | _ -> bug "printf is not declared"
  in
  let format_str = L.build_global_stringptr "\n" "fmt" the_builder in  
  let _ = L.build_call the_printf [| format_str |] "" the_builder in
  the_builder


let builtins_map:(string, builtin_translator) Hashtbl.t = Hashtbl.create 10
let () = Hashtbl.add builtins_map "print_int" trans_print_int
let () = Hashtbl.add builtins_map "println" trans_println

