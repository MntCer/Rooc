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

let print_str = {
  sf_name = "print_str";
  sf_params = Some ({
    sp_params=[{
    sv_name = "s";
    sv_type = ST_string;
    sv_mutable = true;
    sv_initial_value = None;
  }]});
  sf_type = {
    sft_params_type = [ST_string];
    sft_return_type = ST_int;
  };
  sf_body = BuiltIn;
}

let concat_str = {
  sf_name = "concat_str";
  sf_params = Some ({
    sp_params=[{
    sv_name = "s1";
    sv_type = ST_string;
    sv_mutable = true;
    sv_initial_value = None;
  };
  {
    sv_name = "s2";
    sv_type = ST_string;
    sv_mutable = true;
    sv_initial_value = None;
  }]});
  sf_type = {
    sft_params_type = [ST_string; ST_string];
    sft_return_type = ST_string;
  };
  sf_body = BuiltIn;
}

let print_bool = {
  sf_name = "print_bool";
  sf_params = Some ({
    sp_params=[{
    sv_name = "b";
    sv_type = ST_bool;
    sv_mutable = true;
    sv_initial_value = None;
  }]});
  sf_type = {
    sft_params_type = [ST_bool];
    sft_return_type = ST_int;
  };
  sf_body = BuiltIn;
}

let print_float = {
  sf_name = "print_float";
  sf_params = Some ({
    sp_params=[{
    sv_name = "f";
    sv_type = ST_float;
    sv_mutable = true;
    sv_initial_value = None;
  }]});
  sf_type = {
    sft_params_type = [ST_float];
    sft_return_type = ST_int;
  };
  sf_body = BuiltIn;
}

let builtins_semant = [
  print_int;
  println;
  print_str;
  concat_str;
  print_bool;
  print_float;
]

type builtin_translator = L.llcontext -> L.llbuilder -> ir_local_scope -> ir_global_scope -> L.llbuilder

(**
  translate the built-in print_int into LLVM IR.

  Author: Yuanfei
*)
let trans_print_int
  (the_context:L.llcontext)
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

(**
  author: Yuanfei    
*)
let trans_println
  (the_context:L.llcontext)
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

(**
  author: Mona    
*)
let trans_print_str
  (the_context:L.llcontext)
  (the_builder: L.llbuilder)
  (the_scope: ir_local_scope)
  (the_namespace: ir_global_scope)
  : L.llbuilder =
  let the_printf =
    match lookup "printf" (IRGlobalScope the_namespace) with
    | Some (IRFuncEntry (IRExternFunction f)) -> f.ief_function
    | None | _ -> bug "printf is not declared"
  in
  let format_str = L.build_global_stringptr "%s" "fmt" the_builder in
  let the_param =
    match lookup "s" (IRLocalScope the_scope) with
    | Some (IRVarEntry v) -> v
    | None | _ -> bug "print_str's parameter is not right in its scope"
  in
  
  let s = L.build_load the_param.iv_value_addr "s" the_builder in
  let _ = L.build_call the_printf [| format_str; s |] "" the_builder in
  the_builder

(**
  author: Mona Yuanfei  
*)
let trans_concat_str
  (the_context:L.llcontext)
  (the_builder: L.llbuilder)
  (the_scope: ir_local_scope)
  (the_namespace: ir_global_scope)
  : L.llbuilder =
  let strlen = 
    match lookup "strlen" (IRGlobalScope the_namespace) with
    | Some (IRFuncEntry (IRExternFunction f)) -> f.ief_function
    | None | _ -> bug "strlen is not declared" in
  let malloc = 
    match lookup "malloc" (IRGlobalScope the_namespace) with
    | Some (IRFuncEntry (IRExternFunction f)) -> f.ief_function
    | None | _ -> bug "malloc is not declared" in
  let memcpy = 
    match lookup "memcpy" (IRGlobalScope the_namespace) with
    | Some (IRFuncEntry (IRExternFunction f)) -> f.ief_function
    | None | _ -> bug "memcpy is not declared" in
  (* Get parameters s1 and s2 from the scope *)
  let s1_ptrptr = 
    match lookup "s1" (IRLocalScope the_scope) with
    | Some (IRVarEntry v) -> v.iv_value_addr
    | None | _ -> bug "print_str's parameter is not right in its scope" in
  let s1_ptr = L.build_load s1_ptrptr "s1_ptr" the_builder in
  let s2_ptrptr = 
    match lookup "s2" (IRLocalScope the_scope) with
    | Some (IRVarEntry v) -> v.iv_value_addr
    | None | _ -> bug "print_str's parameter is not right in its scope" in
  let s2_ptr = L.build_load s2_ptrptr "s2_ptr" the_builder in
  (* Get lengths of s1 and s2 *)
  let s1_len = L.build_call strlen [| s1_ptr |] "s1_len" the_builder in
  let s2_len = L.build_call strlen [| s2_ptr |] "s2_len" the_builder in
  (* Calculate total length for new string (including null terminator) *)
  let total_len = L.build_add (L.build_add s1_len s2_len "tmp" the_builder) (L.const_int (L.i32_type the_context) 1) "total_len" the_builder in
  (* Allocate memory for the new concatenated string *)
  let new_str_ptr = L.build_call malloc [| total_len |] "new_str" the_builder in
  (* Copy s1 and s2 into the new string *)
  let _ = L.build_call memcpy [| new_str_ptr; s1_ptr; s1_len |] "" the_builder in
  let _ = L.build_call memcpy [| L.build_gep new_str_ptr [| s1_len |] "tmp" the_builder; s2_ptr; s2_len |] "" the_builder in
  (* Add null terminator to the new string *)
  let _ = L.build_store (L.const_int (L.i8_type the_context) 0) (L.build_gep new_str_ptr [| total_len |] "tmp" the_builder) the_builder in
  let _ = L.build_ret new_str_ptr the_builder in
    the_builder

(**
  author: Yuanfei    
*)
let trans_print_bool
  (the_context:L.llcontext)
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
    let search_result=lookup "b" (IRLocalScope the_scope) in
    match search_result with
    | Some (IRVarEntry (v)) -> v
    | None | _ -> bug "print_bool's pamameter is not right in its scope"
  in
  let b = L.build_load the_param.iv_value_addr "b" the_builder in
  let _ = L.build_call the_printf [| format_str; b |] "" the_builder in
  the_builder

let trans_print_float
  (the_context:L.llcontext)
  (the_builder:L.llbuilder)
  (the_scope:ir_local_scope)
  (the_namespace: ir_global_scope)
  : L.llbuilder =
  let the_printf = 
    match lookup "printf" (IRGlobalScope the_namespace) with
    | Some (IRFuncEntry (IRExternFunction (f))) -> f.ief_function
    | None | _ -> bug "printf is not declared"
  in
  let format_str = L.build_global_stringptr "%f" "fmt" the_builder in  
  let the_param =
    let search_result=lookup "f" (IRLocalScope the_scope) in
    match search_result with
    | Some (IRVarEntry (v)) -> v
    | None | _ -> bug "print_bool's pamameter is not right in its scope"
  in
  let f = L.build_load the_param.iv_value_addr "f" the_builder in
  let _ = L.build_call the_printf [| format_str; f |] "" the_builder in
  the_builder


let builtins_map:(string, builtin_translator) Hashtbl.t = Hashtbl.create 10
let () = Hashtbl.add builtins_map "print_int" trans_print_int
let () = Hashtbl.add builtins_map "println" trans_println
let () = Hashtbl.add builtins_map "print_str" trans_print_str
let () = Hashtbl.add builtins_map "concat_str" trans_concat_str
let () = Hashtbl.add builtins_map "print_bool" trans_print_bool
let () = Hashtbl.add builtins_map "print_float" trans_print_float

(**
  some external functions
  
  authors: Yuanfei.
*)
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
    insert_function "printf" (IRExternFunction wrapped_printf) the_scope

let declare_strlen 
  the_context 
  the_module 
  the_scope 
  : unit =
  let i32_t = L.i32_type the_context in
  let i8_ptr_t = L.pointer_type (L.i8_type the_context) in
  let strlen_type = L.function_type i32_t [| i8_ptr_t |] in
  let the_strlen = L.declare_function "strlen" strlen_type the_module in
  let wrapped_strlen = {
    ief_function_type = strlen_type;
    ief_function = the_strlen;
  } in
    insert_function "strlen" (IRExternFunction wrapped_strlen) the_scope

let declare_malloc 
  the_context 
  the_module 
  the_scope 
  : unit =
  let i8_ptr_t = L.pointer_type (L.i8_type the_context) in
  let i32_t = L.i32_type the_context in
  let malloc_type = L.function_type i8_ptr_t [| i32_t |] in
  let the_malloc = L.declare_function "malloc" malloc_type the_module in
  let wrapped_malloc = {
    ief_function_type = malloc_type;
    ief_function = the_malloc; } in
    insert_function "malloc" (IRExternFunction wrapped_malloc) the_scope

let declare_memcpy 
  the_context 
  the_module 
  the_scope =
  let i8_ptr_t = L.pointer_type (L.i8_type the_context) in
  let i32_t = L.i32_type the_context in
  let memcpy_type = L.function_type i8_ptr_t [| i8_ptr_t; i8_ptr_t; i32_t |] in
  let the_memcpy = L.declare_function "memcpy" memcpy_type the_module in
  let wrapped_memcpy = {
    ief_function_type = memcpy_type;
    ief_function = the_memcpy;
  } in
  insert_function "memcpy" (IRExternFunction wrapped_memcpy) the_scope    


type external_function_declaration = L.llcontext->L.llmodule->ir_global_scope->unit

let external_functions = [
  declare_printf;
  declare_strlen;
  declare_malloc;
  declare_memcpy;
]