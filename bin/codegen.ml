(* Code generating for Rooc *)

open Sast
open Util
open StructualIR
open Builtins
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate_module (sast: s_module) =
  let context    = L.global_context () in

  (* Add types *)
  let i32_t     = L.i32_type    context 
  and i8_t      = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  in
  
  (* Create an LLVM module *)
  let the_module = L.create_module context "theRoocProgram" in

  (* Convert SAST types to LLVM types *)
  let ltype_of_stype = function
    | ST_int -> i32_t
    | ST_bool -> i1_t
    | ST_float -> float_t
    (* //TODO: *)
    | _ -> raise (todo_failure "not supported. in ltype_of_stype")
  in

  let the_namespace = init_global_scope () in

  (* Declare built-in functions *)

  let translate_function (f: s_function) =
    match f.sf_body with
    | UserDefined body -> 
      (* Step 1: Determine LLVM function type *)
      (let llvm_return_type = ltype_of_stype f.sf_type.sft_return_type in
      let llvm_param_types = Array.of_list (List.map ltype_of_stype f.sf_type.sft_params_type) in
      let llvm_function_type = L.function_type llvm_return_type llvm_param_types in

      let llvm_function = L.define_function f.sf_name llvm_function_type the_module in

      (* Builder for function body *)
      let builder = L.builder_at_end context (L.entry_block llvm_function) in

      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  
      (* Process formals: Set names and allocate space *)
      let local_scope = init_local_scope (GlobalScope the_namespace) in
      let add_formal (param: s_variable) llvm_param =
        let local_name = param.sv_name in
        let local_type = ltype_of_stype param.sv_type in
        let () = L.set_value_name local_name llvm_param in
        let alloca = L.build_alloca local_type local_name builder in
        let _ = L.build_store llvm_param alloca builder in
        let local_var = {
          iv_name = local_name;
          iv_type = local_type;
          iv_value_addr = alloca;
        } in
        insert_local_variable local_name local_var local_scope;
      in
      (match f.sf_params with
      | Some params -> 
        List.iter2 add_formal params.sp_params
                          (Array.to_list (L.params llvm_function))
      | None -> ());
      (* ... translate the function body ... *)
      let stmts=body.sbe_stmts in
      let rec translate_stmt (s: s_stmt) builder (scope:ir_local_scope) : unit =
        (match s with
        | S_expr_stmt e -> 
          (translate_expr e builder scope)
        | S_var_decl_stmt v | S_let_decl_stmt v -> 
          (translate_var_decl v builder scope)
        )
      and translate_expr (e:s_expr) builder (scope:ir_local_scope): unit = 
        let e_content = e.se_content in
        match e_content with
        | S_int_literal i -> 
           ignore ( L.const_int i32_t i)
        | S_float_literal f -> ignore (L.const_float_of_string float_t f)
        | _ -> ()
      and translate_var_decl (v: s_variable) builder (scope:ir_local_scope) : unit =
        let local_name = v.sv_name in
        let local_type = ltype_of_stype v.sv_type in
        let alloca = L.build_alloca local_type local_name builder in
        let local_var = {
          iv_name = local_name;
          iv_type = local_type;
          iv_value_addr = alloca;
        } in
        insert_local_variable local_name local_var scope
      in
        List.iter (fun s -> ignore( translate_stmt s builder local_scope)) stmts;
      )
    | BuiltIn -> ()
  in

  let translate_item (key: string) (item: s_symbol_table_entry) : unit =
    match item with
    | FuncEntry f -> translate_function f
    | _ -> raise (todo_failure "not supported. in translate_item")
  in

  let translate_builtins () =
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
      } the_namespace;
  in
  translate_builtins ();
  Hashtbl.iter translate_item sast.sm_namespace.sst_symbols;
  the_module
