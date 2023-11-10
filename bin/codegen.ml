(* Code generating for Rooc *)

open Sast
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (functions, traits, structs, impls) =
  let context    = L.global_context () in

  (* Add types *)
  (* TODO *)
  let i32_t     = L.i32_type    context 
  and i8_t      = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  (* ... *)

  in

  let the_module = L.create_module context "Rooc" in

  let ltype_of_typ = function
      A.Int      -> i32_t
  (* ... *)
  in

  let trait_decls : (L.llvalue * strait_decl) StringMap.t = 
  (* TODO *)
  (* record traits *)
  StringMap.empty in

  let struct_decls : (L.llvalue * sstruct_decl) StringMap.t =
  (* record structs *)
  StringMap.empty in

  let impl_decls : (L.llvalue * simpl_decl) StringMap.t =
  (* TODO *)
  (* record implmentations *)
  StringMap.empty in
  
  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in 
  let printf_func : L.llvalue =
     L.declare_function "printf" printf_t the_module in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
  (* record function decls *)
  let function_decl m fdecl =
    let name = fdecl.sfd_name
    and formal_types =
      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sfd_formals)
    in let ftype = L.function_type (ltype_of_typ fdecl.sfd_typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl = 

    let (the_function, _) = StringMap.find fdecl.sfd_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let rec expr builder ((_, e) : sexpr) = match e with
        SLiteral i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                         let _  = L.build_store e' (lookup s) builder in e'
      | SBinop (e1, op, e2) ->
          let (t, _) = e1
          and e1' = expr builder e1
          and e2' = expr builder e2 in
          if t = A.Float then (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or ->
              raise (Failure "internal error: semant should have rejected and/or on float")
          ) e1' e2' "tmp" builder
          else (match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
      | SUnop(op, e) ->
          let (t, _) = e in
          let e' = expr builder e in
          (match op with
            A.Neg when t = A.Float -> L.build_fneg
          | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      | SCall ("print_str", [e]) -> 
          L.build_call printf_func [| (L.build_global_stringptr e "fmt" builder) |] "printf" builder
      | _ -> L.const_int i32_t 0
      
      (* TODO: add function call, member *)
      (* ... *)

    in let add_terminal builder instr =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) 
    in

    let rec stmt builder = function
        SBlock sl -> List.fold_left stmt builder sl
      (* TODO *)
      (* ... *)
      | _ -> builder in
    let builder = stmt builder (SBlock fdecl.sfd_body) in
    add_terminal builder (match fdecl.sfd_typ with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  let build_method_body idecl = 
    List.iter build_function_body idecl.si_methods
    (* TODO: deal with implementations' methods *)  
  in
  List.iter build_function_body functions;
  List.iter build_method_body impls;
  the_module

