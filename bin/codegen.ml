(* Code generating for Rooc *)

open Sast
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (functions, traits, structs, impls) =
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

  (* Convert types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
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

    let local_vars =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
	      let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	      StringMap.add n local m 
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (t, n) =
	      let local_var = L.build_alloca (ltype_of_typ t) n builder
	      in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sfd_formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.sfd_locals 
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> raise (Failure ("undeclared variable " ^ n))
    in

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
      | SCall ("print_str", [(_,SSliteral (e))]) -> 
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
      | SExpr e -> let _ = expr builder e in builder
      | SReturn e -> let _ = match fdecl.sfd_typ with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder
                     in builder
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

  let _ = List.iter build_method_body impls in
  List.iter build_function_body functions;
  the_module

