(*
  Semantic Process.

  author: Yuanfei, Mona, Xinyang
*)
open Ast
open Sast
open Util
open Builtins

(**
  some info needed when analysing.    
*)
type semant_cxt ={
  sc_namespace: s_symbol_table;
  sc_type_env: (string, s_type_env_entry) Hashtbl.t;
  sc_current_sb: s_symbol_table;
}

let analyse_module 
  (the_module:roc_module) 
  : s_module =

  (**
    First time encounter a struct, will register it as a "unresolved" type,
    analyser will not dive deep into struct body in the first pass.
    Second pass in analysing struct, when analyser checking the fields' type,
    if a field's type is some struct and this struct is registered, it's good.
  *)
  let rec analyse_type_expr
  (raw_type_expr: r_type_expr)
  (the_cxt: semant_cxt)
  : s_type =
  match raw_type_expr with
  | R_user_defined_type type_name ->
    let search_result = lookup_type the_cxt.sc_type_env type_name in
    (match search_result with
    | None -> raise (SemanticError ("type not registered:" ^ type_name))
    | Some (S_unresolved name ) -> 
      ST_struct name (* #NOTE: ad hoc solution, just work for struct. *)
    | Some (S_resolved the_type) -> the_type)

  (**
    map each raw type to its corresponding semantic type.

    author: Yuanfei
  *)
  and analyse_type 
  (raw_type: r_type)
  (the_cxt: semant_cxt)
  : s_type =
    match raw_type with
    (* if primitive type, easy. *)
    | T_unit -> ST_unit
    | T_int -> ST_int
    | T_float -> ST_float
    | T_bool -> ST_bool
    | T_string -> ST_string
    | T_typex typex -> analyse_type_expr typex the_cxt in

  (** 
    take a ast node `roc_expr` and current scope's symbol table, 
    do the semantic analysis on it. return the analysed sast node `s_expr`.

    authors: Yuanfei, Mona, Xinyang
  *)
  let rec analyse_expr 
    (raw_expr: roc_expr)
    (the_cxt: semant_cxt) 
    : s_expr =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let the_namespace = the_cxt.sc_namespace in
    match raw_expr with
    (* empty expression *)
    | EXPR_null -> { se_type = ST_unit; se_expr = S_EXPR_null }

    (* literals *)
    | Roc_string_literal strl -> {se_type = ST_string; se_expr = S_string_literal strl}
    | Roc_int_literal intl -> { se_type = ST_int; se_expr = S_int_literal intl }
    | Roc_float_literal fltl -> { se_type = ST_float; se_expr = S_float_literal fltl }
    | Roc_bool_literal booll -> { se_type = ST_bool; se_expr = S_bool_literal booll }

    (* Unary Expression *)
    | Roc_unary_expr (op, e) ->
      let analysed_expr = analyse_expr e the_cxt in
      let analysed_type = match (op, analysed_expr.se_type) with
        | (Neg, ST_int) -> ST_int
        | (Neg, ST_float) -> ST_float
        | (Not, ST_bool) -> ST_bool
        | _ -> raise (type_err_failure "Unary expression type mismatch")
          (* ST_error  *)
      in
      { se_type = analysed_type; 
        se_expr = S_unary_expr (op, analysed_expr) }

    (* Binary Arithmetic Expression *)
    | Roc_arith_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 the_cxt in
        let analysed_e2 = analyse_expr e2 the_cxt in
        let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_int, ST_int) -> ST_int
          | (ST_float, ST_float) -> ST_float
          | _ -> raise (type_err_failure "Arithmetic expression type mismatch and only supports int and float")
            (* ST_error   *)
            (*TODO: string concatenation as a built-in function?*) in
          { se_type = analysed_type; 
            se_expr = S_arith_expr (op, analysed_e1, analysed_e2) }

    (* Binary Logical Expression *)
    | Roc_logical_expr (op, e1, e2) ->
        let analysed_e1 = analyse_expr e1 the_cxt in
        let analysed_e2 = analyse_expr e2 the_cxt in
        let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
          | (ST_bool, ST_bool) -> ST_bool
          | _ -> raise (type_err_failure "Logical expression only supports bool type")
            (* ST_error *) in
          { se_type = analysed_type; 
            se_expr = S_logical_expr (op, analysed_e1, analysed_e2) }

    (* Binary Comparison Expression *)
    | Roc_comparison_expr (op, e1, e2) ->
      let analysed_e1 = analyse_expr e1 the_cxt in
      let analysed_e2 = 
        match (analysed_e1.se_type,e2) with
        | (ST_struct struct_name, EXPR_nullstruct) -> 
          { se_type = ST_struct struct_name; 
            se_expr = S_EXPR_nullstruct struct_name }
        | _ -> 
          analyse_expr e2 the_cxt in
      let analysed_type = match (analysed_e1.se_type, analysed_e2.se_type) with
        | (ST_int, ST_int) | (ST_float, ST_float) | (ST_bool, ST_bool) -> ST_bool
        | (ST_struct s1, ST_struct s2) -> 
          ST_bool
        (* #TODO: need more tests. *)
        | (ST_string, ST_string ) -> 
          ST_bool
        | _ -> raise (type_err_failure "Comparison expression type mismatch and only supports int, float, and bool")
          (* ST_error *)
      in
      { se_type = analysed_type; 
        se_expr = S_comparison_expr (op, analysed_e1, analysed_e2) }
        
    (*
      Analyse the params, and ensure the callee is a existing function. 
      Use the function's return type as the type of this call expression.     
    *)
    | Roc_call_expr (callee, arg_list) ->
      let analysed_args = 
        List.map (fun arg -> analyse_expr arg the_cxt) arg_list 
      in
      let search_result = lookup_symbol callee the_symbol_table in
      (match search_result with
      | None -> raise (SemanticError "callee not found")
      | Some(VarEntry v) -> raise (SemanticError "callee is a variable")
      | Some(FuncEntry f) -> 
        (* function arguments type checking *)
        let expected_param_types = f.sf_type.sft_params_type in
        (* if the number of arguments matches the number of parameters *)
          if List.length expected_param_types <> List.length analysed_args then
            raise (type_err_failure "Incorrect number of arguments in function call")
          else (* if each argument type matches the expected parameter type *)
            List.iter2 (fun expected_type expr ->
              if expected_type <> expr.se_type then
                raise (type_err_failure "Argument type mismatch in function call")
                ) expected_param_types analysed_args;
        (* extract return type*)
        let analysed_type = f.sf_type.sft_return_type in
        let analysed_callee = callee in
        { se_type = analysed_type; 
          se_expr = S_EXPR_call({
            sc_callee = analysed_callee;
            sc_arguments = analysed_args; }) }
      | Some (FuncSigEntry fs) ->
        let analysed_type = fs.sfs_type.sft_return_type in
        let analysed_callee = callee in 
        { se_type = analysed_type; 
          se_expr = S_EXPR_call({
            sc_callee = analysed_callee;
            sc_arguments = analysed_args; }) } )
    
    | Roc_grouped_expr e ->
      let analysed_expr = analyse_expr e the_cxt in
      { se_type = analysed_expr.se_type; 
        se_expr = S_grouped_expr analysed_expr}

    (*
      the type for field_access is the final field's type.    
    *)
    | EXPR_field_access (var_name, expr) -> (
      (* flatten the recursive call tree first. *)
      let field_name_list = 
        let rec get_field_name_list 
          acc 
          (x:roc_expr) 
          : string list = (
          match x with
          | EXPR_path s -> 
            List.rev (s::acc)
          | EXPR_field_access (s2,e2) -> 
            let new_acc = s2::acc in
            get_field_name_list new_acc e2) 
        in
        get_field_name_list [] expr
      in
      (* check if there is the var *)
      match lookup_symbol var_name the_symbol_table with
      | None -> 
        let () = print_string(var_name) in
        raise (SemanticError "struct not found in namespace")
      | Some (VarEntry v) -> 
        let v_type = v.sv_type in
        let the_struct_name = 
          match v.sv_type with
          | ST_struct s -> s
          | _ -> raise (SemanticError "want to access a non-struct field.") 
        in
        (* let () = print_string(the_struct_name) in *)
        let the_struct = 
          (match lookup_symbol the_struct_name the_namespace with
          | None -> raise (SemanticError "struct not found in namespace")
          | Some (StructEntry s) -> s
          | _ -> raise (SemanticError (the_struct_name ^ " is not a struct")))
        in 
        let rec analyse_field_name_list 
          (cur_struct: s_struct)
          (the_field_name_list:string list) 
          : s_type =
          match the_field_name_list with
          | [field_name] -> 
            (* check if the struct has field with this name. *)
            let the_field = 
              (match List.find_opt 
                    (fun field -> field.ssf_name = field_name) cur_struct.ss_fields with
              | None -> raise (SemanticError "field not found in struct")
              | Some f -> f)
            in
              (* #TEST: what will happen if here is a struct type? *)
              the_field.ssf_type 
          | field_name::tl -> 
            (* has this field? *)
            let the_field = 
              (match List.find_opt (fun field -> 
                field.ssf_name = field_name) 
                cur_struct.ss_fields with
              | None -> 
                let _ = Printf.printf "field %s not found in struct: %s\n" field_name cur_struct.ss_name in
                raise (SemanticError "field not found in struct")
              | Some f -> f)
            in
            let the_field_type = the_field.ssf_type in
            (* need the field type to be a struct. *)
            let the_field_type_name = match the_field_type with
              | ST_struct s -> s
              | _ -> raise (SemanticError "want to access a non-struct type's field.")
            in 
            (* use the type name to search it's def. *)
            let the_next_struct = 
              match lookup_symbol the_field_type_name the_namespace with
              | None -> raise (SemanticError "struct not found in namespace")
              | Some (StructEntry s) -> s
              | _ -> raise (SemanticError (the_field_type_name ^ " is not a struct")) in
            (* let _ = Printf.printf "the next struct is: %s\n" the_next_struct.ss_name in #DEBUG *)
              analyse_field_name_list the_next_struct tl
        in
        let analysed_type = analyse_field_name_list the_struct field_name_list in
        { se_type = analysed_type; 
          se_expr = S_EXPR_field_access (var_name, field_name_list); })

    | EXPR_path (var_name) ->
      (match lookup_symbol var_name the_symbol_table with
      | None -> raise (SemanticError "variable not found in symbol table")
      | Some (VarEntry v) -> 
        let analysed_type = v.sv_type in
        { se_type = analysed_type; 
          se_expr = S_EXPR_path var_name; }
      | Some (StructEntry s) ->
        let analysed_type = 
          (match lookup_type the_type_env s.ss_name with
          | None -> raise (SemanticError "struct type not defined in type env")
          | Some (S_resolved t) -> t) 
        in
        { se_type = analysed_type; 
          se_expr = S_EXPR_path var_name; }
      | _ -> raise (SemanticError "Not a variable in symbol table"))

    | EXPR_struct (e1, field_exprs) ->
      (* #NOTE: in fact, this situation is prevented by parser. *)
      let () = match e1 with 
        | EXPR_path _ -> ()
        | _ -> raise (SemanticError "struct expression's first part is not a path")
      in

      (* Try to get the struct's def info. *)
      let analysed_e1 = analyse_expr e1 the_cxt in
      let the_struct_name = 
        (match analysed_e1.se_type with
        | ST_struct name -> name
        | _ -> raise (SemanticError "struct expression's first part is not a struct type"))
      in
      let the_struct = 
        (match lookup_symbol the_struct_name the_namespace with
        | None -> raise (SemanticError "struct not found in namespace")
        | Some (StructEntry s) -> s
        | _ -> raise (SemanticError (the_struct_name ^ " is not a struct")))
      in
      
      (* ensure no duplicate field name in expr_field *)
      let sorted_field_exprs = 
        List.sort 
          (fun a b -> 
            let name1=a.esf_name in
            let name2=b.esf_name in
            compare name1 name2) 
          field_exprs in
      let rec has_duplicate
        (the_list: struct_field_expr list)
        (last_name: string)
        : bool =
        match the_list with
        | [] -> false
        | hd::tl -> 
          let the_name = hd.esf_name in
          if the_name = last_name then true
          else has_duplicate tl the_name
      in
      let name_checked_expr_fields = 
        if has_duplicate sorted_field_exprs "" then
          raise (SemanticError "duplicate field name in struct expression")
        else sorted_field_exprs
      in
        
      (* to match each expr and field *)
      let the_fields = the_struct.ss_fields in
      let sorted_the_fields = 
        List.sort 
          (fun a b -> 
            let name1=a.ssf_name in
            let name2=b.ssf_name in
            compare name1 name2) 
          the_fields in
      let () = List.iter2 
        (fun field expr_field ->
          let field_name = field.ssf_name in
          let expr_field_name = expr_field.esf_name in
          (* ensure all field has its expr *)
          if field_name <> expr_field_name then
            raise (SemanticError "field name mismatch in struct expression")
          else 
          (* ensure all types are correct *)
            let field_type = field.ssf_type in
            let expr_field_expr = expr_field.esf_expr in
              match (field_type,expr_field_expr) with
              | (ST_struct _, EXPR_nullstruct) -> ()
              | _ -> 
                let analysed_expr_field_expr = analyse_expr expr_field_expr the_cxt in
                if field_type <> analysed_expr_field_expr.se_type then
                  raise (type_err_failure "field type mismatch in struct expression")
                else ()) 
        sorted_the_fields name_checked_expr_fields in

      (* Sure about the field expr's correctness. *)
      (* analyse nullstruct *)
      let analysed_fields = List.map2 (fun field expr_field ->
        let expr_field_expr = expr_field.esf_expr in
        match expr_field_expr with
        | EXPR_nullstruct -> 
          let the_struct_name = 
            (match field.ssf_type with
            | ST_struct name -> name
            | _ -> raise (SemanticError "assign null to non-struct field")) in
          let field_type = field.ssf_type in
            { sv_name = expr_field.esf_name;
              sv_type = field_type;
              sv_mutable = true;
              sv_initial_value = Some (
                { se_type = field.ssf_type;
                  se_expr = S_EXPR_nullstruct the_struct_name;}); }
        | _ ->
          let analysed_expr_field_expr = analyse_expr expr_field_expr the_cxt in
          { sv_name = expr_field.esf_name;
            sv_type = analysed_expr_field_expr.se_type;
            sv_mutable = true;
            sv_initial_value = Some analysed_expr_field_expr; })
        (* analyse each field's expr in original order. *)
        sorted_the_fields name_checked_expr_fields in

      (* Now, analyse each field's expr in original order. *)
      let analysed_type = ST_struct the_struct_name in
      { se_type = analysed_type;
        se_expr = S_EXPR_struct 
        (the_struct_name, analysed_fields); }

    (* treat as type, but I think it's bad. *)
    | EXPR_nullstruct ->
      raise (SemanticError "`null` appears in a wrong place.")
    
  (**
    check if a statement is legal
    authors: Yuanfei, Mona
  *)
  and analyse_stmt 
  (to_analyse: roc_stmt)
  (the_cxt: semant_cxt)
  : s_stmt = 
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    match to_analyse with
    | Roc_expr_stmt expr -> 
      let analysed_expr = analyse_expr expr the_cxt in
      S_expr_stmt analysed_expr

    | Roc_var_decl_stmt var_decl -> 
      let var_name = var_decl.rv_name in
      let analysed_var = analyse_variable var_decl true the_cxt in
      (* Add to symbol table *)
      let () = insert_symbol the_symbol_table var_name (VarEntry analysed_var) in
      S_var_decl_stmt analysed_var

    | Roc_let_decl_stmt let_decl ->
      let let_name = let_decl.rv_name in
      let analysed_let = analyse_variable let_decl false the_cxt in
      (* Add to symbol table *)
      let () = insert_symbol the_symbol_table let_name (VarEntry analysed_let) in
      S_let_decl_stmt analysed_let
      
    | STMT_assignment (e1, e2) ->
      (**
        check if the expr is a allowed mutable left value.
      *)
      let check_lval (expr: roc_expr) : unit =
        match expr with
        | EXPR_path s -> 
          (* not immutable. *)
          (match lookup_symbol s the_symbol_table with
          | None -> raise (SemanticError "variable not found in symbol table")
          | Some (VarEntry v) -> 
            if not v.sv_mutable then
              raise (SemanticError "variable is immutable")
            else ()
          | _ -> raise (SemanticError "left hand side of assignment is not a lvalue"))
        (* every field is mutable*)
        | EXPR_field_access _ -> ()
        | _ -> raise (SemanticError "left hand side of assignment is not a lvalue") in
      let () = check_lval e1 in
      let analysed_e1 = analyse_expr e1 the_cxt in
      let analysed_e2 = analyse_expr e2 the_cxt in
      let analysed_type = 
        if analysed_e1.se_type = analysed_e2.se_type 
        then analysed_e1.se_type 
        else 
          raise (type_err_failure "Assignment type mismatch") in
        S_STMT_assignment (analysed_e1, analysed_e2)

    | Roc_return_stmt e ->
        let analysed_expr = analyse_expr e the_cxt in
        S_STMT_return analysed_expr

    
    | STMT_block b ->
        let analysed_block = analyse_block b the_cxt true in
        S_STMT_block analysed_block

    | Roc_while_stmt (cond, body) ->
      let analysed_cond = analyse_expr cond the_cxt in
      let cond_type = analysed_cond.se_type in
      let () = 
        match cond_type with
        | ST_bool -> ()
        | _ -> raise (type_err_failure "while condition type mismatch")
      in
      let analysed_block = analyse_block body the_cxt true in
      S_STMT_while ({
        sws_condition = analysed_cond;
        sws_body = analysed_block; })

    | Roc_break_stmt ->
      S_STMT_break
    
    | Roc_continue_stmt ->
      S_STMT_continue

    | Roc_if_stmt (cond, then_branch, else_branch) ->
      let analysed_cond = analyse_expr cond the_cxt in
      let cond_type = analysed_cond.se_type in
      let () = 
        match cond_type with
        | ST_bool -> ()
        | _ -> raise (type_err_failure "if condition type mismatch")
      in
      let analysed_then = analyse_block then_branch the_cxt true in
      let analysed_else = analyse_block else_branch the_cxt true in
      S_STMT_if ({
        sie_condition = analysed_cond;
        sie_true_branch = analysed_then;
        sie_false_branch = analysed_else;})

  (**
    if the `init_new_table` is true, the function will create a new symbol table with current symbol table in context as its parent,
    otherwise, the function will use the current symbol table as the table for the block.

    author: Yuanfei
  *)
  and analyse_block 
    ({rb_stmts:roc_stmt list}) 
    (the_cxt: semant_cxt)
    (init_new_table:bool) 
    : s_block =
    (* determine which is "the table" *)
    let table_in = the_cxt.sc_current_sb in
    let the_namesapce = the_cxt.sc_namespace in
    let the_type_env = the_cxt.sc_type_env in
    let the_cxt = 
      if init_new_table then 
        let the_new_scope = init_symbol_table ~parent:table_in () in
        { sc_namespace = the_namesapce;
          sc_type_env = the_type_env;
          sc_current_sb = the_new_scope; }
      else the_cxt
    in
    (* Analyse *)
    let to_analyse = rb_stmts in
    (* #TODO: also need a function wrapper here *)
    let analysed_stmts = List.map (fun stmt -> analyse_stmt stmt the_cxt) to_analyse in
    { sb_stmts = analysed_stmts; 
      sb_scope = the_cxt.sc_current_sb; }

  (**
    analyse and register a variable in current scope.
    
    author: Yuanfei
  *)
  and analyse_variable 
    (raw_variable: roc_variable) 
    (is_mutable: bool) 
    (the_cxt: semant_cxt)
    : s_variable =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_name = raw_variable.rv_name in
    let analysed_type = analyse_type raw_variable.rv_type the_cxt in
    let analysed_initial_value = 
      match raw_variable.rv_initial_expr with
      | None -> None
      | Some expr -> Some (analyse_expr expr the_cxt)
    in
    let () = 
      match analysed_initial_value with
      | None -> ()
      | Some expr -> 
          if expr.se_type <> analysed_type then
            raise (type_err_failure "variable initial value type mismatch")
    in
    { sv_name = analysed_name; 
      sv_type = analysed_type; 
      sv_mutable = is_mutable;
      sv_initial_value = analysed_initial_value } in

  (**
    transfer params AST form to SAST form, 
    register the params as variables in function scope.     
  *)
  let analyse_params 
    (raw_params: roc_params) 
    (the_cxt: semant_cxt)
    : s_params =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_params = 
      List.map 
        (fun param -> analyse_variable param true the_cxt) 
        raw_params.rp_params in
    { sp_params = analysed_params }
  in

  let analyse_params_type 
    (raw_params: roc_params) 
    (the_cxt: semant_cxt)
    : s_type list =
    List.map 
      (fun param -> analyse_type param.rv_type the_cxt) 
      raw_params.rp_params
  in

  (**
    only needs to create type by struct name.
    #NOTE: no deeper type analysis, it's an ad hoc solution.

    author: Yuanfei
  *)
  let register_struct 
  (raw_struct: r_struct) 
  (the_cxt: semant_cxt)
  : unit =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let name_for_register = raw_struct.rs_name in
    let type_for_register = ST_struct name_for_register in
    insert_type the_type_env name_for_register (S_resolved type_for_register)
  in

  (**
    ensure a field's name and it's type.
  *)
  let analyse_field 
    (raw_field: r_struct_field) 
    (the_cxt: semant_cxt)
    : s_struct_field =
    let analysed_name = raw_field.rsf_name in
    let analysed_type = analyse_type raw_field.rsf_type the_cxt in
    { ssf_name = analysed_name;
      ssf_type = analysed_type; }
  in

  (**
    insert the struct into symbol table, update the type_env.

    author: Yuanfei
  *)
  let analyse_struct 
    (raw_struct: r_struct) 
    (the_cxt: semant_cxt)
    : s_struct =
    let analysed_name = raw_struct.rs_name in
    let raw_fields = raw_struct.rs_fields in
    (* don't allow two fields with same name. *)
    let sorted_r_fields = 
      List.sort (fun f1 f2 -> compare f1.rsf_name f2.rsf_name) raw_fields in
    let rec has_duplicate
      (the_list: r_struct_field list)
      (last_name: string)
      : bool =
      match the_list with
      | [] -> false
      | hd::tl -> 
        let the_name = hd.rsf_name in
        if the_name = last_name then true
        else has_duplicate tl the_name
    in
    let () = 
      if has_duplicate sorted_r_fields "" then
        raise (SemanticError "duplicate field name in struct")
      else ()
    in
    (* analyse fields *)
    let analysed_fields = 
      List.map 
      (fun field -> analyse_field field the_cxt) 
      raw_fields 
    (* #NOTE: I think this sort is bad. 
       maybe we will support the alignment in the future.*)
    (* let analysed_fields = 
      List.sort (fun f1 f2 -> compare f1.ssf_name f2.ssf_name) analysed_fields *)
    in
    let analysed_struct =
      { ss_name = analysed_name;
        ss_fields = analysed_fields; }
    in
    analysed_struct

  in

  (**
    the whole process of analysing structs. include 2 passes.
  *)
  let process_structs
    (the_module: roc_module)
    (the_cxt: semant_cxt)
    : unit =
    let the_namesapce = the_cxt.sc_namespace in
    let the_type_env = the_cxt.sc_type_env in
    (* need to register and analyse struct first. *)
    let () = 
      List.iter 
      (fun item ->
        (match item with
        | StructItem the_struct ->
          register_struct the_struct the_cxt
        | _ -> ()))
      the_module.rm_items in
    let () =
      List.iter
        (fun item ->
          (match item with
          | StructItem the_struct ->
            let analysed_struct = analyse_struct the_struct the_cxt in
            (* #TODO: not finished. *)
            insert_symbol the_namesapce analysed_struct.ss_name (StructEntry analysed_struct)
          | _ -> ())) 
      the_module.rm_items in
    ()
  in

  (**
    register traits.

    authors: Xinyang
  *)
  let analyse_method_sig
  (raw_method_sig: roc_method_signature)
  (the_cxt: semant_cxt)
  : s_method_signature = 
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_name = raw_method_sig.rms_name in
    let raw_params =  raw_method_sig.rms_params in
    let raw_return_type = raw_method_sig.rms_return_type in
    let analysed_params =
        (match raw_params with
        | None -> None
        | Some params -> Some (analyse_params params the_cxt))
    in
    let analysed_type =
      (match raw_params with
      | None ->
        { sft_params_type = [];
          sft_return_type = analyse_type raw_return_type  the_cxt; }
      | Some params ->
        let analysed_params_type = analyse_params_type params the_cxt in
        { sft_params_type = analysed_params_type;
          sft_return_type = analyse_type raw_return_type the_cxt; } )
    in   
    { sms_name = analysed_name;
      sms_params = analysed_params;
      sms_type = analysed_type; } 
  in

  (**
    author: Xinyang     
  *)
  let register_trait
  (raw_trait: roc_trait)
  (the_cxt: semant_cxt)
  : unit = 
    let the_namespace = the_cxt.sc_namespace in
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_name = raw_trait.rt_name in
    let analyse_method_sig_cur_cxt methods = 
      analyse_method_sig methods the_cxt
    in
    let analysed_method_sigs = 
      List.map analyse_method_sig_cur_cxt raw_trait.rt_methods
    in 
    let analysed_trait = 
      { st_name = analysed_name;
        st_method_signatures = analysed_method_sigs; }
    in
    insert_symbol the_namespace analysed_trait.st_name (TraitEntry analysed_trait)
  in

  (**
    the whole process of analysing traits. just 1 pass.
    
    author: Xinyang
  *)
  let process_traits
    (the_module: roc_module)
    (the_cxt: semant_cxt)
    : unit = 
    let the_namespace = the_cxt.sc_namespace in
    let the_type_env = the_cxt.sc_type_env in
    let () =
      List.iter
      (fun item ->
        (match item with
        | TraitItem the_trait ->
          register_trait the_trait the_cxt
        | _ -> ()))
      the_module.rm_items in
  ()
  in

  (**
    Analyse a function's name, params and return type and construct them into a `s_function_signature` type. 
    Register this signature into symbol table for the following semantic analysis.

    Author: Yuanfei
  *)
  let register_function 
  (raw_func: roc_function) 
  (the_cxt: semant_cxt)
  : unit =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in
    let analysed_name = raw_func.rf_name in
    let raw_params =  raw_func.rf_params in
    let analysed_params = 
      (match raw_params with
      | None -> None
      | Some params -> Some (analyse_params params the_cxt))
    in
    (* #TODO: should I analyse type here? *)
    let analysed_type = 
      (match raw_params with
      | None -> 
        { sft_params_type = []; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } 
      | Some params -> 
        let analysed_params_type = analyse_params_type params the_cxt in
        { sft_params_type = analysed_params_type; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } )
    in
    let analysed_func_sig = 
      { sfs_name = analysed_name;
        sfs_params = analysed_params;
        sfs_type = analysed_type; }
    in
    insert_symbol the_symbol_table analysed_name (FuncSigEntry analysed_func_sig)
  in

  (**
    More deeper analysis for a function.
    
    Author: Yuanfei
  *)
  let analyse_function 
    (raw_func: roc_function) 
    (the_cxt: semant_cxt)
    : s_function =
    let the_symbol_table = the_cxt.sc_current_sb in
    let the_type_env = the_cxt.sc_type_env in

    let analysed_name = raw_func.rf_name in
    let raw_params =  raw_func.rf_params in
    let analysed_params = 
      (match raw_params with
      | None -> None
      | Some params -> Some (analyse_params params the_cxt))
    in
    let analysed_type = 
      (match raw_params with
      | None -> 
        { sft_params_type = []; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } 
      | Some params -> 
        let analysed_params_type = analyse_params_type params the_cxt in
        { sft_params_type = analysed_params_type; 
          sft_return_type = analyse_type raw_func.rf_return_type the_cxt; } )
    in
    let param_vars = 
      (match analysed_params with
      | None -> []
      | Some params -> params.sp_params) in 
    (* create a new scope and accordingly new cxt *)
    let func_local_scope = init_symbol_table ~parent:the_symbol_table () in
    List.iter (fun param -> 
      let param_name = param.sv_name in
      let param_entry = VarEntry param in
      insert_symbol func_local_scope param_name param_entry) param_vars;
    let the_new_cxt = {
      sc_namespace = the_cxt.sc_namespace;
      sc_type_env = the_cxt.sc_type_env;
      sc_current_sb = func_local_scope;
    } in
    let body_to_analyse = raw_func.rf_body in
    let analysed_body = UserDefined (analyse_block body_to_analyse the_new_cxt false) in
    { sf_name = analysed_name;
      sf_params = analysed_params;
      sf_type = analysed_type;
      sf_body = analysed_body; }
  in

  (**
    the whole process of analysing functions. include 2 passes.
  *)
  let process_functions 
    (the_module:roc_module) 
    (the_cxt: semant_cxt)
    : unit =
    let the_namesapce = the_cxt.sc_namespace in
    (* to register functions*)
    let () = 
      List.iter 
      (fun item ->
        (match item with
        | FunctionItem the_function -> 
          register_function the_function the_cxt
        | _ -> ()))
      the_module.rm_items in
    (* to analyse functions *)
    let () = 
      List.iter (fun item ->
        (match item with
        | FunctionItem func -> 
          let analysed_func = analyse_function func the_cxt in
          update_symbol_table the_namesapce analysed_func.sf_name (FuncEntry analysed_func)
        (* #TODO: *)
        | _ -> ())) the_module.rm_items in 
    ()
  in

  (**************************************************************
     the helper functions end, the analysis begins. 
  **************************************************************)

  (* create the context *)
  let the_namespace = init_symbol_table () in
  let the_type_env = Hashtbl.create 10 in
  let the_cxt = {
    sc_namespace = the_namespace;
    sc_type_env = the_type_env;
    sc_current_sb = the_namespace;
  } in

  (* process struct *)
  let () = process_structs the_module the_cxt in

  (* process trait *)
  let () = process_traits the_module the_cxt in

  (* Insert builtins *)
  let () = List.iter (fun builtin -> 
    let name = builtin.sf_name in
    let entry = FuncEntry builtin in
    insert_symbol the_namespace name entry) builtins_semant; 
  in

  (* process items *)
  let () = process_functions the_module the_cxt in

  (* special check for main *)
  let () =
    (match lookup_symbol "main" the_namespace with
    | None -> raise (SemanticError "main function not found")
    | Some (FuncEntry f) -> 
      let the_return_type = f.sf_type.sft_return_type in
      (match the_return_type with
      | ST_int -> ()
      | _ -> bug "main function's return type is not int")
    | _ -> raise (SemanticError "main is not a function"))
  in
    
    { sm_namespace = the_cxt.sc_namespace;
      sm_type_env = the_cxt.sc_type_env; }

