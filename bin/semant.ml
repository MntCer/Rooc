

open Ast
open Sast

module StringMap = Map.Make(String)

let check(functions,traits,structs,impls) =
  let check_binds (kind : string) (to_check : bind list) = 
    let name_compare (_, n1) (_, n2) = compare n1 n2 in
    let check_it checked binding = 
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked

    in 
    let _ = List.fold_left check_it [] (List.sort name_compare to_check) 
    in to_check
  in 
  let check_func_sig func_sig = 
    let checked_formals = check_binds "formal" func_sig.fs_formals in 
    {
      sfs_typ = func_sig.fs_typ;
      sfs_name = func_sig.fs_name;
      sfs_formals = checked_formals;
    }
  in 
  let check_trait trait =
    (* %TODO: method name *)
    {
      str_name = trait.tr_name;
      str_methods = List.map check_func_sig trait.tr_methods;
    }
  in 
  let check_traits (to_check : trait_decl list) =
    let compare_trait_name a b = compare a.tr_name b.tr_name in
    let check_it checked trait =
      let dup_err = "duplicate trait declaration" in
      match checked with 
        [] -> trait :: checked
      | last_checked :: _ -> 
        if last_checked.tr_name = trait.tr_name then
          raise (Failure (dup_err ^ ": " ^ trait.tr_name))
        else trait :: checked
    in 
    let _ = List.fold_left check_it [] (List.sort compare_trait_name to_check) in 
    let _ = List.map check_trait to_check in 
    to_check 
  in 
  let straits = check_traits traits in

  let check_struct s_decl =
    {
      ss_name = s_decl.s_name;
      ss_fields = check_binds "field" s_decl.s_fields;
    }
  in 
  let sstructs = List.map check_struct structs in


  (* add built-in functions *)
  let built_in_decls = 
    let add_bind map (name, ty) = 
      StringMap.add name {
        fd_typ = Void; fd_name = name; 
        fd_formals = [(ty, "x")];
        fd_locals = []; fd_body = [] 
      } map
    in List.fold_left add_bind StringMap.empty [ 
        ("print_str", String);
      ]
  in

  let add_func map fd =
    let built_in_err = "function " ^ fd.fd_name ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fd_name
    and make_err er = raise (Failure er)
    and n = fd.fd_name (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)
  
  (* check func*)
  let check_func func =
    let formals' = check_binds "formal" func.fd_formals in
    let locals' = check_binds "local" func.fd_locals in

    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty ( formals' @ locals' )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let rec expr = function
        Literal l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Sliteral l -> (String, SSliteral l)
      | Member (a, b) -> (Void, SMember (a, b))
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.fd_formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.fd_formals args
          in (fd.fd_typ, SCall(fname, args'))
      | Noexpr     -> (Void, SNoexpr)
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
        SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.fd_typ then SReturn (t, e') 
        else raise (
          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
          string_of_typ func.fd_typ ^ " in " ^ string_of_expr e
        ))

      (* A block is correct if each statement is correct and nothing
        follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
        let rec check_stmt_list = function
            [Return _ as s] -> [check_stmt s]
          | Return _ :: _   -> raise (Failure "nothing may follow a return")
          | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
          | s :: ss         -> check_stmt s :: check_stmt_list ss
          | []              -> []
        in 
        SBlock(check_stmt_list sl)
    in
    {
      sfd_typ = func.fd_typ;
      sfd_name = func.fd_name;
      sfd_formals = formals';
      sfd_locals = locals';
      sfd_body = match check_stmt (Block func.fd_body) with
          SBlock(sl) -> sl
        | _ -> 
          let err = "internal error: block didn't become a block?"
          in raise (Failure err)
    }

  in
  let check_impl i_decl =
    {
      si_name = i_decl.i_name;
      si_forstruct = i_decl.i_forstruct;
      si_methods = List.map check_func i_decl.i_methods;
    }
  in 
  let simpls = List.map check_impl impls in
  (List.map check_func functions, straits, sstructs, simpls)
