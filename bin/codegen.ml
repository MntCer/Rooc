(* Code generating for Rooc *)

open Sast
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (functions, traits, structs, impls) =
  let context    = L.global_context () in

  (* Add types *)
  (* TODO *)
  let i32t_t     = L.i32_type    context
  (* ... *)

  let the_module = L.create_module context "Rooc" in

  let ltype_of_typ = function
      A.Int      -> i32_t
  (* ... *)
  in

  let trait_decls : (L.llvalue * trait_decl) StringMap.t = 
  (* TODO *)
  (* record traits *)
  StringMap.empty in

  let struct_decls : (L.llvalue * struct_decl) StringMap.t =
  (* record structs *)
  StringMap.empty in

  let impl_decls : (L.llvalue * impl_decl) StringMap.t =
  (* TODO *)
  (* record implmentations *)
  StringMap.empty in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
  (* record function decls *)
  let function_decl m fdecl =
    let name = fdecl.fs_name
    and formal_types =
      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.fs_formals)
    in let ftype = L.function_type (ltype_of_typ fdecl.fs_typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
  List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl = 

    let rec expr builder ((_, e) : sexpr) = match e with
        SLiteral i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                         let _  = L.build_store e' (lookup s) builder in e'
      (* TODO *)
      (* ... *)

    let rec stmt builder = function
        SBlock sl -> List.fold_left stmt builder sl
      (* TODO *)
      (* ... *)
      | _ -> builder
  
  let build_method_body idecl = 
    (* TODO: deal with implementations' methods *)  
  in
  List.iter build_function_body functions;
  List.iter build_method_body impls;
  the_module

