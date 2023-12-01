(* Top-level of the Rooc compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast 
            | Sast 
            (* | LLVM_IR | Compile *)

let () =
  let action = ref Ast in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
  ] in  
  let usage_msg = "usage: ./rooc.native [-a|-s|-l|-c] [file.rooc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.roc_module Scanner.token lexbuf in  
  match !action with
      Ast -> print_string (Ast.string_of_module ast)
    | _ -> 
      let sast = Semantic.analyse_module ast in
      match !action with
        Ast     -> print_string (Ast.string_of_module ast)
      | Sast    -> print_string (Ast.string_of_module sast)
  