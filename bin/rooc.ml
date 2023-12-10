(* Top-level of the Rooc compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = 
  |  Ast 
  | Sast 
  | LLVM_IR 
  | Compile

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./rooc.native [-a|-s|-l|-c] [file.rooc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.roc_module Scanner.token lexbuf in  
  match !action with
      Ast -> print_string (Ast.string_of_module ast)
    | _ -> 
      let sast = Semant.analyse_module ast in
      match !action with
        Ast     -> print_string (Ast.string_of_module ast)
      | Sast    -> print_string (Sast.string_of_smodule sast)
      | LLVM_IR -> 
        let the_context= Llhelper.the_global_context in
        let translated_module = Lltrans.trans_module sast the_context in
        print_string(Llvm.string_of_llmodule(translated_module))

      | Compile -> 
        let the_context= Llhelper.the_global_context in
        let translated_module = Lltrans.trans_module sast the_context in
        let () = Llvm_analysis.assert_valid_module translated_module in
        print_string (Llvm.string_of_llmodule translated_module);