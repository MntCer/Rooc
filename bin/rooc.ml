(* Top-level of the Rooc compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "check and print the AST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./rooc.native [-a|-s|-l|-c] [file.rooc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in  
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> 
    let sast = Sement.check ast in
    match !action with
      Ast | Sast     -> print_string (Ast.string_of_program ast)
      | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
      | Compile -> let m = Codegen.translate sast in 	Llvm_analysis.assert_valid_module m;
      print_string (Llvm.string_of_llmodule m)
    
