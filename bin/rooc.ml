(* Top-level of the Rooc compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast 
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
  let ast = Parser.program Scanner.token lexbuf in  
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  (* | _ -> let sast = Semant.check ast in *)
  | _ -> let sast = ast in
    match !action with
      Ast     -> print_string (Ast.string_of_program ast)
    (* | Sast    -> print_string (Sast.string_of_sprogram sast) *)
    | Sast    -> print_string (Ast.string_of_program sast)
