(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = AST | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", AST);  (* Print the AST only *)
            ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
            ("-c", Compile) ] (* Generate, check LLVM IR *)
    else Compile in
    let lexbuf = Lexing.from_string (Preprocess.process_files Sys.argv.(2) Sys.argv.(3) )in
    let ast = Parser.program Scanner.token lexbuf in
    Semant.check ast;
    match action with 
      AST -> print_string (Ast.string_of_program ast)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
    | Compile -> let m = Codegen.translate ast in
      Llvm_analysis.assert_valid_module m;
      print_string (Llvm.string_of_llmodule m)