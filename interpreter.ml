open Ast

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
    let env = Symtab.symtab_of_program program in
    let checked = Check.check_program program env in
    ignore checked; print_string "Passed Semantic Analysis.\n"
    
    (*prerr_string (string_of_program program) *)
    (*print_string (Symtab.string_of_symtab env) *)
