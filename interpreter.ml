
type action = Ast | Symtab | Help

let usage (name:string) =
  "usage:\n" ^ name ^ "\n" ^
    "        -ast < source.lrx              (Print AST of source)\n" ^
    "        -sym < source.lrx              (Print Symbol Table of source)\n"

let _ =
  let action = 
  if Array.length Sys.argv > 0 then
    (match Sys.argv.(1) with
        "-ast" -> Ast
      | "-sym" -> Symtab
      | _ -> Help)  
  else Help in   

  match action with
      Help -> print_endline (usage Sys.argv.(0)) 
    | _ ->
      let lexbuf = Lexing.from_channel stdin in
      let program = Parser.program Scanner.token lexbuf in
      (match action with
          Ast -> let listing = Ast.string_of_program program
                 in prerr_string listing
        | Symtab -> let env = Symtab.symtab_of_program program in
                    prerr_string (Symtab.string_of_symtab env)
        | Help -> print_endline (usage Sys.argv.(0)))