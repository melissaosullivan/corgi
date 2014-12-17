
type action = Ast | Symtab | Sem | Javagen | Help

let usage (name:string) =
  "usage:\n" ^ name ^ "\n" ^
    "        -ast < source.corg              (Print AST of source)\n" ^
    "        -sym < source.corg              (Print Symbol Table of source)\n" ^
    "		     -sem < source.corg				       (Print Semantic Analysis of source)\n" ^
    "        -javagen < source.corg          (Print java intermediate code of source)\n"

let _ =
  let action = 
  if Array.length Sys.argv > 0 then
    (match Sys.argv.(1) with
        "-ast" -> Ast
      | "-sym" -> Symtab
      | "-sem" -> Sem
      | "-javagen" -> Javagen
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
        | Sem -> let env = Symtab.symtab_of_program program in
            		let checked = Check.verify_semantics program env in
    					ignore checked; print_string "Passed Semantic Analysis.\n"
        | Javagen -> let env = Symtab.symtab_of_program program in
                let checked = Check.verify_semantics program env in
                let outstring = Javagen.write_pgm checked in
                print_string outstring
        | Help -> print_endline (usage Sys.argv.(0)))