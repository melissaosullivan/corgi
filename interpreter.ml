open Ast

let string_of_elseifs elseifs = 
	String.concat "" (List.map function(expr, stmt) -> string_of_expr expr ^ string_of_stmt stmt)
	

let rec string_of_stmt = function
	Block(stmts) ->
		"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr expr ^ ";\n"
	| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
	| If(e, s, ei, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s ^ 
		string_of_elseifs ei
	| If(e, s1, ei, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
		string_of_stmt s1 ^ string_of_elseifs ei ^ "else\n" ^ string_of_stmt s2
	| For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
	| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
	| Loop(v, a, s) -> "loop (" ^ v ^ ":" ^ a ^ ") " ^ string_of_stmt s

let string_of_vdecl vdecl = vdecl.vtype ^ " " ^ vdecl.vname ^ ";\n"	
let string_of_pdecl pdecl = pdecl.ptype ^ " " ^ pdecl.pname	

let string_of_fdecl fdecl =
	fdecl.rtype ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_pdecl fdecl.formals) ^ ")\n{\n" ^
	String.concat "" (List.map string_of_vdecl fdecl.locals) ^
	String.concat "" (List.map string_of_stmt fdecl.body) ^
	"}\n"

let string_of_program (vars, funcs) =
	String.concat "" (List.map string_of_vdecl (List.rev vars) ) ^ "\n" ^
	String.concat "\n" (List.map string_of_fdecl (List.rev funcs) ) ^ "\n"

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf