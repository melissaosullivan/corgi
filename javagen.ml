open Ast
open Check

(* To Do: 
	length and access 
	D_call???
*)

let write_type = function 
	  Bool_Type -> "bool"
	| Int_Type -> "int"
	| String_Type -> "String"
	| Pitch_Type -> "Pitch"
	| Frac_Type -> "Frac"
	| Rhythm_Type -> "Rhythm"
	| Duration_Type -> "Duration"
	| Chord_Type -> "Chord"
	| Track_Type -> "Track"
	| Composition_Type -> "Composition"

let write_types ts =
	match ts with Corgi_Prim(t) -> write_type t 

let write_op_primitive = function
	Add -> " + "
	| Sub -> " - "
	| Mult -> " * "
	| Div -> " / "
	| Equal -> " == "
	| Neq -> " != "
	| Less -> " < " 	
	| Leq -> " <= "
	| Greater -> " > "
	| Geq -> " >= "

let write_op_compares e1 op e2 =
	match op with 
	  Equal -> "(%s).equals(%s)" e1 e2
	| Less ->  "(%s).compareTo(%s) < 0" e1 e2
	| Leq -> "(%s).compareTo(%s) <= 0" e1 e2
	| Greater -> "(%s).compareTo(%s) > 0" e1 e2
	| Geq -> "(%s).compareTo(%s) >= 0" e1 e2

let write_unop_expr dexpr uop t =
	match uop with 
	  Neg -> "-(%s)" write_expr dexpr
	| Not -> "!" write_expr dexpr 

let write_binop_expr expr1 op expr2 t =
	let e1 = write_expr expr1 and e2 = write_expr expr2 in 
		let write_binop_expr_help e1 op e2 = 
			match get_type_of t with
				Int_Type -> match op with 
					(Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Geq | And | Or) ->  
					e1 ^ write_op_primitive op ^ e2
			  | String_Type -> match op with 
					 Add -> " + "
					| (Equal | Less | Leq | Greater | Geq) -> write_op_compares e1 op e2
			  | Bool_Type -> match op with
			  		And -> e1 ^ " && " ^ e2
			  | (Pitch_Type | Frac_Type | Rhythm_Type | Duration_Type | Chord_Type | Track_Type | Composition_Type) -> match op with
			  		(Equal | Less | Leq | Greater | Geq) -> write_op_compares e1 op e2 
			  		| Add -> "(%s).add(%s)" e1 e2
			  		| Sub -> "(%s).subtract(%s)" e1 e2
			  		| Mult -> "(%s).multiply(%s)" e1 e2
			  		| Divide -> "(%s).divide(%s)" e1 e2
		in write_binop_expr_help e1 op e2 


let write_array_expr dexpr_list t =
	"new %s[]" write_type t ^ " {" ^ String.concat "," (List.map write_expr dexpr_list) ^ "}"

let write_expr = function
	  D_Bool_Lit(boolLit, t) -> string_of_bool boolLit 
	| D_Int_Lit(intLit, t) -> match t with 
								  Int_Type -> string_of_int intLit
								  Pitch_Type -> "new Pitch(%d)" intLit
								  Duration_Type -> "new Duration(%d)" intLit   
	| D_String_Lit(strLit, t) -> strLit
	| D_Frac_Lit(num, denom, t) -> match t with 
									  Frac_Type -> "new Frac(%d, %d)" num denom
									| Duration_Type -> "new Duration(new Frac(%d, %d))" num denom 
	| D_Id (str, _) -> str
	| D_Array_Lit(dexpr_list, t) -> write_array_expr dexpr_list t
	| D_uop(d_expr, uop, t) -> write_unop_expr d_expr uop t
	| D_Binop (dexpr1, op, dexpr2, t) -> write_binop_expr dexpr op dexpr2 t
	| D_Tuple(dexpr1, dexpr2) -> "new Pitch_Duration_Tuple(%s, %s)" write_expr dexpr1 dexpr2
	| D_Null_Lit -> "null"
	| D_Noexpr -> ""

let write_assign name dexpr t =
	match t with
	  (Bool_Type | Int_Type | String_Lit | Frac_Lit) -> "%s = %s" name write_expr dexpr
	| (Pitch_Type | Duration_Type | Rhythm_Type | Chord_Type | Track_Type | Composition_Type) ->  "%s = new %s(%s)" name write_type t write_expr dexpr

let write_stmt = function
	  D_CodeBlock(dblock) -> write_block dblock 
	| D_Expr(dexpr) -> write_expr dexpr ^ ";"
	| D_Assign (name, dexpr, t) -> write_assign name dexpr t ^ ";"
	| D_Return(dexpr) -> "return " ^ write_expr expr ^ ";"
    | D_If(dexpr, dblock1, dblock2) -> "if(%s)" ^ write_block dblock1 ^ write_block dblock2
    | D_For(dexpr1, dexpr2, dexpr3, dblock) -> ^ ";"
    | D_While(dexpr, dblock) -> "while(%s)" write_expr dexpr ^ write_block dblock
	
let write_block dblock =
	"{\n" ^  String.concat "\n" (List.map write_scope_var_decl dblock.d_locals) ^ String.concat "\n" (List.map write_stmt dblock.d_statements) ^ "\n}"

let write_scope_var_decl svd =
	match svd with scope_var_decl(str,ts,i) -> str ^ write_types t
 
let write_func dfunc =
	write_types dfunc.d_ret_type ^ " %s(" dfunc.d_fname ^ String.concat "," (List.map write_scope_var_decl dfunc.d_formals) ^ ")" ^ write_block dfunc.d_fblock

let write_pgm pgm =
	match pgm with dprogram(svdlst, dfunclst) -> String.concat "\n" (List.map write_scope_var_decl svdlst) ^ String.concat "\n" (List.map write_func funclst)
