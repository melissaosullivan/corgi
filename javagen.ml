open Ast
open Check

(* To Do: 
	length and access 
	D_call???
*)

let write_type = function 
	  Bool_Type -> "Boolean"
	| Int_Type -> "int"
	| String_Type -> "String"
	| Pitch_Type -> "Pitch"
	| Frac_Type -> "Frac"
	| Rhythm_Type -> "Rhythm"
	| Duration_Type -> "Duration"
	| Chord_Type -> "Chord"
	| Track_Type -> "Track"
	| Composition_Type -> "Composition"
	| PD_Type -> "PD_TYPE"

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
	| Mod -> " % "
	| _ -> raise (Failure "and/or begin applied to a java primitive")

let write_op_compares e1 op e2 =
	match op with 
	Equal -> "(" ^ e1 ^ ").equals(" ^ e2 ^ ")"
	| Less ->  "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " < 0"
	| Leq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " <= 0"
	| Greater -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " > 0"
	| Geq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " >= 0"

let rec get_typeof_dexpr = function
	  D_Bool_Lit(boolLit, t) -> t
	| D_Int_Lit(intLit, t) -> t
	| D_String_Lit(strLit, t) -> t
	| D_Frac_Lit(num_expr, denom_expr, t) -> t
	| D_Id (str, t) -> t
	| D_Array_Lit(dexpr_list, t) -> t
	| D_Unop(d_expr, uop, t) -> t
	| D_Binop (dexpr1, op, dexpr2, t) -> t
	| D_Tuple(dexpr1, dexpr2, t) -> t
	(* | D_Null_Lit -> "null" *)
	| D_Noexpr -> Null_Type
	| D_Call(str,dexpr_list,t) -> t
	| D_Access(str,dexpr,t) -> t
	

let rec write_expr = function
	  D_Bool_Lit(boolLit, t) -> string_of_bool boolLit 
	| D_Int_Lit(intLit, t) -> (match t with 
								   Int_Type -> string_of_int intLit
								 | Pitch_Type -> "new Pitch(" ^ string_of_int intLit  ^ ")"
								 | Duration_Type -> "new Duration(" ^ string_of_int intLit ^ ")" ^ string_of_int intLit
								 | _ -> raise(Failure(write_type t ^ " is not a integer")))
	| D_String_Lit(strLit, t) -> "\"" ^ strLit ^ "\""
	| D_Frac_Lit(num_expr, denom_expr, t) -> (match t with 
									  Frac_Type -> "new Frac(" ^ write_expr num_expr ^ "," ^ write_expr denom_expr ^ ")"
									| Duration_Type -> "new Duration(new Frac(" ^ write_expr num_expr ^ "," ^ write_expr denom_expr ^ "))")
	| D_Id (str, yt) -> str
	| D_Array_Lit(dexpr_list, t) -> write_array_expr dexpr_list t
	| D_Unop(d_expr, uop, t) -> write_unop_expr d_expr uop t
	| D_Binop (dexpr1, op, dexpr2, t) -> write_binop_expr dexpr1 op dexpr2 t
	| D_Tuple(dexpr1, dexpr2, t) ->  "new Pitch_Duration_Tuple(" ^ write_expr dexpr1 ^ "," ^ write_expr dexpr2 ^")"  
	(* | D_Null_Lit -> "null" *)
	| D_Noexpr -> ""
	| D_Call(str,dexpr_list,t) -> (match str with 
								  "print" -> "System.out.println("  ^ String.concat "+" (List.map tostring_str dexpr_list) ^ ")"
								  | _ -> str ^ "(" ^ String.concat "," (List.map write_expr dexpr_list) ^ ")")
	| D_Access(str,dexpr,t) -> str ^ "["  ^ write_expr dexpr ^ "]"


and write_binop_expr expr1 op expr2 t =
	let e1 = write_expr expr1 and e2 = write_expr expr2 in 
		let write_binop_expr_help e1 op e2 = 
			match t with
				Int_Type -> (match op with 
					(Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Geq | And | Or) ->  
					e1 ^ write_op_primitive op ^ e2)
			  | String_Type -> (match op with 
					 Add -> " + "
					| (Equal | Less | Leq | Greater | Geq) -> write_op_compares e1 op e2)
			  | Bool_Type -> (match op with
			  		  And -> e1 ^ " && " ^ e2
			  		| Or -> e1 ^ " || " ^ e2 
			  		| _ -> write_binop_expr expr1 op expr2 get_typeof_dexpr
			  | (Pitch_Type | Frac_Type | Rhythm_Type | Duration_Type | Chord_Type | Track_Type | Composition_Type) -> (match op with
			  		(Equal | Less | Leq | Greater | Geq) -> write_op_compares e1 op e2 
			  		| Add -> "(" ^ e1 ^ ").add(" ^ e2 ^ ")"
			  		| Sub -> "(" ^ e1 ^ ").subtract(" ^ e2 ^ ")"
			  		| Mult -> "(" ^ e1 ^ ").multiply(" ^ e2 ^ ")"
			  		| Div -> "(" ^ e1 ^ ").divide(" ^ e2 ^ ")")
		in write_binop_expr_help e1 op e2 

and write_unop_expr dexpr uop t =
	(match uop with 
	  Neg -> "-(" ^ write_expr dexpr ^ ")" 
	| Not -> "!" ^ write_expr dexpr) 

and write_array_expr dexpr_list t =
	  match t with 
	  PD_Type -> "new Pitch_Duration_Tuple[]"  ^ " {" ^ String.concat "," (List.map write_expr dexpr_list) ^ "}"
	| _ -> "new " ^ write_type t ^ "[]"  ^ " {" ^ String.concat "," (List.map write_expr dexpr_list) ^ "}"

and write_tostr_class dexpr =
	let t = get_typeof_dexpr dexpr in
	match t with  
		  Bool_Type -> "Boolean"
		| Int_Type -> "Integer"
		| _ -> raise (Failure "toString method should already be in class")

and tostring_str dexpr =
	let t = get_typeof_dexpr dexpr in
	match t with  
		  (Bool_Type | Int_Type) -> write_tostr_class dexpr ^ ".toString(" ^ write_expr dexpr ^ ")"
		| String_Type -> write_expr dexpr
		| _ -> write_expr dexpr ^ ".toString()"

	

let write_scope_var_decl svd =
	let (n, b, t, _) = svd in write_type t ^ " " ^ n ^ ";\n"

let write_scope_var_decl_func svd =
	let (n, b, t, _) = svd in write_type t ^ " " ^ n

let write_assign name dexpr t =
	(match t with
	  Bool_Type | Int_Type | String_Type | Frac_Type -> name ^ " = " ^ write_expr dexpr
	| Pitch_Type | Duration_Type | Rhythm_Type | Chord_Type | Track_Type | Composition_Type -> name ^ " = new " ^ write_type t ^ "(" ^ write_expr dexpr ^ ")"
	| _ -> raise(Failure(write_type t ^ " is not a valid assign_type")))

let rec write_stmt = function
	  D_CodeBlock(dblock) -> write_block dblock 
	| D_Expr(dexpr) -> write_expr dexpr ^ ";"
	| D_Assign (name, dexpr, t) -> write_assign name dexpr t ^ ";\n"
	| D_Return(dexpr) -> "return " ^ write_expr dexpr ^ ";\n"
    | D_If(dexpr, dstmt1, dstmt2) -> "if(" ^ write_expr dexpr ^  ")" ^  write_stmt dstmt1 ^ "else"  ^ write_stmt dstmt2
    | D_For(dstmt1, dstmt2, dstmt3, dblock) -> "for(" ^ write_stmt dstmt1  ^ " ; " ^ write_stmt dstmt2 ^ " ; " ^ write_stmt dstmt3 ^ ")" 
    | D_While(dexpr, dblock) -> "while(" ^ write_expr dexpr ^ ")"  ^ write_block dblock
	
and write_block dblock =
	"{\n" ^  String.concat "\n" (List.map write_scope_var_decl dblock.d_locals) ^ String.concat "\n" (List.map write_stmt dblock.d_statements) ^ "\n}"
 
let write_func dfunc =
	match dfunc.d_fname with
	"main" -> "public static void main(String[] args)" ^ write_block dfunc.d_fblock
	| _ -> "static " ^ write_type dfunc.d_ret_type ^ " " ^ dfunc.d_fname ^ "("  ^ String.concat "," (List.map write_scope_var_decl_func dfunc.d_formals) ^ ")" ^ write_block dfunc.d_fblock

let write_pgm pgm = 
	 "public class Intermediate {\n" ^ String.concat "\n" (List.map write_scope_var_decl pgm.d_gvars) ^ String.concat "\n" (List.map write_func pgm.d_pfuncs) ^ "}"
