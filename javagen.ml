open Ast
open Check

(* To Do: 
	length and access 
	D_call???
*)

type d_expr =
	  D_Bool_Lit of bool * prim_type
	| D_Int_Lit of int * prim_type
    | D_String_Lit of string * prim_type
    | D_Frac_Lit of int * int  * prim_type
    | D_Id of string * prim_type
  (*  | D_Array_Lit of d_expr list * prim_type *)
    | D_Binop of d_expr * op * d_expr * prim_type
    | D_Unop of d_expr * uop * prim_type
    | D_Call of string * d_expr list * prim_type
    | D_Tuple of d_expr * d_expr * prim_type
  (*  | D_Null_Lit
    | D_Noexpr *)

type d_stmt = 
	  D_CodeBlock of d_block
	| D_Expr of d_expr
	| D_Assign of string * d_expr * prim_type
	| D_Return of d_expr
    | D_If of d_expr * d_block * d_block
    | D_For of d_expr * d_expr * d_expr * d_block
    | D_While of d_expr * d_block

and d_block = {
	d_locals : scope_var_decl list;
    d_statements: d_stmt list;
    d_block_id: int;
}

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
	| D_Assign (name, dexpr, t) -> write_assign name dexpr t
	| D_Null_Lit -> "null"
	| D_Noexpr -> ""


let write_array_expr dexpr_list t =
	"new %s[]" write_type t ^ " {" ^ String.concat "," (List.map write_expr dexpr_list) ^ "}"

let write_assign name dexpr t =
	match t with
	  (Bool_Type | Int_Type | String_Lit | Frac_Lit) -> "%s = %s" name write_expr dexpr
	| (Pitch_Type | Duration_Type | Rhythm_Type | Chord_Type | Track_Type | Composition_Type) ->  "%s = new %s(%s)" name write_type t write_expr dexpr

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


(*prim binop/less/leq/greater/geq *)
