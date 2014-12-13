open Ast


(*
type d_expr =
	  D_Bool_Lit of bool * prim_type
	| D_Int_Lit of int * prim_type
    | D_String_Lit of string * prim_type
    | D_Frac_Lit of int * int  * prim_type
    | D_Id of string * prim_type
    | D_Array_Lit of expr list * prim_type
    | D_Binop of expr * op * expr * prim_type
    | D_Unop of expr * uop * prim_type
    | D_Create of types * string * expr  * prim_type
    | D_Assign of string * expr * prim_type
    | D_Call of string * expr list * prim_type
    | D_Tuple of expr * expr * prim_type
    | D_Null_Lit * prim_type
    | D_Noexpr * prim_type

type d_stmt = 
	  D_CodeBlock of d_block
	| D_Expr of d_expr
	| D_Return of d_expr
    | D_If of d_expr * d_block * d_block
    | D_For of d_expr * d_expr * d_expr * d_block
    | D_While of d_expr * d_block


type d_func = {
	d_fname : string;
	d_ret_type : types;
	d_formals : scope_var_decl list;
	c_fblock : c_block;
}

type d_program = scope_var_decl list * c_func list

*)

let verify_gvar gvar env = 
	let var = Symtab.symtab_find (fst gvar) env in 
	let id = Symtab.symtab_get_id (fst gvar) env in
	gvar

let verify_var var env =
	var
(*
let verify_block block env =
	let verified_vars = map_to_list_env verify_var block.locals env in
	let 
*)
(* type func = { ret_type : types; 
				 fname : string; 
				 formals: var list; 
				 fblock: block; }
   to verify a function we check:
   	1.) block returns return type
   	2.) check formals
   	3.) check fname
   	4.) 
	
	*)
(*
let verify_expr expr env =
	match expr with
		  Bool_Lit(b) -> D_Bool_Lit(b,Bool_Type)
		| Int_Lit(i) -> D_Int_Lit(i, Int_Type)
		| String_Lit(s) -> D_String_Lit(s, String_Type)
		| Frac_Lit(n,d) -> D_Frac_Lit(n, d, Frac_Type)
		| Id(s) -> D_Id(s, String_Type)
		| Array_Lit of expr list wtf 
	    | Binop of expr * op * expr
    	| Unop of expr * uop
        | Create of types * string * expr 
        | Assign of string * expr
        | Call of string * expr list
   	    | Tuple of expr * expr
        | Null_Lit
        | Noexpr

*) 
let verify_func func env =
	func (*
	let verified_formals = map_to_list_env verify_var func.formals env in
	let return = verify_block func.fblock env in 
	let *)

(*	match var with
		  Func_Decl(f) -> raise(Failure "symbol is function not a variable")
		| Var_Decl(v) -> gvar 

	this may or may not happen ever ? *)

let rec map_to_list_env func mlist env =
	match mlist with
		  [] -> []
		| head :: tail ->
			let r = func head env in 
				r :: map_to_list_env func tail env

let verify_semantics program env = 
	let (gvar_list, func_list) = program in 
	let verified_gvar_list = map_to_list_env verify_gvar gvar_list env in  (*we are here*)
	let verified_func_list = map_to_list_env verify_func func_list env in
		(verified_func_list, verified_gvar_list)
