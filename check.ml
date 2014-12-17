open Ast

let fst_of_three (t, _, _) = t
let snd_of_three (_, t, _) = t
let thrd_of_three (_, _, t) = t

type d_expr =
	  D_Bool_Lit of bool * prim_type
	| D_Int_Lit of int * prim_type
	| D_String_Lit of string * prim_type
	| D_Frac_Lit of d_expr * d_expr  * prim_type (* Expressions of type int *)
	| D_Id of string * prim_type
	| D_Array_Lit of d_expr list * prim_type
	| D_Binop of d_expr * op * d_expr * prim_type
	| D_Unop of d_expr * uop * prim_type
	| D_Call of string * d_expr list * prim_type
	| D_Tuple of d_expr * d_expr * prim_type  (* Come back and fix tuples *)
	| D_Access of string * d_expr * prim_type
	| D_Noexpr 

type d_stmt = 
	  D_CodeBlock of d_block
	| D_Expr of d_expr
	| D_Assign of string * d_expr * prim_type
	| D_Return of d_expr
	| D_If of d_expr * d_stmt * d_stmt (* stmts of type D_CodeBlock *)
	| D_For of d_stmt * d_stmt * d_stmt * d_block (* stmts of type D_Assign | D_Noexpr * D_Expr of type bool * D_Assign | D_Noexpr *)
	| D_While of d_expr * d_block

and d_block = {
	d_locals : scope_var_decl list;
	d_statements: d_stmt list;
	d_block_id: int;
}


type d_func = {
	d_fname : string;
	d_ret_type : prim_type; (* Changed from types for comparison error in verify_stmt*)
	d_formals : scope_var_decl list;
	d_fblock : d_block;
}

type d_program = {
	d_gvars: scope_var_decl list;
	d_pfuncs: d_func list;
}

let type_of_expr = function
	D_Int_Lit(_,t) -> t
  | D_Bool_Lit(_,t) -> t
  | D_String_Lit(_,t) -> t
  | D_Frac_Lit(_,_,t) -> t
  | D_Id(_,t) -> t
  | D_Binop(_,_,_,t) -> t 
  | D_Array_Lit (_, t) -> t
  | D_Unop (_, _, t) -> t 
  | D_Call (_, _, t) -> t
  | D_Tuple (_, _, t) -> t (* Come back and fix tuples *)
  | D_Access (_, _, t) -> t
  | D_Noexpr -> Null_Type 

(* let rec compare_list lst1 lst2 =
	match (lst1, lst2) with
	([], []) -> true
	| (h1::t1, h2::t2) -> (h1 = h2) && compare_list t1 t2
	| _ -> false *)

let rec map_to_list_env func lst env =
	match lst with
		  [] -> []
		| head :: tail ->
			let r = func head env in 
				r :: map_to_list_env func tail env

let verify_gvar gvar env = 
	let decl = Symtab.symtab_find (fst_of_three gvar) env in 
	let id = Symtab.symtab_get_id (fst_of_three gvar) env in
	match decl with 
		Var_Decl(v) -> let (vname, varray, vtype, id) = v in
			(vname, varray, vtype, id)
		| _ -> raise(Failure("global" ^ (fst_of_three gvar) ^ " not a variable"))

let verify_var var env = 
	let decl = Symtab.symtab_find (fst_of_three var) env in
	let id = Symtab.symtab_get_id (fst_of_three var) env in
	match decl with
		Func_Decl(f) -> raise(Failure("symbol is not a variable"))
	  | Var_Decl(v) -> let (vname, varray, vtype, id) = v in
			(vname, varray, vtype, id)

let verify_is_func_decl name env =
	let decl = Symtab.symtab_find name env in
	match decl with 
		Func_Decl(f) -> name
		| _ -> raise(Failure("id " ^ name ^ " not a function"))

let verify_unop_and_get_type e unop =
	let e_type = type_of_expr e in
	match e_type with 
		  Bool_Type -> 
			if unop = Neg then raise (Failure "incorrect negation operator applied to Bool")
			else Bool_Type
		| Int_Type -> if unop = Not then raise (Failure "incorrect negation operator applied to Int")
			else Int_Type
		| Frac_Type -> if unop = Not then raise (Failure "incorrect negation operator applied to Frac")
			else Frac_Type
		| _ -> raise (Failure "negation operator applied to type that doesn't support negation")

let verify_id_get_type id env = 
	let decl = Symtab.symtab_find id env in
	match decl with
		Var_Decl(v) -> let (_, _, t, _) = v in t
		| _ -> raise(Failure("id " ^ id ^ " not a variable."))

let verify_id_is_array id env = 
	let decl = Symtab.symtab_find id env in
	match decl with
		Var_Decl(v) -> let(_, is_array, _, _ ) = v in is_array
		| _ -> raise(Failure("id " ^ id ^ " not an array.")) 

let verify_binop l r op =
	let tl = type_of_expr l in
	let tr = type_of_expr r in
	match op with 
		Add | Sub  -> (match (tl, tr) with
			Int_Type, Int_Type -> Int_Type
			| Int_Type, Pitch_Type -> Pitch_Type	
			| Int_Type, Frac_Type -> Frac_Type
			| Int_Type, Duration_Type -> Duration_Type
			| Pitch_Type, Int_Type -> Pitch_Type
			| Pitch_Type, Pitch_Type -> Pitch_Type
			| Frac_Type, Int_Type -> Frac_Type
			| Frac_Type, Frac_Type -> Frac_Type
			| Frac_Type, Duration_Type -> Duration_Type
			| Duration_Type, Int_Type -> Duration_Type
			| Duration_Type, Frac_Type -> Duration_Type
			| Duration_Type, Duration_Type -> Duration_Type
			| _, _ -> raise(Failure("Cannot apply + - op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| Mult | Div | Mod -> (match (tl, tr) with
			(* I removed duration * duration operations, otherwise we can merge both sets *)
			Int_Type, Int_Type -> Int_Type
			| Int_Type, Pitch_Type -> Pitch_Type	
			| Int_Type, Frac_Type -> Frac_Type
			| Int_Type, Duration_Type -> Duration_Type
			| Pitch_Type, Int_Type -> Pitch_Type
			| Pitch_Type, Pitch_Type -> Pitch_Type
			| Frac_Type, Int_Type -> Frac_Type
			| Frac_Type, Frac_Type -> Frac_Type
			| Frac_Type, Duration_Type -> Duration_Type
			| Duration_Type, Int_Type -> Duration_Type
			| Duration_Type, Frac_Type -> Duration_Type
			| _, _ -> raise(Failure("Cannot apply */% op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| Equal | Neq -> if tl = tr then Bool_Type else (match(tl, tr) with
			| Int_Type, Pitch_Type -> Bool_Type	
			| Int_Type, Frac_Type -> Bool_Type
			| Int_Type, Duration_Type -> Bool_Type
			| Pitch_Type, Int_Type -> Bool_Type
			| Frac_Type, Int_Type -> Bool_Type
			| Frac_Type, Duration_Type -> Bool_Type
			| Duration_Type, Int_Type -> Bool_Type
			| Duration_Type, Frac_Type -> Bool_Type
			| _, _ -> raise(Failure("Cannot apply == !=  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| Less | Greater | Leq | Geq-> (match (tl, tr) with
			Int_Type, Int_Type -> Bool_Type
			| Int_Type, Pitch_Type -> Bool_Type	
			| Int_Type, Frac_Type -> Bool_Type
			| Int_Type, Duration_Type -> Bool_Type
			| Pitch_Type, Int_Type -> Bool_Type
			| Pitch_Type, Pitch_Type -> Bool_Type
			| Frac_Type, Int_Type -> Bool_Type
			| Frac_Type, Frac_Type -> Bool_Type
			| Frac_Type, Duration_Type -> Bool_Type
			| Duration_Type, Int_Type -> Bool_Type
			| Duration_Type, Frac_Type -> Bool_Type
			| Duration_Type, Duration_Type -> Bool_Type
			| String_Type, String_Type -> Bool_Type
			| _, _ -> raise(Failure("Cannot apply < > <= >=  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))
		| And | Or -> (match (tl, tr) with
			Bool_Type, Bool_Type -> Bool_Type
			| _, _ -> raise(Failure("Cannot apply && ||  op to types " ^ string_of_prim_type tl ^ " + " ^ string_of_prim_type tr)))

let verify_tuple_types p d =
	match type_of_expr p with
		Int_Type | Pitch_Type -> (match type_of_expr d with
			Int_Type | Frac_Type | Duration_Type -> true
			| _ -> raise(Failure("Second term in tuple must be of type (*,)"))
		)
		| _ -> raise(Failure("First term in tuple must be of type pitch (*,)"))
 
(*let verify_assign id *)
let rec verify_expr expr env =
	let () = print_endline ("verifying expr: " ^ string_of_expr expr) in
	match expr with                                         (* expr evaluates to *)
		  Bool_Lit(b)     -> D_Bool_Lit(b,Bool_Type)        (* D_Bool_Lit *)
		| Int_Lit(i)      -> D_Int_Lit(i, Int_Type)		    (* D_Int_Lit *)
		| String_Lit(s)   -> D_String_Lit(s, String_Type)   (* D_String_Lit*)
		| Frac_Lit(n,d)   ->                                (* D_Frac_Lit *)
			let vn = verify_expr n env in 
			let vd = verify_expr d env in
			if type_of_expr vn <> Int_Type or type_of_expr vd <> Int_Type then (* Come back and also accept Frac as n and d *)
				raise(Failure("Fraction literal must have integer numerator and denominator."))
			else D_Frac_Lit(vn, vd, Frac_Type)
		| Id(s)           -> 								(* D_Id_Lit *)
			let vid_type = verify_id_get_type s env in
			D_Id(s, vid_type)           
		| Binop(l, op, r) -> 
			let vl = verify_expr l env in
			let vr = verify_expr r env in
			let vtype = verify_binop vl vr op in
			D_Binop(vl, op, vr, vtype)                    (* D_Binop *)
		| Unop(e, uop) -> 
			let ve = verify_expr e env in
			let ve_type = verify_unop_and_get_type ve uop in
			D_Unop(ve, uop, ve_type)                        (* D_Unop *)
		| Array_Lit (ar) -> 
			let (va, va_type) = verify_array ar env in
			D_Array_Lit(va, va_type)                        (* D_Array_Lit *)
		| Call (name, args) -> 
			let va = verify_expr_list args env in 
			let vt = verify_call_and_get_type name va env in
			D_Call(name, va, vt)                             (* D_Call *)
		| Tuple(e1, e2) ->                                   (* D_Tuple *)
			let ve1 = verify_expr e1 env in
			let ve2 = verify_expr e2 env in
			if verify_tuple_types ve1 ve2 then D_Tuple(ve1, ve2, PD_Type)
			else raise(Failure("Invalid tuple."))
		| Access(ar, i) ->
			let is_array = verify_id_is_array ar env in
			let ar_type = verify_id_get_type ar env in
			let vi = verify_expr i env in
			let vit = type_of_expr vi in 
			if vit = Int_Type && is_array then D_Access(ar, vi, ar_type)
			else raise(Failure("symbol " ^ ar ^ " must be an array, index must be of type int")) 
		| Noexpr -> D_Noexpr


and verify_array arr env = 
	match arr with
	[] -> ([], Null_Type) (* Empty *)
	| head :: tail ->
		let verified_head = verify_expr head env in
		let head_type = type_of_expr verified_head in
		(* Verify that other elements are valid expressions with consistent types *)
		(* Decision time: Only use pd_type ? *)
			let rec verify_list_and_type l t e = match l with
				[] -> ([], t)
				| hd :: tl -> 
					let ve = verify_expr hd e in
					let te = type_of_expr ve in
					if t = te then (ve :: (fst (verify_list_and_type tl te e)), t) 
					else raise (Failure "Elements of inconsistent types in Array_Lit")
			in
		(verified_head :: (fst (verify_list_and_type tail head_type env)), head_type) 

and verify_expr_list lst env =
	match lst with
	[] -> []
	| head :: tail -> verify_expr head env :: verify_expr_list tail env

and verify_call_and_get_type name vargs env =
	let decl = Symtab.symtab_find name env in (* function name in symbol table *)
	let fdecl = match decl with
		Func_Decl(f) -> f                     (* check if it is a function *)
		| _ -> raise(Failure (name ^ " is not a function")) in
	if name = "print" then Int_Type           (* Add more builtins when we have more builtins *)
	else 
		let (_,rtype,params,_) = fdecl in
		if (List.length params) = (List.length vargs) then
			let arg_types = List.map type_of_expr vargs in
			if (* compare_list_types *) params = arg_types then rtype
			else raise(Failure("Argument types in " ^ name ^ " call do not match formal parameters."))
		else raise(Failure("Function " ^ name ^ " takes " ^ string_of_int (List.length params) ^
						   " arguments, called with " ^ string_of_int (List.length vargs)))

let verify_id_match_type (id:string) ve env = (* Add support for assigning compatible types *)
	let decl = Symtab.symtab_find id env in 
	let vdecl = match decl with (* check that id refers to a variable *)
	Var_Decl(v) -> v
	| _ -> raise(Failure (id ^ " is not a variable")) in
	let (_,is_array, id_type, _) = vdecl in
	let vt = type_of_expr ve in

	let id_is_array = verify_id_is_array id env in

	if id_is_array then
		(match ve with
		D_Array_Lit(_, _) -> if id_type = vt then id_type(* Check that it goes into id's type *)
			else (match(id_type, vt) with
				Rhythm_Type, Duration_Type
				| Chord_Type, PD_Type
				| Composition_Type, Track_Type
				| Track_Type, Chord_Type -> id_type
				| _, _ -> raise(Failure("Cannot assign " ^ string_of_prim_type vt ^ " to " ^ id ^ " of type " ^ string_of_prim_type id_type)))
		| D_Id(s, _) -> if verify_id_is_array s env then (
						if id_type = vt then id_type
						else (match(id_type, vt) with (* Compatible simple types *)
							Frac_Type, Int_Type  
							| Duration_Type, Int_Type 
							| Duration_Type, Frac_Type
							| Pitch_Type, Int_Type -> id_type
							| _, _ -> raise(Failure("Cannot assign " ^ string_of_prim_type vt ^ " to " ^ id ^ " of type " ^ string_of_prim_type id_type ))
						)
				   ) else raise(Failure("Cannot assign single element to array."))
		| _ -> raise(Failure("Cannot assign " ^ string_of_prim_type vt ^ " to " ^ id ^ " of type " ^ string_of_prim_type id_type )))
	else (* id is not an array *)
		if id_type = vt then id_type else (match (id_type, vt) with
			Frac_Type, Int_Type  
			| Duration_Type, Int_Type 
			| Duration_Type, Frac_Type
			| Pitch_Type, Int_Type 
			| Rhythm_Type, Duration_Type -> id_type
			| _, _ -> raise(Failure("Cannot assign " ^ string_of_prim_type vt ^ " to " ^ id ^ " of type " ^ string_of_prim_type id_type )))

let rec verify_stmt stmt ret_type env =
	let () = print_endline ("verifying statement: " ^ string_of_stmt stmt) in
	match stmt with
	Return(e) ->
		let verified_expr = verify_expr e env in
		if ret_type = type_of_expr verified_expr then D_Return(verified_expr) 
		else raise(Failure "return type does not match") 
	| Expr(e) -> 
		let verified_expr = verify_expr e env in
		D_Expr(verified_expr)
	| Assign(id, e) -> (* Verify that id is compatible type to e *)
		let ve = verify_expr e env in
		let vid_type = verify_id_match_type id ve env in 
		D_Assign(id, ve, vid_type) 
	| Block(b) -> 
		let verified_block = verify_block b ret_type (fst env, b.block_id) in
		D_CodeBlock(verified_block)
	| If(e, b1, b2) ->
		let verified_expr = verify_expr e env in
		if (type_of_expr verified_expr) = Bool_Type then
			let vb1 = verify_block b1 ret_type (fst env, b1.block_id) in
			let vb2 = verify_block b2 ret_type (fst env, b2.block_id) in
			D_If(verified_expr, D_CodeBlock(vb1), D_CodeBlock(vb2))
		else raise(Failure("Condition in if statement must be a boolean expression."))
	| For(assignment1, condition, assignment2, block) ->
		let va1 = (match assignment1 with
			Assign(_, _) | Expr(_) ->  verify_stmt assignment1 ret_type env 
			| _ -> raise(Failure("First term in For statement must be assignment or no expression. (*;;)"))) in
		let vc = (match condition with
			Expr(e) -> 
				let ve = verify_expr e env in
				let vt = type_of_expr ve in
				if vt = Bool_Type or vt = Null_Type then verify_stmt condition ret_type env 
				else raise(Failure("Condition in For statement must be boolean or no expression. (;*;)"))
			| _ -> raise(Failure("Condition in For statement must be boolean or no expression. (;*;)"))) in
		let va2 = (match assignment1 with
			Assign(_, _) | Expr(_) ->  verify_stmt assignment2 ret_type env 
			| _ -> raise(Failure("Last term in For statement must be assignment or no expression. (;;*)"))) in
		let vb = verify_block block ret_type (fst env, block.block_id) in
		D_For(va1, vc, va2, vb)
	| While(condition, block) ->
		let vc = verify_expr condition env in
		let vt = type_of_expr vc in 
		if vt = Bool_Type then 
			let vb = verify_block block ret_type (fst env, block.block_id) in
			D_While(vc, vb)
		else raise(Failure("Condition in While statement must be boolean."))



and verify_stmt_list stmt_list ret_type env = 
	match stmt_list with
		  [] -> []
		| head :: tail -> (verify_stmt head ret_type env) :: (verify_stmt_list tail ret_type env)

and verify_block block ret_type env =
	let verified_vars = map_to_list_env verify_var block.locals (fst env, block.block_id) in
	let verified_stmts = verify_stmt_list block.statements ret_type env in 
	{ d_locals = verified_vars; d_statements = verified_stmts; d_block_id = block.block_id }

(*verify formals, get return type, verify function name, verify fblock *)
let verify_func func env =
	(* let () = Printf.printf "verifying function \n" in *)
	let verified_block = verify_block func.fblock func.ret_type (fst env, func.fblock.block_id) in
	(* let () = Printf.printf "func.fname" in *) 
	let verified_formals = map_to_list_env verify_var func.formals (fst env, func.fblock.block_id) in
	let verified_func_decl = verify_is_func_decl func.fname env in 
	{ d_fname = verified_func_decl; d_ret_type = func.ret_type; d_formals = verified_formals; d_fblock = verified_block }

let verify_semantics program env = 
	let (gvar_list, func_list) = program in 
	let verified_gvar_list = map_to_list_env verify_var gvar_list env in 
	let () = Printf.printf "after verifying gvars \n" in
	let verified_func_list = map_to_list_env verify_func func_list env in
	let () = Printf.printf "after verifying functions \n" in
		{ d_pfuncs = verified_func_list; d_gvars = verified_gvar_list}
