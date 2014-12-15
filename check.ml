open Ast

let fst_of_three (t, _, _) = t
let snd_of_three (_, t, _) = t

type d_expr =
	  D_Bool_Lit of bool * prim_type
	| D_Int_Lit of int * prim_type
    | D_String_Lit of string * prim_type
    | D_Frac_Lit of int * int  * prim_type
    | D_Id of string * prim_type
    | D_Array_Lit of d_expr list * prim_type
    | D_Binop of d_expr * op * d_expr * prim_type
    | D_Unop of d_expr * uop * prim_type
    | D_Call of string * d_expr list * prim_type
    | D_Tuple of d_expr * d_expr * prim_type  (* Come back and fix tuples *)
    | D_Access of string * d_expr * prim_type
  (*  | D_Null_Lit *)
    | D_Noexpr 

type d_stmt = 
	  D_CodeBlock of d_block
	| D_Expr of d_expr
	| D_Assign of string * d_expr * prim_type
	| D_Return of d_expr
    | D_If of d_expr * d_stmt * d_stmt (* stmts of type D_CodeBlock *)
    | D_For of d_expr * d_expr * d_expr * d_block
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

type d_program = scope_var_decl list * d_func list




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
 (* | D_Unop of d_expr * uop * prim_type
  | D_Assign of string * d_expr * prim_type
  | D_Call of string * d_expr list * prim_type
  | D_Tuple of d_expr * d_expr * prim_type
  | D_Null_Lit
  | D_Noexpr
  | C_Binop(t,_,_,_) -> t
  | C_Unop(t,_,_) -> t 
  | C_Id(t,_,_) -> t
  | C_Assign(t,_,_) -> t
  | C_Tree(t, d, _, _) -> 
    (match t with
        Lrx_Atom(t) -> Lrx_Tree({datatype = t; degree = Int_Literal(d)})
      | _ -> raise (Failure "Tree type must be Lrx_atom"))
  | C_Call(f,_) -> let (_,r,_,_) = f in r
  | (C_Noexpr | C_Null_Literal) -> raise (Failure("Type of expression called on Null_Literal or Noexpr")) *)

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
	let decl = Symtab.symtab_find (fst gvar) env in 
	let id = Symtab.symtab_get_id (fst gvar) env in
	gvar

let verify_var var env = 
	let decl = Symtab.symtab_find (fst var) env in
	let id = Symtab.symtab_get_id (fst var) env in
	match decl with
		Func_Decl(f) -> raise(Failure("symbol is not a variable"))
	  | Var_Decl(v) -> (fst_of_three v, snd_of_three v, id)

let verify_is_func_decl fdecl env =
	fdecl

(*
let rec check_statement stmt ret_type env in_loop = 
	match stmt with
		CodeBlock(b) ->
       		let checked_block = check_block b ret_type env in_loop in
       		C_CodeBlock(checked_block)
    | 	Return(e) -> 
       		let verified_expr = verify_expr e env in
       		let t = type_of_expr checked in
       		if t = ret_type then C_Return(checked) else
       		raise (Failure("function return type " ^ string_of_var_type t ^ "; type " ^ string_of_var_type ret_type ^ "expected"))
     | Expr(e) -> C_Expr(check_expr e env) 
     | If(e, b1, b2) -> 
        let c = check_expr e env in
        let t = type_of_expr c in
        (match t with
          Lrx_Atom(Lrx_Bool) -> C_If(c, check_block b1 ret_type env in_loop, check_block b2 ret_type env in_loop)
        | _ -> raise (Failure "If statement must evaluate on boolean expression"))
     | For(e1, e2, e3, b) -> 
       let (c1, c2, c3) = (check_expr e1 env, check_expr e2 env, check_expr e3 env) in
       if(type_of_expr c2 = Lrx_Atom(Lrx_Bool)) then
       C_For(c1, c2, c3, check_block b ret_type env (in_loop + 1))
		   else raise(Failure("for loop condition must evaluate on boolean expressions"))
	   | While(e, b) -> 
       let c = check_expr e env in
		   if type_of_expr c = Lrx_Atom(Lrx_Bool) then 
       C_While(c, check_block b ret_type env (in_loop + 1))
		   else raise(Failure("while loop must evaluate on boolean expression"))
    | Continue ->
       if in_loop = 0 then raise (Failure "continue statement not within for or while loop")
       else C_Continue
    | Break ->
       if in_loop = 0 then raise (Failure "break statement not within for or while loop")
       else C_Break
*)

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

(*let verify_id id*)


let verify_binop l r op =
	(* match op with
		Add ->  *)
	type_of_expr(l)            (* type of l and type of r match*)
								(* operator is appropriate *)

 
(*let verify_assign id *)
let rec verify_expr expr env =
	let () = print_endline ("verifying expr: " ^ string_of_expr expr) in
	match expr with                                         (* expr evaluates to *)
		  Bool_Lit(b)     -> D_Bool_Lit(b,Bool_Type)        (* D_Bool_Lit *)
		| Int_Lit(i)      -> D_Int_Lit(i, Int_Type)		    (* D_Int_Lit *)
		| String_Lit(s)   -> D_String_Lit(s, String_Type)   (* D_String_Lit*)
		| Frac_Lit(n,d)   -> D_Frac_Lit(n, d, Frac_Type)    (* D_Frac_Lit *)
		| Id(s)           -> D_Id(s, String_Type)           (* D_Id_Lit *)
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
     		D_Tuple(ve1, ve2, PD_Type)  (* Come back and fix tuples *)
     	| Access(ar, i) -> (* Not implemented yet *)
     		(* Check that ar is in symtab, ar can only be of type chord? *) 
     		(* Check that i is a lit or id of type int *)
     		D_Access(ar, verify_expr (*change to verify_int_expr *) i env, Chord_Type)
     	| Noexpr -> D_Noexpr


and verify_array arr env = 
	match arr with
	[] -> ([], Null_Type) (* Empty *)
	| head :: tail ->
		let verified_head = verify_expr head env in
		let head_type = type_of_expr verified_head in
		(* Verify that other elements are valid expressions with consistent types *)
		(* Decision time: Only use pd_type ? *)
			let rec verify_list_and_type l t e =       
				match l with
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

(*
and get_type_of_rhs_assign = function
	  Bool_Lit(b)     -> D_Bool_Lit(b,Bool_Type)        (* D_Bool_Lit *)
		| Int_Lit(i)      -> D_Int_Lit(i, Int_Type)		    (* D_Int_Lit *)
		| String_Lit(s)   -> D_String_Lit(s, String_Type)   (* D_String_Lit*)
		| Frac_Lit(n,d)   -> D_Frac_Lit(n, d, Frac_Type)    (* D_Frac_Lit *)
		| Id(s)           -> D_Id(s, String_Type)           (* D_Id_Lit *)
		(*| Array_Lit of expr list  *)
	    | Binop(l, op, r) -> 
	    	let vl = verify_expr l env in
	    	let vr = verify_expr r env in
	    	let vl_type = verify_binop vl vr op env in
	    	D_Binop(vl, op, vr, vl_type)                      (* D_Binop *)
     	| Unop(e, uop) -> 
     		let ve = verify_expr e env in
     		let ve_type = verify_unop_and_get_type ve uop in
     		D_Unop(ve, uop, ve_type)        
     	| _ -> raise (Failure "right hand side of assignment expression is invalid") *) 


let verify_id_is_type (id:string) vt env = (* Add support for assigning compatible types *)
	let decl = Symtab.symtab_find id env in 
	let vdecl = match decl with (* check that id refers to a variable *)
	Var_Decl(v) -> v
	| _ -> raise(Failure (id ^ " is not a variable")) in
	let (_, idtype, _) = vdecl in
	if idtype = vt then id (* id is of type vt *)
	else  raise (Failure("Expression of type " ^ string_of_prim_type vt ^ " assigned to variable " ^ id ^ " of type " ^ string_of_prim_type idtype))

let rec verify_stmt stmt ret_type env =
	match stmt with
	Return(e) ->
		let verified_expr = verify_expr e env in
		if ret_type = type_of_expr verified_expr then D_Return(verified_expr) 
	    else raise(Failure "return type does not match") 
	| Expr(e) -> 
		let verified_expr = verify_expr e env in
		D_Expr(verified_expr)
	| Assign(id, e) -> 
    	let ve = verify_expr e env in
    	let vt = type_of_expr ve in
    	let vid = verify_id_is_type id vt env in 
    	D_Assign(vid, ve, vt) 
    | Block(b) -> 
    	let verified_block = verify_block b ret_type (fst env, b.block_id) in
    	D_CodeBlock(verified_block)
    | If(e, b1, b2) ->
    	let verified_expr = verify_expr e env in
    	let () = print_endline (string_of_prim_type (type_of_expr verified_expr)) in
    	if (type_of_expr verified_expr) = Bool_Type then
	    	let vb1 = verify_block b1 ret_type (fst env, b1.block_id) in
	    	let vb2 = verify_block b2 ret_type (fst env, b2.block_id) in
	    	D_If(verified_expr, D_CodeBlock(vb1), D_CodeBlock(vb2))
    	else raise(Failure("Condition in if statement must be a boolean expression."))

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

(*
-       let verified_formals = map_to_list_env verify_var func.formals env in
-       let return = verify_block func.fblock env in
-       let *)
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
(*let verify_binop l op r = 
	let *)





(*	match var with
		  Func_Decl(f) -> raise(Failure "symbol is function not a variable")
		| Var_Decl(v) -> gvar 

	this may or may not happen ever ? *)



let verify_semantics program env = 
	let (gvar_list, func_list) = program in 
	let verified_gvar_list = map_to_list_env verify_var gvar_list env in 
	let () = Printf.printf "after verifying gvars \n" in
	let verified_func_list = map_to_list_env verify_func func_list env in
	let () = Printf.printf "after verifying functions \n" in
		(verified_func_list, verified_gvar_list)
