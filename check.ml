open Ast


let verify_gvar gvar env = 
	let var = Symtab.symtab_find (fst gvar) env in 
	let id = Symtab.symtab_get_id (fst gvar) env in
	gvar

let verify_gfunc gfunc env =
	gfunc

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
	let (gvar_list, gfunc_list) = program in 
	let verified_gvar_list = map_to_list_env verify_gvar gvar_list env in  (*we are here*)
	let verified_gfunc_list = map_to_list_env verify_gfunc gfunc_list env in
		(verified_gfunc_list, verified_gvar_list)
