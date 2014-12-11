open Ast

(* 
 * SymMap contains string : Ast.decl pairs representing
 * identifiername_scopenumber : decl
 *)
module SymTable = Map.Make(String)

let scope_parents = Array.make 1000 0

let string_of_decl = function
      Var_Decl(n, t, id)     -> string_of_vdecl (n, t)
    | Func_Decl(n, t, f, id) -> 
        (string_of_types t) ^ " " ^ n ^ "(" ^ 
        String.concat ", " (List.map string_of_types f) ^ ")"

let string_of_symtab env =
    let symlist = SymTable.fold
        (fun s t prefix -> (string_of_decl t) :: prefix) (fst env) [] in
    let sorted = List.sort Pervasives.compare symlist in
    String.concat "\n" sorted

let rec symtab_add_decl (name:string) (decl:decl) env =
    let (table, scope) = env in (* get current scope and environment *)
    let to_find = name ^ "_" ^ (string_of_int scope) in
    if SymTable.mem to_find table then raise(Failure("symbol " ^ name ^ " declared twice in same scope"))
    else ((SymTable.add to_find decl table), scope)

(* 
 * recursively add list of variables to the symbol table along with the scope of
 * the block in which they were declared
 *) 
let rec symtab_add_vars (vars:var list) env =
    match vars with
      [] -> env
    | (vname, vtype) :: tail -> let env = symtab_add_decl vname (Var_Decl(vname, vtype, snd env)) env in (* name, type, scope *)
        symtab_add_vars tail env 

(* add declarations inside statements to the symbol table *)
let rec symtab_add_stmts (stmts:stmt list) env =
    match stmts with
      [] -> env (* block contains no statements *)
    | head :: tail -> let env = (match head with
        Block(s) -> symtab_add_block s env (* statement is an arbitrary block *)
        | For(e1, e2, e3, s) -> symtab_add_block s env (* add the for's block to the record *)
        | While(e, s) -> symtab_add_block s env (* same deal as for *)
        | If(e, s1, s2) -> let env = symtab_add_block s1 env in symtab_add_block s2 env (* add both of if's blocks separately *)
        | _ -> env) in symtab_add_stmts tail env (* return, continue, break, etc *)

and symtab_add_block (b:block) env =
    let (table, scope) = env in 
    let env = symtab_add_vars b.locals (table, b.block_id) in 
    let env = symtab_add_stmts b.statements env in 
    scope_parents.(b.block_id) <- scope; (* parent is block_id - 1 *)
    ((fst env), scope) (* return what we've made *)

and symtab_add_func (f:func) env =
    let scope = snd env in
    let args = List.map snd f.formals in (* gets name of every formal *)
    let env = symtab_add_decl f.fname (Func_Decl(f.fname, f.ret_type, args, scope)) env in (* add current function to table *)
    let env = symtab_add_vars f.formals ((fst env), f.fblock.block_id) in (* add vars to the next scope in. scope_id is ahead by one *)
    symtab_add_block f.fblock ((fst env), scope) (* add body to symtable given current environment and scope *) 

(* add list of functions to the symbol table *)
and symtab_add_funcs (funcs:func list) env =
    match funcs with
       [] -> env
     | head :: tail -> let env = symtab_add_func head env in 
       symtab_add_funcs tail env

let add_builtins env =
    symtab_add_decl "print" (Func_Decl("print", Corgi_Prim(Int_Type), [], 0)) env


let symtab_of_program (p:Ast.program) =
    let env = add_builtins (SymTable.empty, 0) in
    let env = symtab_add_vars (fst p) env in
    symtab_add_funcs (snd p) env

(**)