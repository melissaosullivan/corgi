type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type prim_type =
    Bool_Type
  | Int_Type
  | Pitch_Type
  | String_Type
  | Frac_Type
  | Rhythm_Type
  | Duration_Type
  | PD_Type
  | Chord_Type
  | Track_Type
  | Composition_Type
  | Null_Type
  
type types = 
  Corgi_Prim of prim_type

type var = string * bool * prim_type

type expr =
    Bool_Lit of bool 
  | Int_Lit of int 
  | String_Lit of string
  | Frac_Lit of expr * expr (* int * int or Id's of type int *)
  | Id of string
  | Array_Lit of expr list
  | Binop of expr * op * expr
  | Unop of expr * uop
  (* | Create of types * string * expr  *)
  | Call of string * expr list
  | Access of string * expr 
  | Tuple of expr * expr
  | Noexpr

type stmt =
    Block of block
  | Expr of expr
  | Assign of string * expr
  | Array_Assign of string * expr * expr
  | Return of expr
  | If of expr * block * block
  | For of stmt * stmt * stmt * block
  | While of expr * block

and block = {
    locals : var list;
    statements: stmt list;
    block_id: int;
}

(*type variable = {
  vname : string;
  vtype : types;
 vexpr : expr; 
}*)

type parameter = {
  pname : string;
  ptype : prim_type;
}

type func = {
    ret_type : prim_type;
    fname : string;
    formals : var list;
    fblock : block;
  }
  
type program = var list * func list

(* Added from Lorax *)

type scope_var_decl = string * bool * prim_type * int

type scope_func_decl = string * prim_type * prim_type list * int

type decl = 
    Func_Decl of scope_func_decl
  | Var_Decl of scope_var_decl


let string_of_prim_type = function
    Bool_Type -> "bool"
  | Int_Type -> "int"
  | Pitch_Type -> "pitch"
  | String_Type -> "string"
  | Frac_Type -> "frac"
  | Rhythm_Type -> "rhythm"
  | Duration_Type -> "duration"
  | Chord_Type -> "chord"
  | Track_Type -> "track"
  | Composition_Type -> "composition"
  | PD_Type -> "(pitch, duration)"
  | Null_Type -> "null"

let string_of_types = function
  Corgi_Prim(t) -> string_of_prim_type t

let string_of_unop = function
    Neg -> "-"
  | Not -> "!"

let string_of_binop = function
    Add -> "+" 
  | Sub -> "-" 
  | Mult -> "*" 
  | Div -> "/" 
  | Mod -> "%"
  | Equal -> "==" 
  | Neq -> "!="
  | Less -> "<" 
  | Leq -> "<=" 
  | Greater -> ">" 
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"


let rec string_of_expr = function
  Bool_Lit(b) -> string_of_bool b
  | Int_Lit(i) -> string_of_int i
  | String_Lit(s) -> s
  | Frac_Lit(n, d) -> "$" ^ string_of_expr n ^ "/" ^ string_of_expr d ^ "$"
  | Array_Lit(e) -> String.concat ", " (List.map string_of_expr e) 
  | Id(s) -> s
  | Access(ar, i) -> ar ^ "@" ^ string_of_expr i 
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      string_of_binop o ^ " " ^
      string_of_expr e2
  | Unop(e, o) -> 
      (match o with 
          Neg -> "-" ^ string_of_expr e
        | Not -> "!" ^ string_of_expr e)
  (* |  Create(t, id, rhs) -> string_of_types t ^ " " ^ id ^ " = " ^ string_of_expr rhs  *)
  | Tuple(e1, e2) -> "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Call(f, e) -> 
    f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | Noexpr -> ""

(* let string_of_elif (expr, stmt) = 
  "elif (" ^ string_of_expr expr ^ ") { \n" ^
  string_of_stmt stmt ^ "\n}\n" 
 
let string_of_elseifs elseifs = 
  String.concat "" (List.map (function(expr, stmt) -> string_of_expr expr ^ string_of_stmt stmt) elseifs) ^ "\n"  *)

(*
let string_of_vdecl vdecl = string_of_types vdecl.vtype ^ " " ^ vdecl.vname ^ 
              " = " ^ string_of_expr vdecl.vexpr ^ ";\n"  
*)
let string_of_array_bool a = 
  if a then "[] " else "" 

let string_of_vdecl v =
  let (n, a, t) = v in 
    string_of_prim_type t ^ " " ^ string_of_array_bool a ^ n

let rec string_of_stmt = function
    Block(b) -> string_of_block b
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Assign(id, rhs) -> id ^ " = " ^ string_of_expr rhs ^ "; \n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, b1, b2) -> 
    (match b2.statements with
        [] -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_block b1
      | _  -> "if (" ^ string_of_expr e ^ ")\n" ^
              string_of_block b1 ^ "else\n" ^ string_of_block b2)
  | For(a1, c, a2, b) ->
      "for (" ^ string_of_stmt a1 ^ string_of_stmt c ^
      string_of_stmt a2  ^ ") " ^ string_of_block b
  | While(e, b) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_block b

and string_of_block (b:block) =
  "{\n" ^
  String.concat ";\n" (List.map string_of_vdecl b.locals) ^ (if (List.length b.locals) > 0 then ";\n" else "") ^
  String.concat "" (List.map string_of_stmt b.statements) ^
  "}\n"

let string_of_fdecl fdecl =
  (string_of_prim_type fdecl.ret_type) ^ " " ^ 
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n" ^
  string_of_block fdecl.fblock

(* need to rewrite *)
let string_of_decl = function
	  Var_Decl(n, a, t, id)     -> string_of_vdecl (n, a, t)
	| Func_Decl(n, t, f, id) -> 
	  (string_of_prim_type t) ^ " " ^ 
	  n ^ "(" ^ 
	  String.concat ", " (List.map string_of_prim_type f) ^ ")"

(* ___________________________________ *)

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl (List.rev vars) ) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs) ) ^ "\n"