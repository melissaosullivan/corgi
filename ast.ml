type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type types = 
    Bool_Type
  | Int_Type
  | Pitch_Type
  | String_Type
  | Frac_Type
  | Rhythm_Type
  | Duration_Type
  | Chord_Type
  | Track_Type
  | Composition_Type

type expr =
    Bool_Lit of bool 
  | Int_Lit of int 
  | String_Lit of string
  | Frac_Lit of int * int 
  | Id of string
  | Array_Lit of expr list
  | Binop of expr * op * expr
  | Create of types * string * expr 
  | Assign of string * expr
  | Call of string * expr list
  | Tuple of expr * expr
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * (expr * stmt) list * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type variable = {
  vname : string;
  vtype : types;
  vexpr : expr;
}

type parameter = {
  pname : string;
  ptype : types;
}

type func = {
    ret_type : types;
    fname : string;
    formals : parameter list;
    locals : variable list;
    body : stmt list;
  }
  
type decl = 
    Func_Decl of scope_func_decl
  | Var_Decl of scope_var_decl

type program = variable list * func list


let string_of_types = function
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

let string_of_binop = function
    Add -> "+" 
  | Sub -> "-" 
  | Mult -> "*" 
  | Div -> "/" 
  | Equal -> "==" 
  | Neq -> "!="
  | Less -> "<" 
  | Leq -> "<=" 
  | Greater -> ">" 
  | Geq -> ">="


let rec string_of_expr = function
  Bool_Lit(b) -> string_of_bool b
  | Int_Lit(i) -> string_of_int i
  | String_Lit(s) -> s
  | Frac_Lit(n, d) -> "$" ^ string_of_int n ^ "/" ^ string_of_int d ^ "$"
  | Array_Lit(e) -> String.concat ", " (List.map string_of_expr e) 
  | Id(s) -> s 
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      string_of_binop o ^ " " ^
      string_of_expr e2
  |  Create(t, id, rhs) -> string_of_types t ^ " " ^ id ^ " = " ^ string_of_expr rhs 
  |  Assign(id, rhs) -> id ^ " = " ^
    string_of_expr rhs
  | Tuple(e1, e2) -> "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Call(f, e) -> 
    f ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | Noexpr -> ""

(* let string_of_elif (expr, stmt) = 
  "elif (" ^ string_of_expr expr ^ ") { \n" ^
  string_of_stmt stmt ^ "\n}\n" 
 
let string_of_elseifs elseifs = 
  String.concat "" (List.map (function(expr, stmt) -> string_of_expr expr ^ string_of_stmt stmt) elseifs) ^ "\n"  *)

let rec string_of_stmt = function
  Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, ei, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s ^
    String.concat "" (List.map (function(expr, stmt) -> 
      "elif (" ^ string_of_expr expr ^ ") {\n" ^ string_of_stmt stmt ^ "\n}\n") ei) ^ "\n" 
  | If(e, s1, ei, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
    string_of_stmt s1 ^
    String.concat "" (List.map (function(expr, stmt) -> 
      "elif (" ^ string_of_expr expr ^ ") {\n" ^ string_of_stmt stmt ^ "\n}\n") ei) ^ "\n" ^
    "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl vdecl = string_of_types vdecl.vtype ^ " " ^ vdecl.vname ^ 
              " = " ^ string_of_expr vdecl.vexpr ^ ";\n"  
let string_of_pdecl pdecl = string_of_types pdecl.ptype ^ " " ^ pdecl.pname 

let string_of_fdecl fdecl =
  string_of_types fdecl.ret_type ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_pdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl (List.rev vars) ) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs) ) ^ "\n"