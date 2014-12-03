type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    Literal of int
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
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
  vtype : string;
}

type parameter = {
  pname : string;
  ptype : string;
}

type func_decl = {
    rtype : string;
    fname : string;
    formals : parameter list;
    locals : variable list;
    body : stmt list;
  }

type program = variable list * func_decl list