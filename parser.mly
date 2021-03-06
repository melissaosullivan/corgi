%{ open Ast 

let scope_id = ref 1

let inc_block_id (u:unit) =
    let x = scope_id.contents in
    scope_id := x + 1; x
%}

%token SEMI LPAREN RPAREN LBRACE LBRACKET RBRACE RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN ARRAY_ASSIGN AT
%token EQ NEQ LT LEQ GT GEQ DOLLAR
%token RETURN IF ELIF ELSE FOR WHILE 
/* %token BREAK CONTINUE */
%token TRUE FALSE NULL
%token AND OR NOT

%token BOOL INT
%token STRING RHYTHM CHORD TRACK COMPOSITION
%token FRAC PITCH DURATION

%token <string> ID
%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <string> FRAC_LIT
%token <string> STRING_LIT
%token <string> ARRAY_LIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE ELIF
%right ASSIGN
%right DOLLAR
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left NEG NOT

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

prim_type:
    BOOL {Bool_Type}
  | INT {Int_Type}
  | STRING {String_Type}
  | FRAC {Frac_Type}
  | PITCH {Pitch_Type}
  | DURATION {Duration_Type}
  | RHYTHM {Rhythm_Type}
  | CHORD {Chord_Type}
  | TRACK {Track_Type}
  | COMPOSITION {Composition_Type}

fdecl:
   prim_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { ret_type = $1;
         fname = $2;
	       formals = $4;
	       fblock = {locals = List.rev $7; statements = List.rev $8; block_id = inc_block_id ()} } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    fvdecl                { [$1] }
  | formal_list COMMA fvdecl { $3 :: $1 }

/* Suggested to make vdecls a statement */
vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/* Was here before
vdecl:
   prim_type ID { ({vname = $2; vtype = $1; vexpr = Noexpr}) }
   | prim_type ID ASSIGN expr { {vname = $2; vtype = $1; vexpr = $4}}
*/
vdecl:
  prim_type ID SEMI{ ($2, false, $1) }
  | prim_type LBRACKET RBRACKET ID SEMI { ($4, true, $1)}

fvdecl:
  prim_type ID { ($2, false, $1) }
  | prim_type LBRACKET RBRACKET ID { ($4, true, $1) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    block { Block($1) }
  | expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | IF LPAREN expr RPAREN block %prec NOELSE { If($3, $5, {locals = []; statements = []; block_id = inc_block_id ()}) }
  | IF LPAREN expr RPAREN block ELSE block { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN block { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN block { While($3, $5) }
  | ID ASSIGN expr SEMI{ Assign($1, $3) }
  | ID ASSIGN AT int_expr expr SEMI { Array_Assign($1, $5, $4)}
  /* array_variable = @ 2 4; */

block:
  LBRACE stmt_list RBRACE { {locals = []; statements = List.rev $2; block_id = inc_block_id ()} }

/*
elifs:
  { [] }
  | elifs ELIF LPAREN expr RPAREN stmt { ($4, $6) :: $1 }
*/

expr_opt:
    /* nothing */ { Expr(Noexpr) }
  | expr          { Expr($1) }
  | ID ASSIGN expr{ Assign($1, $3) }

expr:
  literal {$1}
  | DOLLAR int_expr DIVIDE int_expr DOLLAR {Frac_Lit($2, $4)}
  | LBRACKET expr_list RBRACKET { Array_Lit($2) }
  | LPAREN expr COMMA expr RPAREN { Tuple($2, $4)}
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod, $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr             { Binop($1, And, $3) }
  | expr OR     expr             { Binop($1, Or, $3) }
  | MINUS expr %prec NEG         { Unop($2, Neg) }
  | NOT expr                     { Unop($2, Not) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | ID AT int_expr{ Access($1, $3) }
  | LPAREN expr RPAREN { $2 }

int_expr:
  ID               { Id($1) }
  | INT_LIT        { Int_Lit($1) } 

expr_list:
  /* nothing */ { [] }
  | expr { [$1] }
  | expr COMMA expr_list {$1 :: $3}

literal:
    BOOL_LIT   { Bool_Lit($1) }
  | INT_LIT    { Int_Lit($1) }
  | STRING_LIT { String_Lit($1) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
