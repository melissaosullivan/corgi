{ open Parser }

let char_lit = ['a'-'z' 'A'-'Z']?
let string_lit = ['a'-'z' 'A'-'Z']*
let frac_lit = '$'(int_lit'/'int_lit | int_lit)'$'
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let rhythm = '['((int_lit',')*int_lit)?']'
let pd_tuple = '('int_lit','duration')'
let chord = '['((pd_tuple',')*pd_tuple)?']'
let track = '['((chord',')*chord)?']'
let composition = '['((track',')*track)?']'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }                   (* Punctuation *)
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'	   { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| '$'      { DOLLAR }

| "if"     { IF }                       (* Keywords *)
| "elif"   { ELIF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

| "int"    { DATATYPE("int") }                      (* Types *)
| "char"   { DATATYPE("char") }
| "string" { DATATYPE("string") }
| "frac"   { DATATYPE("frac") }
| "pitch"  { DATATYPE("pitch") }
| "duration" { DATATYPE("duration") }
| "chord"    { DATATYPE("chord") }
| "track"    { DATATYPE("track") }
| "composition" { DATATYPE("composition") }
| "rhythm" { DATATYPE("rhythm") }


| int_lit as lxm { INT_LIT(int_of_string lxm) }
| string_lit as lxm { STRING_LIT(lxm) }
| frac_lit as lxm { FRAC_LIT(lxm) }
| id as lxm { ID(lxm) }
| rhythm as lxm { RHYTHM(lxm) }
| chord as lxm { CHORD(lxm) }
| track as lxm { TRACK(lxm) }
| composition as lxm { COMPOSITION(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }