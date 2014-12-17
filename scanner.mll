{ open Parser 
  exception ParsingError of string }


(* regular definitions *)

let char_lit = ['a'-'z' 'A'-'Z']?
let int_lit = ['0'-'9']+
let string_lit = '\"' ([^'\"']* as lxm) '\"'
(*let frac_lit = '$'(int_lit '/' int_lit | int_lit)'$'*)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
(*let rhythm = '['((int_lit ',' )* int_lit)? ']'
let pd_tuple = '(' int_lit ',' frac_lit ')'
let chord = '[' ((pd_tuple ',' )*pd_tuple)? ']'
let track = '[' ((chord ',' )*chord)? ']'
let composition = '[' ((track ',' )*track)? ']'*)
(*let array_content = (char_lit | int_lit | string_lit | frac_lit | id
let array_lit = '['((array_content ',' )* array_content)? ']'*)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "//"     { comment lexbuf }			(* Single-line comments *)
| "/*"     { comments lexbuf }          (* Multi-line comments *)
| '('      { LPAREN }                   (* Punctuation *)
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['	   { LBRACKET }
| ']'	   { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=>"	   { ARRAY_ASSIGN }
| "!"      { NOT } 
| "&&"     { AND }
| "||"     { OR } 
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| '$'      { DOLLAR }
| '@'      { AT }
| "%"      { MOD } 

| "if"     { IF }                       (* Keywords *)
| "elif"   { ELIF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "null"   { NULL }


| "int"    { INT }                      (* Types *)
(*| "char"   { CHAR }*)
| "bool"   { BOOL }
| "string" { STRING }
| "frac"   { FRAC }
| "pitch"  { PITCH }
| "rhythm" { RHYTHM }
| "duration" { DURATION }
| "chord"    { CHORD }
| "track"    { TRACK }
| "composition" { COMPOSITION }


| ("true"|"false") as lit { BOOL_LIT(bool_of_string lit) }
| int_lit as lit { INT_LIT(int_of_string lit) }
| '\"' ([^'\"']* as lit) '\"' { STRING_LIT(lit) }
(*| frac_lit as lit { FRAC_LIT(lit) }*)
| id as lit { ID(lit) }
(*| '[' ((array_content ',' )* array_content)? as lit ']' {ARRAY_LIT(lit)}*)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "\n" { token lexbuf }
| eof  { EOF }
| _ { comment lexbuf }

and comments = parse
  "*/" { token lexbuf }
| eof { raise (ParsingError("unterminated comment"))}
| _    { comments lexbuf }
