type token =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EOF
  | LITERAL of (int)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
