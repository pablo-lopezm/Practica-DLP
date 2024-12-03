type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | CONCAT
  | BOOL
  | NAT
  | STRING
  | CASE
  | OF
  | AS
  | OR
  | NIL
  | CONS
  | HEAD
  | TAIL
  | ISNIL
  | QUIT
  | LPAREN
  | RPAREN
  | LCURLY
  | RCURLY
  | LBRACKET
  | RBRACKET
  | DOT
  | COMMA
  | EQ
  | COLON
  | GT
  | LT
  | DARROW
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)
  | IDT of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
