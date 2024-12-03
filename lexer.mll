{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse 
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA } 
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "case"      { CASE }
  | "of"        { OF }
  | "as"        { AS }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "isnil"     { ISNIL }
  | "quit"      { QUIT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LCURLY }
  | '}'         { RCURLY }
  | '['         { LBRACKET }
  | ']'         { RBRACKET }
  | '.'         { DOT }
  | ','         { COMMA }
  | '='         { EQ }
  | ':'         { COLON }
  | '>'         { GT }
  | '<'         { LT }
  | '|'         { OR }
  | "=>"        { DARROW }  
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z'] ['a'-'z' '_' '0'-'9']* { IDT (Lexing.lexeme lexbuf) }
  | '"'[^ '"' ';' '\n']*'"' { 
                  let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s - 2)) 
                  }
  | eof         { EOF }
  | _           { raise Lexical_error }

  