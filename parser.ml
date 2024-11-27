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
  | QUIT
  | LPAREN
  | RPAREN
  | LCURLY
  | RCURLY
  | DOT
  | COMMA
  | EQ
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Lambda;;
# 38 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* IN *);
  269 (* CONCAT *);
  270 (* BOOL *);
  271 (* NAT *);
  272 (* STRING *);
  273 (* QUIT *);
  274 (* LPAREN *);
  275 (* RPAREN *);
  276 (* LCURLY *);
  277 (* RCURLY *);
  278 (* DOT *);
  279 (* COMMA *);
  280 (* EQ *);
  281 (* COLON *);
  282 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  283 (* INTV *);
  284 (* IDV *);
  285 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
\006\000\006\000\007\000\007\000\004\000\004\000\008\000\008\000\
\008\000\008\000\008\000\009\000\009\000\000\000"

let yylen = "\002\000\
\004\000\002\000\002\000\001\000\006\000\006\000\006\000\008\000\
\001\000\002\000\002\000\002\000\003\000\002\000\003\000\003\000\
\003\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\000\000\003\000\005\000\001\000\003\000\003\000\001\000\
\001\000\001\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\019\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\000\000\023\000\038\000\000\000\000\000\009\000\000\000\021\000\
\000\000\010\000\011\000\012\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\
\014\000\000\000\000\000\000\000\000\000\013\000\016\000\000\000\
\000\000\017\000\018\000\000\000\015\000\032\000\033\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\000\000\035\000\006\000\030\000\
\005\000\007\000\000\000\000\000\028\000\037\000\000\000\008\000"

let yydgoto = "\002\000\
\019\000\035\000\021\000\068\000\022\000\036\000\037\000\060\000\
\069\000"

let yysindex = "\005\000\
\007\255\000\000\230\254\000\000\000\000\047\255\050\255\050\255\
\050\255\240\254\249\254\050\255\022\000\047\255\080\255\000\000\
\255\254\000\000\000\000\026\000\114\255\000\000\003\255\000\000\
\024\255\000\000\000\000\000\000\006\255\013\255\050\255\000\000\
\012\255\008\255\016\255\019\255\020\255\047\255\000\000\017\255\
\000\000\115\255\047\255\047\255\115\255\000\000\000\000\047\255\
\047\255\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\115\255\115\255\044\255\054\255\079\255\034\255\068\255\073\255\
\000\000\000\000\078\255\076\255\081\255\047\255\115\255\047\255\
\047\255\047\255\072\255\000\000\115\255\000\000\000\000\000\000\
\000\000\000\000\091\255\008\255\000\000\000\000\047\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\083\255\000\000\
\001\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\092\255\084\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\083\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\255\000\000\000\000\000\000\085\255\
\000\000\000\000\000\000\090\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\218\255\116\000\069\000\047\000\000\000\
\049\000"

let yytablesize = 286
let yytable = "\020\000\
\021\000\023\000\004\000\059\000\025\000\001\000\063\000\003\000\
\004\000\005\000\006\000\029\000\033\000\007\000\008\000\009\000\
\010\000\011\000\067\000\012\000\030\000\032\000\038\000\013\000\
\014\000\039\000\015\000\042\000\043\000\044\000\047\000\048\000\
\080\000\016\000\017\000\018\000\052\000\045\000\049\000\050\000\
\051\000\061\000\062\000\053\000\066\000\073\000\064\000\003\000\
\004\000\005\000\006\000\004\000\005\000\007\000\008\000\009\000\
\010\000\011\000\029\000\012\000\029\000\029\000\029\000\029\000\
\014\000\070\000\015\000\014\000\079\000\015\000\081\000\082\000\
\083\000\016\000\024\000\018\000\016\000\024\000\018\000\071\000\
\003\000\004\000\005\000\006\000\072\000\088\000\007\000\008\000\
\009\000\010\000\011\000\074\000\012\000\023\000\023\000\075\000\
\076\000\014\000\077\000\015\000\084\000\078\000\087\000\026\000\
\024\000\027\000\016\000\024\000\034\000\023\000\036\000\023\000\
\023\000\023\000\023\000\004\000\005\000\065\000\023\000\023\000\
\023\000\085\000\026\000\027\000\028\000\086\000\000\000\031\000\
\054\000\055\000\056\000\014\000\057\000\015\000\058\000\040\000\
\041\000\000\000\000\000\000\000\016\000\024\000\018\000\000\000\
\000\000\000\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\021\000\000\000\000\000\000\000\004\000\
\004\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\000\000\000\000\021\000\000\000\021\000\004\000\021\000\004\000\
\000\000\004\000\000\000\021\000\021\000\021\000"

let yycheck = "\001\000\
\000\000\028\001\000\000\042\000\006\000\001\000\045\000\001\001\
\002\001\003\001\004\001\028\001\014\000\007\001\008\001\009\001\
\010\001\011\001\057\000\013\001\028\001\000\000\024\001\017\001\
\018\001\000\000\020\001\025\001\005\001\024\001\019\001\024\001\
\071\000\027\001\028\001\029\001\038\000\025\001\023\001\021\001\
\021\001\043\000\044\000\027\001\000\000\012\001\048\000\001\001\
\002\001\003\001\004\001\002\001\003\001\007\001\008\001\009\001\
\010\001\011\001\019\001\013\001\021\001\022\001\023\001\024\001\
\018\001\022\001\020\001\018\001\070\000\020\001\072\000\073\000\
\074\000\027\001\028\001\029\001\027\001\028\001\029\001\026\001\
\001\001\002\001\003\001\004\001\006\001\087\000\007\001\008\001\
\009\001\010\001\011\001\024\001\013\001\002\001\003\001\023\001\
\019\001\018\001\023\001\020\001\029\001\021\001\012\001\021\001\
\021\001\021\001\027\001\028\001\029\001\018\001\021\001\020\001\
\021\001\022\001\023\001\002\001\003\001\049\000\027\001\028\001\
\029\001\075\000\007\000\008\000\009\000\077\000\255\255\012\000\
\014\001\015\001\016\001\018\001\018\001\020\001\020\001\022\001\
\021\000\255\255\255\255\255\255\027\001\028\001\029\001\255\255\
\255\255\255\255\031\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\255\255\
\255\255\255\255\018\001\255\255\020\001\019\001\022\001\021\001\
\255\255\023\001\255\255\027\001\028\001\029\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  CONCAT\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  QUIT\000\
  LPAREN\000\
  RPAREN\000\
  LCURLY\000\
  RCURLY\000\
  DOT\000\
  COMMA\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 45 "parser.mly"
      ( Bind (_1, _3) )
# 257 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 47 "parser.mly"
      ( Eval _1 )
# 264 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
      ( Quit )
# 270 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 54 "parser.mly"
      ( _1 )
# 277 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 286 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 58 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 295 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 60 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 304 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 62 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 314 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( _1 )
# 321 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( TmSucc _2 )
# 328 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmPred _2 )
# 335 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( TmIsZero _2 )
# 342 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmConcat (_2, _3) )
# 350 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmApp (_1, _2) )
# 358 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 79 "parser.mly"
      ( TmProj (_1, string_of_int _3) )
# 366 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 83 "parser.mly"
      ( _2 )
# 373 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tupleList) in
    Obj.repr(
# 85 "parser.mly"
      ( TmTuple _2 )
# 380 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'recordList) in
    Obj.repr(
# 87 "parser.mly"
      ( TmRecord _2)
# 387 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
      ( TmTrue )
# 393 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
      ( TmFalse )
# 399 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
      ( TmVar _1 )
# 406 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 416 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
        ( TmString _1 )
# 423 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 104 "parser.mly"
        ( [_1] )
# 430 "parser.ml"
               : 'tupleList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleList) in
    Obj.repr(
# 106 "parser.mly"
        ( _1 :: _3 )
# 438 "parser.ml"
               : 'tupleList))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
        ( [] )
# 444 "parser.ml"
               : 'tupleList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 112 "parser.mly"
        ( [(_1, _3)] )
# 452 "parser.ml"
               : 'recordList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordList) in
    Obj.repr(
# 114 "parser.mly"
        ( (_1, _3) :: _5 )
# 461 "parser.ml"
               : 'recordList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 119 "parser.mly"
      ( _1 )
# 468 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 121 "parser.mly"
      ( TyArr (_1, _3) )
# 476 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 125 "parser.mly"
      ( _2 )
# 483 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
      ( TyBool )
# 489 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
      ( TyNat )
# 495 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
      ( TyString )
# 501 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tyList) in
    Obj.repr(
# 133 "parser.mly"
      ( TyTuple _2 )
# 508 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 137 "parser.mly"
      ( [_1] )
# 515 "parser.ml"
               : 'tyList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tyList) in
    Obj.repr(
# 139 "parser.mly"
      ( _1 :: _3 )
# 523 "parser.ml"
               : 'tyList))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.command)
