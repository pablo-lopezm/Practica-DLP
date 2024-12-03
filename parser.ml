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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Lambda;;
# 53 "parser.ml"
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
  273 (* CASE *);
  274 (* OF *);
  275 (* AS *);
  276 (* OR *);
  277 (* NIL *);
  278 (* CONS *);
  279 (* HEAD *);
  280 (* TAIL *);
  281 (* ISNIL *);
  282 (* QUIT *);
  283 (* LPAREN *);
  284 (* RPAREN *);
  285 (* LCURLY *);
  286 (* RCURLY *);
  287 (* LBRACKET *);
  288 (* RBRACKET *);
  289 (* DOT *);
  290 (* COMMA *);
  291 (* EQ *);
  292 (* COLON *);
  293 (* GT *);
  294 (* LT *);
  295 (* DARROW *);
  296 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  297 (* INTV *);
  298 (* IDV *);
  299 (* STRINGV *);
  300 (* IDT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\005\000\005\000\006\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\010\000\009\000\
\009\000\003\000\003\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\013\000\013\000\012\000\012\000\012\000\
\014\000\000\000"

let yylen = "\002\000\
\004\000\002\000\004\000\002\000\001\000\006\000\006\000\006\000\
\008\000\004\000\001\000\003\000\007\000\001\000\002\000\002\000\
\002\000\003\000\002\000\003\000\003\000\005\000\005\000\005\000\
\003\000\003\000\003\000\007\000\001\000\001\000\001\000\001\000\
\001\000\004\000\006\000\001\000\003\000\000\000\003\000\001\000\
\003\000\001\000\003\000\003\000\001\000\001\000\001\000\003\000\
\003\000\003\000\001\000\001\000\003\000\000\000\001\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\029\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\000\000\
\033\000\000\000\058\000\000\000\000\000\014\000\000\000\031\000\
\000\000\015\000\016\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\019\000\000\000\000\000\000\000\000\000\018\000\000\000\
\045\000\046\000\047\000\000\000\000\000\000\000\051\000\000\000\
\000\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
\026\000\027\000\000\000\000\000\000\000\000\000\021\000\020\000\
\000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\000\000\039\000\041\000\000\000\037\000\
\000\000\001\000\003\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\000\000\000\000\048\000\050\000\000\000\049\000\
\043\000\000\000\023\000\024\000\022\000\000\000\007\000\006\000\
\008\000\000\000\000\000\012\000\057\000\053\000\056\000\035\000\
\000\000\000\000\000\000\028\000\009\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\027\000\049\000\098\000\029\000\094\000\095\000\030\000\050\000\
\051\000\052\000\073\000\099\000\100\000\101\000"

let yysindex = "\006\000\
\140\255\000\000\222\254\000\000\000\000\184\255\161\000\161\000\
\161\000\234\254\235\254\161\000\184\255\247\254\248\254\251\254\
\252\254\254\254\032\000\184\255\229\255\249\254\000\000\255\254\
\000\000\002\255\000\000\033\000\138\000\000\000\000\255\000\000\
\033\255\000\000\000\000\000\000\005\255\006\255\161\000\026\255\
\001\255\001\255\001\255\001\255\001\255\000\000\018\255\012\255\
\022\255\027\255\028\255\030\255\032\255\184\255\001\255\000\000\
\217\254\000\000\001\255\184\255\184\255\001\255\000\000\034\255\
\000\000\000\000\000\000\001\255\085\255\023\255\000\000\036\255\
\031\255\038\255\044\255\048\255\055\255\000\000\184\255\184\255\
\000\000\000\000\042\255\184\255\088\000\089\000\000\000\000\000\
\058\255\086\255\084\255\062\255\056\255\000\000\082\255\075\255\
\068\255\071\255\076\255\083\255\087\255\088\255\000\000\001\255\
\161\000\161\000\161\000\161\000\000\000\000\000\012\255\000\000\
\089\255\000\000\000\000\184\255\184\255\184\255\184\255\093\255\
\034\255\000\000\001\255\001\255\000\000\000\000\023\255\000\000\
\000\000\161\000\000\000\000\000\000\000\100\255\000\000\000\000\
\000\000\108\255\080\255\000\000\000\000\000\000\000\000\000\000\
\001\255\184\255\094\255\000\000\000\000\095\255\110\000\138\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\103\255\000\000\000\000\024\000\
\000\000\000\000\000\000\000\000\063\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\052\255\
\105\255\000\000\000\000\106\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\107\255\101\255\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\103\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\077\000\000\000\
\000\000\109\255\000\000\000\000\232\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\244\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000"

let yygindex = "\000\000\
\000\000\255\255\007\000\245\255\003\000\000\000\002\000\072\000\
\066\000\000\000\000\000\190\255\034\000\000\000"

let yytablesize = 460
let yytable = "\028\000\
\042\000\087\000\088\000\102\000\033\000\055\000\001\000\031\000\
\034\000\035\000\036\000\040\000\055\000\039\000\065\000\066\000\
\067\000\054\000\047\000\037\000\038\000\041\000\042\000\031\000\
\054\000\043\000\044\000\068\000\045\000\069\000\058\000\046\000\
\056\000\054\000\053\000\059\000\055\000\060\000\070\000\061\000\
\063\000\062\000\013\000\064\000\071\000\078\000\079\000\072\000\
\074\000\075\000\076\000\077\000\085\000\031\000\031\000\080\000\
\081\000\082\000\090\000\091\000\143\000\086\000\005\000\083\000\
\097\000\089\000\084\000\103\000\092\000\105\000\104\000\093\000\
\031\000\031\000\096\000\106\000\011\000\109\000\031\000\107\000\
\031\000\031\000\113\000\111\000\031\000\031\000\108\000\114\000\
\115\000\031\000\116\000\117\000\031\000\031\000\031\000\118\000\
\119\000\120\000\065\000\066\000\067\000\121\000\122\000\123\000\
\124\000\125\000\130\000\131\000\132\000\133\000\129\000\068\000\
\126\000\069\000\135\000\136\000\137\000\138\000\145\000\146\000\
\127\000\147\000\070\000\140\000\128\000\134\000\097\000\139\000\
\071\000\141\000\150\000\144\000\038\000\151\000\040\000\036\000\
\054\000\054\000\052\000\152\000\003\000\004\000\005\000\006\000\
\149\000\110\000\007\000\008\000\009\000\010\000\011\000\148\000\
\012\000\058\000\112\000\000\000\013\000\142\000\000\000\000\000\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\023\000\024\000\025\000\026\000\
\003\000\004\000\005\000\006\000\000\000\000\000\007\000\008\000\
\009\000\010\000\011\000\000\000\012\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\014\000\015\000\016\000\017\000\
\018\000\000\000\020\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\000\000\000\000\
\023\000\032\000\025\000\000\000\000\000\003\000\004\000\005\000\
\006\000\000\000\000\000\007\000\008\000\009\000\010\000\011\000\
\000\000\012\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\014\000\015\000\016\000\017\000\018\000\000\000\020\000\
\000\000\021\000\042\000\042\000\000\000\042\000\042\000\000\000\
\000\000\000\000\022\000\000\000\042\000\023\000\048\000\025\000\
\000\000\000\000\042\000\000\000\042\000\042\000\042\000\000\000\
\000\000\031\000\031\000\042\000\042\000\042\000\042\000\000\000\
\042\000\042\000\042\000\042\000\000\000\042\000\042\000\000\000\
\000\000\042\000\042\000\042\000\031\000\031\000\000\000\013\000\
\013\000\000\000\031\000\000\000\031\000\000\000\013\000\000\000\
\031\000\000\000\000\000\000\000\013\000\031\000\013\000\000\000\
\031\000\031\000\031\000\005\000\005\000\000\000\013\000\000\000\
\013\000\000\000\005\000\000\000\013\000\000\000\000\000\013\000\
\005\000\011\000\011\000\000\000\000\000\000\000\000\000\000\000\
\011\000\000\000\005\000\000\000\005\000\000\000\011\000\000\000\
\005\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\011\000\000\000\011\000\000\000\000\000\000\000\011\000\004\000\
\005\000\011\000\000\000\000\000\007\000\008\000\009\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\015\000\016\000\017\000\018\000\000\000\
\020\000\000\000\021\000\004\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\023\000\032\000\
\025\000\000\000\000\000\000\000\000\000\000\000\014\000\015\000\
\000\000\000\000\004\000\005\000\020\000\000\000\021\000\000\000\
\000\000\000\000\057\000\000\000\000\000\000\000\000\000\022\000\
\000\000\000\000\023\000\032\000\025\000\014\000\015\000\000\000\
\000\000\000\000\000\000\020\000\000\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\000\
\000\000\023\000\032\000\025\000"

let yycheck = "\001\000\
\000\000\041\001\042\001\070\000\006\000\030\001\001\000\042\001\
\007\000\008\000\009\000\013\000\037\001\012\000\014\001\015\001\
\016\001\030\001\020\000\042\001\042\001\031\001\031\001\000\000\
\037\001\031\001\031\001\027\001\031\001\029\001\029\000\000\000\
\000\000\035\001\042\001\036\001\035\001\005\001\038\001\035\001\
\039\000\036\001\000\000\018\001\044\001\028\001\035\001\041\000\
\042\000\043\000\044\000\045\000\054\000\002\001\003\001\034\001\
\030\001\030\001\060\000\061\000\127\000\055\000\000\000\034\001\
\042\001\059\000\035\001\032\001\062\000\032\001\040\001\038\001\
\021\001\022\001\068\000\032\001\000\000\079\000\027\001\032\001\
\029\001\030\001\084\000\042\001\033\001\034\001\032\001\000\000\
\000\000\038\001\033\001\006\001\041\001\042\001\043\001\012\001\
\035\001\042\001\014\001\015\001\016\001\020\001\028\001\036\001\
\034\001\030\001\105\000\106\000\107\000\108\000\104\000\027\001\
\030\001\029\001\116\000\117\000\118\000\119\000\019\001\012\001\
\034\001\042\001\038\001\121\000\037\001\037\001\042\001\035\001\
\044\001\123\000\037\001\130\000\030\001\039\001\030\001\030\001\
\030\001\037\001\030\001\151\000\001\001\002\001\003\001\004\001\
\146\000\080\000\007\001\008\001\009\001\010\001\011\001\145\000\
\013\001\152\000\083\000\255\255\017\001\124\000\255\255\255\255\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\029\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\038\001\255\255\255\255\041\001\042\001\043\001\044\001\
\001\001\002\001\003\001\004\001\255\255\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\013\001\255\255\255\255\255\255\
\017\001\255\255\255\255\255\255\021\001\022\001\023\001\024\001\
\025\001\255\255\027\001\255\255\029\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\038\001\255\255\255\255\
\041\001\042\001\043\001\255\255\255\255\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\255\255\013\001\255\255\255\255\255\255\017\001\255\255\255\255\
\255\255\021\001\022\001\023\001\024\001\025\001\255\255\027\001\
\255\255\029\001\002\001\003\001\255\255\005\001\006\001\255\255\
\255\255\255\255\038\001\255\255\012\001\041\001\042\001\043\001\
\255\255\255\255\018\001\255\255\020\001\021\001\022\001\255\255\
\255\255\002\001\003\001\027\001\028\001\029\001\030\001\255\255\
\032\001\033\001\034\001\035\001\255\255\037\001\038\001\255\255\
\255\255\041\001\042\001\043\001\021\001\022\001\255\255\005\001\
\006\001\255\255\027\001\255\255\029\001\255\255\012\001\255\255\
\033\001\255\255\255\255\255\255\018\001\038\001\020\001\255\255\
\041\001\042\001\043\001\005\001\006\001\255\255\028\001\255\255\
\030\001\255\255\012\001\255\255\034\001\255\255\255\255\037\001\
\018\001\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\255\255\028\001\255\255\030\001\255\255\018\001\255\255\
\034\001\255\255\255\255\037\001\255\255\255\255\255\255\255\255\
\028\001\255\255\030\001\255\255\255\255\255\255\034\001\002\001\
\003\001\037\001\255\255\255\255\007\001\008\001\009\001\255\255\
\255\255\255\255\013\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\021\001\022\001\023\001\024\001\025\001\255\255\
\027\001\255\255\029\001\002\001\003\001\255\255\255\255\255\255\
\255\255\255\255\255\255\038\001\255\255\255\255\041\001\042\001\
\043\001\255\255\255\255\255\255\255\255\255\255\021\001\022\001\
\255\255\255\255\002\001\003\001\027\001\255\255\029\001\255\255\
\255\255\255\255\033\001\255\255\255\255\255\255\255\255\038\001\
\255\255\255\255\041\001\042\001\043\001\021\001\022\001\255\255\
\255\255\255\255\255\255\027\001\255\255\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\038\001\255\255\
\255\255\041\001\042\001\043\001"

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
  CASE\000\
  OF\000\
  AS\000\
  OR\000\
  NIL\000\
  CONS\000\
  HEAD\000\
  TAIL\000\
  ISNIL\000\
  QUIT\000\
  LPAREN\000\
  RPAREN\000\
  LCURLY\000\
  RCURLY\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT\000\
  COMMA\000\
  EQ\000\
  COLON\000\
  GT\000\
  LT\000\
  DARROW\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  STRINGV\000\
  IDT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 60 "parser.mly"
      ( Bind (_1, _3) )
# 376 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 62 "parser.mly"
      ( Eval _1 )
# 383 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 64 "parser.mly"
      ( TBind (_1, _3))
# 391 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
      ( Quit )
# 397 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( _1 )
# 404 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 73 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 413 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 75 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 422 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 77 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 431 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 79 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 441 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cases) in
    Obj.repr(
# 81 "parser.mly"
      ( TmCase (_2, _4) )
# 449 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'case) in
    Obj.repr(
# 85 "parser.mly"
      ( [_1] )
# 456 "parser.ml"
               : 'cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'case) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cases) in
    Obj.repr(
# 87 "parser.mly"
      ( _1 :: _3 )
# 464 "parser.ml"
               : 'cases))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 91 "parser.mly"
      ( (_2, _4, _7) )
# 473 "parser.ml"
               : 'case))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 96 "parser.mly"
      ( _1 )
# 480 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 98 "parser.mly"
      ( TmSucc _2 )
# 487 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 100 "parser.mly"
      ( TmPred _2 )
# 494 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 102 "parser.mly"
      ( TmIsZero _2 )
# 501 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 104 "parser.mly"
      ( TmConcat (_2, _3) )
# 509 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 106 "parser.mly"
      ( TmApp (_1, _2) )
# 517 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
      ( TmProj (_1, _3) )
# 525 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'appTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 110 "parser.mly"
      ( TmProj (_1, string_of_int _3) )
# 533 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 112 "parser.mly"
    ( TmIsNil (_3, _5) )
# 541 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 114 "parser.mly"
    ( TmHead (_3, _5) )
# 549 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 116 "parser.mly"
    ( TmTail (_3, _5) )
# 557 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 121 "parser.mly"
      ( _2 )
# 564 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'recordList) in
    Obj.repr(
# 123 "parser.mly"
      (  TmRecord _2 )
# 571 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tupleList) in
    Obj.repr(
# 125 "parser.mly"
      ( TmTuple _2 )
# 578 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 127 "parser.mly"
      ( TmVariant (_2, _4, _7))
# 587 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
      ( TmTrue )
# 593 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
      ( TmFalse )
# 599 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
      ( TmVar _1 )
# 606 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 135 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 616 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 140 "parser.mly"
      ( TmString _1 )
# 623 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 142 "parser.mly"
      (  TmNil (_3) )
# 630 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 144 "parser.mly"
      (  TmCons (_3, _5, _6) )
# 639 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'recordTerm) in
    Obj.repr(
# 149 "parser.mly"
        ([_1])
# 646 "parser.ml"
               : 'recordList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'recordTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'recordList) in
    Obj.repr(
# 151 "parser.mly"
        ( _1 :: _3 )
# 654 "parser.ml"
               : 'recordList))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
      ([])
# 660 "parser.ml"
               : 'recordList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 156 "parser.mly"
        ( (_1, _3) )
# 668 "parser.ml"
               : 'recordTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 161 "parser.mly"
        ( [_1] )
# 675 "parser.ml"
               : 'tupleList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleList) in
    Obj.repr(
# 163 "parser.mly"
        ( _1 :: _3 )
# 683 "parser.ml"
               : 'tupleList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 168 "parser.mly"
      ( _1 )
# 690 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 170 "parser.mly"
      ( TyArr (_1, _3) )
# 698 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 174 "parser.mly"
      ( _2 )
# 705 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "parser.mly"
      ( TyBool )
# 711 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 178 "parser.mly"
      ( TyNat )
# 717 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "parser.mly"
      ( TyString )
# 723 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'recordListTy) in
    Obj.repr(
# 182 "parser.mly"
      ( TyRecord _2 )
# 730 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'recordListTy) in
    Obj.repr(
# 184 "parser.mly"
      ( TyVariant _2 )
# 737 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tyList) in
    Obj.repr(
# 186 "parser.mly"
      ( TyTuple _2 )
# 744 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 188 "parser.mly"
    ( TyVar (_1))
# 751 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 193 "parser.mly"
      ( [_1] )
# 758 "parser.ml"
               : 'tyList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tyList) in
    Obj.repr(
# 195 "parser.mly"
      ( _1 :: _3 )
# 766 "parser.ml"
               : 'tyList))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "parser.mly"
       ( [] )
# 772 "parser.ml"
               : 'recordListTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'recordTy) in
    Obj.repr(
# 200 "parser.mly"
        ( [_1] )
# 779 "parser.ml"
               : 'recordListTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'recordTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'recordListTy) in
    Obj.repr(
# 202 "parser.mly"
        ( _1 :: _3)
# 787 "parser.ml"
               : 'recordListTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 206 "parser.mly"
        ( (_1, _3) )
# 795 "parser.ml"
               : 'recordTy))
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
