%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token BOOL
%token NAT
%token STRING
%token CASE
%token OF
%token AS
%token OR
%token NIL 
%token CONS
%token HEAD
%token TAIL 
%token ISNIL 
%token QUIT

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LBRACKET
%token RBRACKET
%token DOT
%token COMMA
%token EQ
%token COLON
%token GT
%token LT
%token DARROW
%token ARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> STRINGV
%token <string> IDT

%start s
%type <Lambda.command> s

%%

s :
    IDV EQ term EOF
      { Bind ($1, $3) }
    | term EOF
      { Eval $1 }
    | IDT EQ ty EOF
      { TBind ($1, $3)}
    | QUIT EOF
      { Quit }


term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
  | CASE term OF cases 
      { TmCase ($2, $4) } 
      
cases: 
    case 
      { [$1] } 
  | case OR cases 
      { $1 :: $3 } 

case: 
    LT IDV EQ IDV GT DARROW appTerm 
      { ($2, $4, $7) }


appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | appTerm DOT IDV
      { TmProj ($1, $3) }
  | appTerm DOT INTV
      { TmProj ($1, string_of_int $3) }
  | ISNIL LBRACKET ty RBRACKET atomicTerm
    { TmIsNil ($3, $5) }
  | HEAD LBRACKET ty RBRACKET atomicTerm
    { TmHead ($3, $5) }
  | TAIL LBRACKET ty RBRACKET atomicTerm
    { TmTail ($3, $5) }


atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LCURLY recordList RCURLY
      {  TmRecord $2 }
  | LCURLY tupleList RCURLY
      { TmTuple $2 }
  | LT IDV EQ term GT AS ty
      { TmVariant ($2, $4, $7)}
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }
  | NIL LBRACKET ty RBRACKET 
      {  TmNil ($3) }
  | CONS LBRACKET ty RBRACKET atomicTerm atomicTerm
      {  TmCons ($3, $5, $6) }


recordList:
    recordTerm
        {[$1]}
    | recordTerm COMMA recordList
        { $1 :: $3 }
    | {[]}

recordTerm: 
    IDV EQ term
        { ($1, $3) }
    

tupleList:
    | term
        { [$1] }
    | term COMMA tupleList
        { $1 :: $3 }


ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty 
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LCURLY recordListTy RCURLY
      { TyRecord $2 }
  | LT recordListTy GT
      { TyVariant $2 }
  | LCURLY tyList RCURLY
      { TyTuple $2 }
  | IDT 
        {TyVar ($1)}
    

tyList:
  | ty
      { [$1] }
  | ty COMMA tyList
      { $1 :: $3 }

recordListTy:
    |  { [] }
    | recordTy
        { [$1] }
    | recordTy COMMA recordListTy
        { $1 :: $3}

recordTy:
    IDV COLON ty
        { ($1, $3) }



