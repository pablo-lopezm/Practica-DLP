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
%token QUIT

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token DOT
%token COMMA
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    IDV EQ term EOF
      { Bind ($1, $3) }
    |term EOF
      { Eval $1 }
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
  | appTerm DOT INTV
      { TmProj ($1, string_of_int $3) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LCURLY tupleList RCURLY 
      { TmTuple $2 }
  | LCURLY recordList RCURLY 
      { TmRecord $2}
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

tupleList:
    | term
        { [$1] }
    | term COMMA tupleList
        { $1 :: $3 }
    | 
        { [] } 

recordList:
    | STRINGV EQ term
        { [($1, $3)] }
    | STRINGV EQ term COMMA recordList
        { ($1, $3) :: $5 }


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
  | LCURLY tyList RCURLY
      { TyTuple $2 }

tyList:
    ty
      { [$1] }
  | ty COMMA tyList
      { $1 :: $3 }
