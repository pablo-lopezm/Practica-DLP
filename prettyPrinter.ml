open Format 
open Lambda 

let rec print_term = function 

   TmIf (t1, t2, t3) ->
        open_box 1;
        print_string "if";
        print_space();
        print_term t1;
        print_space();
        print_string "then";
        print_space ();
        print_term t2;
        print_space ();
        print_string "else";
        print_space();
        print_term t3;
        close_box ();

    | TmAbs(idv, ty, t) -> 
        open_box 1; 
        print_string "lambda";
        print_space ();
        print_string idv;
        print_string ":";
        print_space ();
        print_ty ty;
        print_string ".";
        print_space();
        print_term t;
        close_box ();

    | TmLetIn (idv, t1, t2) ->
        open_box 1;
        print_string "let";
        print_space();
        print_string idv;
        print_space();
        print_string "=";
        print_space ();
        print_term t1;
        print_space();
        print_string "in";
        print_space();
        print_term t2;
        close_box();

    | TmCase (t, cases) ->
      open_box 1;
      print_string "case";
      print_space ();
      print_term t;
      print_space ();
      print_string "of";
      print_space ();
      print_cases cases;
      close_box ();

    | TmFix term -> 
        open_box 1;
        print_string "fix";
        print_space ();
        print_string "(";
        print_term term;
        print_string ")";
        close_box ();
        

    | appTerm ->
        open_box 1; 
        print_appterm appTerm;
        close_box ();


and print_cases cases =
  match cases with
    [] -> () 
    | [(label, idv, appTerm)] ->
      print_string "<";
      print_string label;
      print_string "=";
      print_string idv;
      print_string ">";
      print_space ();
      print_string "->";
      print_space ();
      print_appterm appTerm; (* Un único caso *)
    | (label, idv, appTerm) :: rest ->
      print_string "<";
      print_string label;
      print_string "=";
      print_string idv;
      print_string ">";
      print_space ();
      print_string "->";
      print_space ();
      print_appterm appTerm;
      print_string " | "; (* Separador entre casos *)
      print_cases rest;



and print_appterm = function
  
  TmPred(atomicTerm) ->
      open_box 1;
      print_string "pred";
      print_space ();
      print_atomic_term atomicTerm;
      close_box();
  | TmIsZero (atomicTerm) ->
      open_box 1;
      print_string "iszero";
      print_space ();
      print_atomic_term atomicTerm;
      close_box();

  | TmConcat(atomicTerm1, atomicTerm2) ->
      open_box 1;
      print_string "concat";
      print_space ();
      print_atomic_term atomicTerm1;
      print_space ();
      print_atomic_term atomicTerm2;
      close_box();

  |  TmApp (appTerm, atomicTerm) ->
      open_box 1;
      print_appterm appTerm;
      print_space ();
      print_atomic_term atomicTerm;
      close_box();

  | TmProj (appTerm, v) ->
      open_box 1;
      print_appterm appTerm;
      print_string ".";
      print_string v;
      close_box();

  | TmIsNil (ty, atomicTerm) ->
      open_box 1;
      print_string "isnil";
      print_space ();
      print_string ":";
      print_ty ty;
      print_space ();
      print_atomic_term atomicTerm;
      close_box();

  | TmHead (ty,atomicTerm) ->
      open_box 1;
      print_string "head";
      print_space ();
      print_string ":";
      print_ty ty;
      print_space ();
      print_atomic_term atomicTerm;
      close_box();

  | TmTail (ty,atomicTerm) ->
      open_box 1;
      print_string "tail";
      print_space ();
      print_string ":";
      print_ty ty;
      print_space ();
      print_atomic_term atomicTerm;
      close_box();
  | atomicTerm ->
    print_atomic_term atomicTerm

and print_atomic_term = function
  TmRecord (recordList) -> 
       open_box 1;
       print_string "{";
       print_recordList recordList;
       print_string "}";
       close_box ();
  | TmTuple(tupleList) ->
      open_box 1;
      print_string "{";
      print_List tupleList;
      print_string "}";
      close_box ();
  | TmVariant(idv, term, _) ->
      open_box 1;
      print_string "<";
      print_string (idv);
      print_space ();
      print_string "=";
      print_space ();
      print_term term;
      print_string ">";
      close_box ();
  | TmTrue -> 
      open_box 1;
      print_string "true";
      close_box ();
  | TmFalse ->
      open_box 1;
      print_string "false";
      close_box ();
  | TmVar s ->
      open_box 1;
      print_string s;
      close_box ();
  | TmZero -> 
      open_box 1;
      print_string "0";
      close_box ();

  | TmSucc t ->
    let rec count_succ n t' = 
      match t' with
      | TmZero -> 
        print_string (string_of_int n) (* Si llegamos a `TmZero`, imprimimos el número acumulado *)
      | TmSucc s -> 
        count_succ (n + 1) s (* Seguimos contando los `TmSucc` *)
      | _ -> 
        (* Si no es `TmZero` ni `TmSucc`, imprimimos el término completo *)
        open_box 1;
        print_string "succ";
        print_space();
        print_term t;
        close_box();
      in 
        count_succ 1 t

  | TmString(string) ->
      open_box 1;
      print_string "\"";
      print_string string;
      print_string "\"";
      close_box ();
  | TmNil(ty) ->
      open_box 1;
      print_string "nil[";
      print_ty ty;
      print_string "]";
      close_box ();
  | TmCons(ty,atomicTerm1, atomicTerm2 ) ->
      open_box 1;
      print_string "cons[";
      print_ty ty;
      print_string "]";
      print_space ();
      print_atomic_term atomicTerm1;
      print_space ();
      print_atomic_term atomicTerm2;
      close_box ();

  | term -> 
      open_box 1;
      print_space();
      print_string "(";
      print_term term;
      print_string ")";
      print_space ();
      close_box ();

and print_recordList recordList =
  match recordList with
     [] ->
        () 
    | [(label, term)] ->
        print_string label;
        print_string "=";
        print_space();
        print_term term 
    | (label, term) :: rest ->
        print_string label;
        print_string "=";
        print_space();
        print_term term; 
        print_string ","; 
        print_space();
        print_recordList rest ;

and print_List termList =
  match termList with
    [] ->
        () 
    | [term] ->
        print_term term 
    | term :: rest ->
        print_term term; 
        print_string ","; 
        print_space ();
        print_List rest;


and print_ty = function
  TyArr (ty1, ty2) -> 
    open_box 1;
    print_ty ty1;
    print_space();
    print_string "->";
    print_space();
    print_ty ty2;
    close_box ();

  
  | ty1 -> 
    print_atomic_ty ty1

and print_atomic_ty = function 
    TyBool -> 
      open_box 1;
      print_string "Bool";
      close_box ();
    | TyNat -> 
      open_box 1;
      print_string "Nat";
      close_box ();
    | TyString -> 
      open_box 1;
      print_string "String";
      close_box ();
    | TyRecord tyRecordList -> 
        open_box 1;
        print_string "{";
        print_recordTyList tyRecordList;
        print_string "}";
        print_space ();
        
        close_box ();
    | TyVariant tyRecordList -> 
        open_box 1;
        print_string "<";
        print_recordTyList tyRecordList;
        print_string ">";
        close_box ();
    | TyTuple tyList -> 
        open_box 1;
        print_string "{";
        print_tyList tyList;
        print_string "}";
        close_box ();

    | TyList ty -> 
      print_atomic_ty ty

    | TyVar s -> 
      open_box 1;
      print_string s;
      close_box ();
    
    | ty -> 
      open_box 1;
      print_string "(";
      print_ty ty;
      print_string ")";
      close_box(); 

      
and print_tyList tyList =
  match tyList with
    | [] ->
        () 
    | [ty] ->
        print_ty ty 
    | ty :: rest ->
        print_ty ty; 
        print_string ","; 
        print_space ();
        print_tyList rest 

and print_recordTyList tyRecordList =
  match tyRecordList with
    | [] ->
        () (* Lista vacia, no hay nada que imprimir *)
    | [(label, ty)] ->
        print_string label;
        print_string ":";
        print_space ();
        print_ty ty (* Un solo elemento, sin separadores *)
    | (label, ty) :: rest ->
        print_string label;
        print_string ":";
        print_space ();
        print_ty ty; (* Imprimir el par actual *)
        print_string ", "; (* Separador entre pares *)
        print_recordTyList rest (* Recursivamente imprimir el resto de la lista *)
  


and print_eval tyTm tm' =
  open_box 1;
  print_string "-";
  print_space ();
  print_string ":";
  print_space ();
  print_ty tyTm;
  print_space();
  print_string "=";
  print_space ();
  print_term tm';
  print_newline ();
  close_box ();
  print_flush();

  
and print_bind s tyTm tm' =  
  open_box 1;
  print_string s;
  print_space (); 
  print_string ":";
  print_space ();
  print_ty tyTm;
  print_space ();
  print_string "=";
  print_space ();
  print_term tm';
  print_newline ();
  close_box ();
  print_flush();

and print_tbind s tyTm =  
  open_box 1;
  print_string "type";
  print_space ();
  print_string s;
  print_space (); 
  print_string "=";
  print_space ();
  print_ty tyTm;
  print_newline ();
  close_box ();
  print_flush();

  
