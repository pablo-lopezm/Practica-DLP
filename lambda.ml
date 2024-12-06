
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyRecord of (string * ty) list
  | TyTuple of ty list 
  | TyVar of string
  | TyVariant of (string * ty) list
  | TyList of ty 
;;


type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmRecord of (string * term) list
  | TmTuple of term list
  | TmProj of term * string
  | TmVariant of string * term * ty (*TmVariant*)
  | TmCase of term * (string * string * term) list
  | TmNil of ty                  
  | TmCons of ty * term * term      
  | TmIsNil of ty * term              
  | TmHead of ty * term               
  | TmTail of ty * term   
;;

type command =
  Eval of term
  | Bind of string * term
  | TBind of string * ty (*Type variable*)
  | Quit
;;

type binding =
  TyBind of ty
  | TyTmBind of (ty * term)
;;

type context =
  (string * binding) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx s ty =
  (s, TyBind ty) :: ctx
;;

let addvbinding ctx s ty tm =
  (s, TyTmBind (ty, tm)) :: ctx
;;

let gettbinding ctx s =
  match List.assoc s ctx with 
    TyBind ty -> ty
    | TyTmBind (ty, _) -> ty
;;

let getvbinding ctx s = 
  match List.assoc s ctx with 
    TyTmBind (_, tm) -> tm
    | _ -> raise Not_found
;;

exception Type_error of string
;;

(* 
   The function `is_subtype` checks if one type (`tyS`) is a subtype of another (`tyT`) within a given context (`ctx`).
   The function handles two primary cases:
   
   1. TyRecord types:
      - It checks if every field in the target record type (`tyT`) is present in the source record type (`tyS`).
      - It ensures that for each field in the target (`fieldsT`), the corresponding field in the source (`fieldsS`) either exists and is a subtype, or the function returns `false`.
      
   2. TyArr (function types):
      - It checks if the domain (`tyT1`) of the target function is a subtype of the domain of the source function (`tyS1`).
      - Similarly, it checks if the range (`tyT2`) of the target function is a subtype of the range of the source function (`tyS2`).
      
   3. Other types**:
      - If the types are not records or functions, it simply checks if the two types are identical.
*)
let rec is_subtype ctx tyS tyT =
  match (tyS, tyT) with
  (TyRecord fieldsS, TyRecord fieldsT) ->
      List.for_all
        (fun (labelT, tyT) ->
          match List.assoc_opt labelT fieldsS with
          | Some tyS -> is_subtype ctx tyS tyT
          | None -> false)
        fieldsT
  | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
    is_subtype ctx tyT1 tyS1 && is_subtype ctx tyS2 tyT2
  | _ -> (tyS = tyT)
;;

(* 
   Normalizes types in the context, resolving any type variables and ensuring that types are in their base form.
    - Simple types like `TyBool`, `TyNat`, and `TyString`, it just returns the same type.
    - Complex types like `TyArr`, `TyTuple`, `TyRecord`, `TyVariant`, and `TyList`, it recursively processes their components and applies the same transformation to each.
    - If a type variable (`TyVar`) is encountered, the function attempts to resolve it using the context (`ctx`). If no binding exists for the type variable, a `Type_error` is raised.
*)
let rec base_ty ctx ty = match ty with
  TyBool -> TyBool
  | TyNat -> TyNat
  | TyString -> TyString
  | TyArr (ty1, ty2) -> TyArr (base_ty ctx ty1, base_ty ctx ty2)
  | TyTuple tys -> TyTuple (List.map (base_ty ctx) tys)
  | TyRecord tys -> TyRecord (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyVariant tys -> TyVariant (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyVar s -> (try gettbinding ctx s with _ -> raise (Type_error ("no binding type for variable " ^s)))
  | TyList ty -> TyList (base_ty ctx ty)
;;


let rec typeof ctx tm = match tm with
  (* T-True :  boolean type for the true term *)
  TmTrue ->
    TyBool

  (* T-False:  boolean type for the false term *)
  | TmFalse ->
      TyBool

  (* T-If : Evaluates the type of an `if` term (conditional). 
     Ensures the condition is boolean type and both branches have the same type. 
     Returns that common type. *) 
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else 
        raise (Type_error "guard of conditional not a boolean")

  (* T-Zero : numeric type for the zero term*)
  | TmZero ->
      TyNat

  (* T-Succ : Checks that the argument is numeric. 
     If so, returns the numeric (natural) type. *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

  (* T-Pred : Checks that the argument is numeric. 
     If so, returns the numeric (natural) type. *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

  (* T-Iszero : Checks if the argument is numeric. 
     Returns a boolean if numeric, otherwise raises an error.*)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

  (* T-Var : returns the type of the identifier *)
  | TmVar x ->
      (try gettbinding ctx x with 
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

  (* T-Abs : Computes the type of the body in an extended context with the parameter. *)
  | TmAbs (x, tyT1, t2) ->
      let btyT1 = base_ty ctx tyT1 in
      let ctx' = addtbinding ctx x btyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (btyT1, tyT2)

  (* T-App: Applies a function to an argument. 
     Checks that the argument type is a valid subtype of the function's input type. 
     Returns the function's output type.*)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
      | TyArr (tyT11, tyT12) ->
          if is_subtype ctx tyT2 tyT11 then tyT12    
          else raise (Type_error "parameter type mismatch")
      | _ -> raise (Type_error "arrow type expected"))

  (* T-Let : Evaluates the type of a `let-in` definition. 
     Adds the term to the context and evaluates the body in that extended context.*)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2
      
  (* T-Fix : Verifies and evaluates a `fix` term. 
     Ensures that the body type is compatible with the domain and range of the function type.*)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in 
        (match tyT1 with 
          TyArr (tyT11, tyT12) -> 
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))

  (* T-String : Returns the string type for text terms. *)    
  | TmString _-> 
      TyString

  (* T-Concat : Evaluates the concatenation of two strings.*)
  | TmConcat (t1, t2) -> 
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString 
      else raise (Type_error "argument of concat is not a string")

  (* T-Record : Evaluates the type of a record. 
     Computes the type of each field and returns a record type with those field types.*)
  | TmRecord fields ->
    TyRecord (List.map (fun (name, value) -> (name, typeof ctx value)) fields)

  (* T-Tuple : Evaluates the type of a tuple. 
   Computes the type of each element in the tuple and returns a tuple type with those element types.*)
  | TmTuple terms ->
      TyTuple (List.map (typeof ctx) terms)

  (* T-Projection : projection from a tuple or record.
    - Tuples: type of the element at the given index .
    - Records: the type of the field with the given name. *)
  | TmProj (t, s) ->
    let tyT = typeof ctx t in
    (match tyT with
      | TyTuple tyList -> let idx = int_of_string s in
                            if idx > 0 && idx <= List.length tyList then
                              (List.nth tyList (idx - 1))
                            else
                              raise (Type_error ("index " ^ s ^ " not found"))
      | TyRecord fields -> (
        match List.assoc_opt s fields with
          | Some ty -> ty
          | None -> raise (Type_error ("Field '" ^ s ^ "' not found in record")))
      | _ -> raise (Type_error "Projection applied to non-record type"))

  (* T-Variant : Evaluates a variant term. 
     Ensures the value matches the expected type for the specified case.
     Returns the variant type if the case and value are valid. *)
  | TmVariant (s, t, ty) ->
       let tyT1 = typeof ctx t in 
       let tyT2 = base_ty ctx ty in 
       (match tyT2 with 
         TyVariant l ->
          (try if tyT1 = List.assoc s l then tyT2
          else raise (Type_error "type mismatch in variant") 
          with 
          Not_found -> raise (Type_error ("case" ^ s ^ "not found")))

       | _ -> raise (Type_error "variant expected"))
  
  (* T-Case : Evaluates a case term on a variant. 
     Checks that all cases match the variant tags and return the same type. 
     Returns the common type of all cases. *)
  | TmCase (t, cases) -> 
        let tyT1 = typeof ctx t in 
        (match tyT1 with 
          TyVariant l -> 
            let vtags = List.map (function (tag, _) -> tag) l in 
            let ctags = List.map (function (tag, _,_) -> tag) cases in 
            if List.length vtags = List.length ctags && List.for_all (function tag -> List.mem tag vtags) ctags
            then
              let (tag1, id1, tm1) = List.hd cases in
              let ty1 = List.assoc tag1 l in
              let ctx1 = addtbinding ctx id1 ty1 in 
              let rty = typeof ctx1 tm1 in 
              let rec aux = function
                [] -> rty
                | (tagi, idi, tmi)::rest -> 
                  let tyi = List.assoc tagi l in
                  let ctxi = addtbinding ctx idi tyi in 
                  let tyi = typeof ctxi tmi in
                  if tyi = rty then aux rest
                  else raise (Type_error "cases return different types")
                in aux (List.tl cases)
              else 
                raise (Type_error "variant and cases have different tags")
          | _ -> raise (Type_error "variant expected"))

  (* T-Nil : Returns the type of an empty list, which is a list of the specified type. *)
  | TmNil ty -> TyList ty
  
  (* T-Cons : Evaluates a `cons` term (constructing a list). 
     Ensures that the head type matches the specified list element type and the tail is of list type. 
     Returns the list type.*)
  | TmCons (ty, t1, t2) ->
    let tyT1 = typeof ctx t1 in
    let tyT2 = typeof ctx t2 in
      if (tyT1 != ty) then raise (Type_error "Cons type does not match term")
      else 
        (match tyT2 with
          | TyList tyElem when tyElem = tyT1 -> tyT2
          | TyList _ -> raise (Type_error "Type mismatch in list construction")
          | _ -> raise (Type_error "Second argument of cons must be a list"))
  
  (* T- IsNil: Checks if a term is an empty list of a given type.
     Returns a boolean type. *)
  | TmIsNil (ty,tm) ->
    let ty = base_ty ctx ty in 
     (match typeof ctx tm with
        | TyList ty2 -> if (ty==ty2) then TyBool
                        else raise (Type_error "isnil type does not match List type")
        | _ -> raise (Type_error "isnil applied to non-list type"))

  (* T-Head : Extracts the first element of a list. 
     Ensures the term is a list of the specified type. 
     Returns the element type of the list.*)     
  | TmHead (ty, tm) ->
    let ty = base_ty ctx ty in 
     (match typeof ctx tm with
       | TyList ty2 -> if (ty==ty2) then TyList ty2
                          else raise (Type_error "head type does not match List type")
       | _ -> raise (Type_error "head applied to non-list type"))
       
  (* T-Tail : Returns the tail of a list. 
     Ensures the term is a list of the specified type. 
     Returns the list type. *)
  | TmTail (ty, tm) ->
    let ty = base_ty ctx ty in 
    (match typeof ctx tm with
    | TyList ty2 -> if (ty==ty2) then TyList ty2
                       else raise (Type_error "tail type does not match List type")
    | _ -> raise (Type_error "tail applied to non-list type"))

;;


(* TERMS MANAGEMENT (EVALUATION) *)

(* Computes the difference between two lists. 
   Returns a list containing elements of l1 that are not in l2. *)
let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

(* Computes the union of two lists. 
   Combines all elements from l1 and l2 without duplicates. *)
let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

(* Determines the set of free variables in a term. 
   Free variables are variables that are not bound by a lambda abstraction. *)
let rec free_vars tm = match tm with
    TmTrue ->
      []

  | TmFalse ->
      []

  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)

  | TmZero ->
      []

  | TmSucc t ->
      free_vars t

  | TmPred t ->
      free_vars t

  | TmIsZero t ->
      free_vars t

  | TmVar s ->
      [s]
  (* An abstraction doesn't add to the set of free variables unless it's bound inside the body *)
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]

   (* An application combines the free variables of both the function and the argument *)  
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)

  (* A let binding introduces new free variables by merging the free variables of the 
       expression and the body, while avoiding counting the bound variable *)

  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)

  | TmFix t -> 
      free_vars t

  | TmString _ -> 
    []

  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)

  (* Union of the free variables of all its terms *)
  | TmTuple terms ->
    List.fold_left (fun acc t -> lunion acc (free_vars t)) [] terms

  (* Union of the free variables of each value in the record *)
  | TmRecord fields -> 
    List.fold_left (fun acc (name, value) -> lunion acc (free_vars value)) [] fields

  (* Projecting from a term just gives us its free variables *)
  | TmProj (t, _) ->
    free_vars t

  (* A variant's free variables are those of its inner term *)
  | TmVariant (s, tm, ty) ->
     free_vars tm

   (* Includes free variables of both the term and the bodies of the cases,
     while accounting for the variables bound within each case *)
  | TmCase (t, cases) ->
    lunion (free_vars t)
      (List.fold_left
          (fun acc (s1, s2, body) -> 
              lunion acc (ldif (free_vars body) [s1; s2])) [] cases)

  | TmNil _ -> []

  (*Introduces free variables from both the first and second elements *)
  | TmCons (_, t1, t2) -> 
      lunion (free_vars t1) (free_vars t2)

  | TmHead (_, tm) -> (free_vars tm)
  | TmTail (_, tm) -> (free_vars tm)
  | TmIsNil (_, tm) -> (free_vars tm)
;;

(* The `fresh_name` function generates a fresh variable name by checking if the 
   given name already exists in the list of names. If it does not exist, the 
   name is returned; otherwise, the function appends a `'` to the name and 
   recursively tries again until a unique name is found. *)
let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

(* Performs substitution of a term `s` for a variable `x` in a term `tm`. *)
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  (* This case handles the application of substitution to an abstraction.
    If the variable `y` is the same as the variable `x` being substituted, the term remains unchanged. 
    Otherwise, the function checks if `y` is free in the substitution term `s`.
    - If it isn't, it proceeds with the substitution of `x` in the body `t`. 
    - If `y` is free in `s`, a fresh variable `z` is generated to avoid capturing,
      and the substitution occurs with `z` replacing `y` in the body. *)    
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)

  (* This case applies substitution to the terms inside a 'let in'.
   If the variable 'y' is the same as 'x', the term 't2' is left unchanged, and the substitution is only applied to `t1`. 
   If `y` is different, the substitution is applied to both `t1` and `t2`. 
   If `y` appears in the substitution term `s`, a fresh variable 'z' is generated to prevent clashes. *)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t -> 
      TmFix (subst x s t)
  | TmString st -> 
      TmString st
  | TmConcat (t1, t2) -> 
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple terms ->
      TmTuple (List.map (subst x s) terms)
  | TmRecord fields -> 
      TmRecord (List.map (fun (name, value) -> (name, subst x s value)) fields)
  | TmProj (t, string) ->
      TmProj (subst x s t, string)
  | TmVariant (label, t, ty) -> TmVariant (label, subst x s t, ty)
  (* The `TmCase` case handles the substitution for a `case` expression.
   The term `t` in the `TmCase` is substituted first with the expression `s`.
   Afterward, the list of cases is processed. For each case the body is substituted with the expression `s`. 
   The label itself is not affected by the substitution. *)
  | TmCase (t, cases) ->
      TmCase (subst x s t,
            List.map (fun (label,label2, body) -> (label, label2, subst x s body)) cases)
  | TmNil ty -> TmNil ty
  | TmCons (ty, t1, t2) -> 
     TmCons (ty, subst x s t1, subst x s t2)
  | TmHead (ty, tm) -> TmHead (ty,(subst x s tm))
  | TmTail (ty, tm) -> TmTail (ty,(subst x s tm))
  | TmIsNil (ty, tm) -> TmIsNil (ty,(subst x s tm))
   
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

(* Checks if a term is a value, which is a term that cannot be reduced further *)
let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _-> true
  | TmNil ty -> true
  | t when isnumericval t -> true
  | TmVariant (string, term, ty) -> isval term 
  | TmTuple terms -> List.for_all isval terms
  | TmRecord fields -> List.for_all (fun (name, value) -> isval value) fields
  | TmCons (ty, t1, t2) -> (isval t1) && (isval t2)
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)
  
  (* E-Fix: If the term is a fixpoint operation (`TmFix`), substitute the term into the body. *)
  | TmFix(TmAbs(x, _, t2)) ->
      subst x tm t2
  
  (* E-Fix: If the term `TmFix` is not evaluated yet, evaluate it first. *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in 
      TmFix t1'
  
  (* E-Concatenate: If both terms are string values, concatenate them. *)
  | TmConcat (TmString s1, TmString s2) -> 
      TmString (s1 ^ s2)
  
  (* E-Concatenate: If the first term is a string and the second term is not yet evaluated, 
     evaluate the second term first. *)
  | TmConcat (TmString s1, t2) -> 
    let t2' = eval1 ctx t2 in 
    TmConcat (TmString s1, t2')
  
  (* E-Concatenate: If the first term is not yet evaluated, evaluate it first. *)
  | TmConcat (t1, t2) -> 
      let t1' = eval1 ctx t1 in 
      TmConcat (t1', t2)

  (* E-Var: Look up the value of the variable in the context. *)    
  | TmVar s -> 
      getvbinding ctx s
  
  (* E-Tuple: If any term in the tuple is not evaluated yet, evaluate it. *)
  | TmTuple terms ->
    let rec eval_terms = function
      | [] -> raise NoRuleApplies
      | t :: ts when isval t -> t :: eval_terms ts
      | t :: ts -> eval1 ctx t :: ts
    in
    TmTuple (eval_terms terms)

  (* E-Record: If any field value in the record is not evaluated yet, evaluate it. *)
  | TmRecord fields ->
      let rec eval_fields = function
        | [] -> raise NoRuleApplies  
        | (name, value) :: rest when isval value -> (name, value) :: eval_fields rest
        | (name, value) :: rest -> (name, eval1 ctx value) :: rest
      in
      TmRecord (eval_fields fields)
  
  (* E-Proj: If the projection is not evaluated yet, evaluate it. *)    
  | TmProj (TmTuple terms, s) when List.for_all isval terms -> 
      let idx = int_of_string s in
    if idx > 0 && idx <= List.length terms then
      List.nth terms (idx - 1)
    else
      raise (Type_error ("index" ^ s ^ "not found"))

  (* E-Proj: If the term is a tuple and all elements are values, access the index. *)
  | TmProj (TmRecord terms, s) when List.for_all (fun (name, value) -> isval value) terms ->
      (try 
        List.assoc s terms
      with 
      | Not_found  -> 
        raise (Type_error ("type error: label" ^ s ^ "not found")))

  (* E-Proj: If the term is a record and all field values are values, access the field. *)    
  | TmProj (t, s) ->
      TmProj (eval1 ctx t, s)
  
  (* E-Case: If the term is a variant and the value is evaluated, perform the case analysis. *)
  | TmCase (TmVariant (label, v, ty), cases) when isval v ->
      let (_, id, t) = List.find (function (lb, _, _) -> label = lb) cases in
      subst id v t

  (* E-Case: If the term is not evaluated yet, evaluate it first. *)  
  | TmCase (t, cases) -> 
      let t' = eval1 ctx t in
      TmCase (t', cases)

  (* E-Variant: If the term inside the variant is not evaluated yet, evaluate it first. *)
  | TmVariant(label, t1, ty) -> 
      let t1' = eval1 ctx t1 in 
      TmVariant (label, t1', ty)

  (* E-Cons: If the second part of the cons is evaluated, evaluate the first part. *) 
  | TmCons (ty, t1, t2) when isval t2->
    let t1' = eval1 ctx t1 in 
      TmCons (ty, t1', t2)

  (* E-Cons: If the first part of the cons is evaluated, evaluate the second part. *)
  | TmCons (ty, t1, t2) when isval t1->
    let t2' = eval1 ctx t2 in 
      TmCons (ty, t1, t2')
  
  (* E-Cons: If the none are evaluated, do both *)
  | TmCons (ty, t1, t2) when isval t1->
    let t1' = eval1 ctx t2 in 
      let t2' = eval1 ctx t2 in 
        TmCons (ty, t1', t2')
  
  (* E-IsNil: If the term is a `TmNil`, return `TmTrue`. *)
  | TmIsNil (_, TmNil _) -> TmTrue 

  (* E-IsNil: If the term is a cons list, return `TmFalse`. *) 
  | TmIsNil (_, TmCons (_, _, _)) -> TmFalse
  
   (* E-IsNil: If the term is not evaluated yet, evaluate it first. *)
  | TmIsNil (ty, t) ->
    let t' = eval1 ctx t in
      TmIsNil (ty, t')
  (* E-Head: If the term is a `TmCons` and the first element is evaluated, return it. *) 
  | TmHead (_, TmCons (ty, v1, _)) when isval v1 -> v1
  
   (* E-Head: If the term is an empty list (`TmNil`), return `TmNil`. *)
  | TmHead (_, TmNil ty) -> TmNil ty
  
  (* E-Head: If the term is not evaluated yet, evaluate it first. *)
  | TmHead (ty, t) ->
    let t' = eval1 ctx t in
      TmHead (ty, t')

  (* E-Tail: If the term is a `TmCons` and the second element is evaluated, return it. *)   
  | TmTail (_, TmCons (_, _, v2)) when isval v2 -> v2
  
  (* E-Tail: If the term is an empty list (`TmNil`), return `TmNil`. *)
  | TmTail (_, TmNil ty) -> TmNil ty
  
  (* E-Tail: If the term is not evaluated yet, evaluate it first. *)
  | TmTail (ty, t) ->
    let t' = eval1 ctx t in
      TmTail (ty, t')
  
   (* If no rule applies, raise NoRuleApplies. *)
  | _ ->
    raise NoRuleApplies
;;

(* Applies the context to a term by substituting values of all free variables *)
let apply_ctx ctx tm = 
  List.fold_left ( fun t x -> subst x (getvbinding  ctx x) t) tm (free_vars tm)
;;

(*Recursively evaluates a term in the context by applying reductions and substituting free variables *)
let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

