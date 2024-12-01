
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
;;

type command =
  Eval of term
  | Bind of string * term
  | TBind of string * ty (*variable de tipos*)
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


let rec base_ty ctx ty = match ty with
  TyBool -> TyBool
  | TyNat -> TyNat
  | TyString -> TyString
  | TyArr (ty1, ty2) -> TyArr (base_ty ctx ty1, base_ty ctx ty2)
  | TyTuple tys -> TyTuple (List.map (base_ty ctx) tys)
  | TyRecord tys -> TyRecord (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyVariant tys -> TyVariant (List.map (fun (s, ty) -> (s, base_ty ctx ty)) tys)
  | TyVar s -> (try gettbinding ctx s with _ -> raise (Type_error ("no binding type for variable " ^s)))
  (*| TyList ty -> TyList (base_ty ctx ty)*)
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ctx ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      string_of_ty ctx ty1 ^ " -> " ^ string_of_ty ctx ty2 
  | TyString -> 
      "String"
  | TyTuple tys -> 
      "{" ^ String.concat ", " (List.map (string_of_ty ctx) tys) ^ "}"
  | TyRecord fields -> 
      "{" ^ (String.concat ", " 
          (List.map (fun (name, value) -> name ^ ":" ^ (string_of_ty ctx value)) fields)) ^ "}" 
  | TyVar s ->  string_of_ty ctx (base_ty ctx ty)
  | TyVariant variants ->
    "<" ^ (String.concat " | " (List.map (fun (name, ty) -> name ^ ":" ^ string_of_ty ctx ty) variants)) ^ ">"

;; 

let rec string_of_term ctx = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) -> 
      "if " ^ "(" ^ string_of_term ctx t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term ctx t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term ctx t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term ctx t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term ctx t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term ctx t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) -> (match tyS with 
     TyVar string -> "(lambda " ^ s ^ ":" ^ string ^ ". " ^ string_of_term ctx t ^ ")"
    | _ -> "(lambda " ^ s ^ ":" ^ string_of_ty ctx tyS ^ ". " ^ string_of_term ctx t ^ ")")
      
  | TmApp (t1, t2) ->
      "(" ^ string_of_term ctx t1 ^ " " ^ string_of_term ctx t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term ctx t1 ^ " in " ^ string_of_term ctx t2
  | TmFix t ->
      "(fix " ^ string_of_term ctx t ^ ")"
  | TmString s -> 
    "\"" ^ s ^ "\""
  | TmConcat (t1, t2) -> 
      "concat " ^ "(" ^ string_of_term ctx t1 ^ ")" ^ " " ^ "(" ^ string_of_term ctx t2 ^ ")"
  | TmTuple terms -> 
      "{" ^ String.concat ", " (List.map (string_of_term ctx) terms) ^ "}"
  | TmRecord fields ->
        "{" ^ (String.concat ", " 
          (List.map (fun (name, value) -> name ^ "=" ^ (string_of_term ctx value)) fields)) ^ "}"
  | TmProj (t, s) ->
      string_of_term ctx t ^ "." ^ s
  | TmVariant (s,tm, ty) -> "<" ^ s ^ "=" ^ string_of_term ctx tm ^ ">" 
  | TmCase (t, cases) -> 
    let case = List.map (fun (label, label_value, term) -> 
       string_of_term ctx term) cases in
    let cases_string = String.concat " | " case in 
    "case " ^ (string_of_term ctx t) ^ " of " ^ cases_string


  ;;


let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with (*A lo mejor es getvbinding *)
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let btyT1 = base_ty ctx tyT1 in
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (btyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2
    (* T-FIX *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in 
        (match tyT1 with 
          TyArr (tyT11, tyT12) -> 
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))
  
      (* new rule for string *)
  | TmString _-> 
      TyString
  
      (* new rule for string *)
  | TmConcat (t1, t2) -> 
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString 
      else raise (Type_error "argument of concat is not a string")

  | TmRecord fields -> 
      TyRecord (List.map (fun (name, value) -> (name, typeof ctx value)) fields)

  | TmTuple terms ->
      TyTuple (List.map (typeof ctx) terms)
 
  | TmProj (t, s) ->
    let tyT = typeof ctx t in
    (match tyT with
    | TyTuple tys ->
        let idx = int_of_string s in
        if idx > 0 && idx <= List.length tys then
          List.nth tys (idx - 1)
        else
          raise (Type_error "Projection index out of bounds")

    | TyRecord fields ->
        (match List.assoc_opt s fields with
         | Some ty -> ty
         | None -> raise (Type_error ("Field '" ^ s ^ "' not found in record")))
    | _ ->
        raise (Type_error "Projection applied to non-tuple or non-record type"))

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

 | TmCase (t, cases) ->
    let tyT1 = typeof ctx t in

    (match (base_ty ctx tyT1) with
     | TyVariant variants ->
         let vtags = List.map fst variants in
         let ctags = List.map (fun (tag, _, _) -> tag) cases in
         let missing_tags = List.filter (fun tag -> not (List.mem tag ctags)) vtags in
         let extra_tags = List.filter (fun tag -> not (List.mem tag vtags)) ctags in
         if missing_tags <> [] then
           raise (Type_error ("Faltan casos para etiquetas: " ^ String.concat ", " missing_tags));
         if extra_tags <> [] then
           raise (Type_error ("Etiquetas no válidas en casos: " ^ String.concat ", " extra_tags));
         (* Verificar los tipos de cada rama *)
         let ty_result =
           List.fold_left (fun acc (tag, var, body) ->
             let var_ty = List.assoc tag variants in
             let ctx' = addtbinding ctx var var_ty in
             let body_ty = typeof ctx' body in
             match acc with
             | None -> Some body_ty
             | Some ty when ty = body_ty -> Some ty
             | Some ty ->
                 raise (Type_error "Los tipos de las ramas en TmCase no coinciden")
           ) None cases
         in
         (match ty_result with
          | Some ty -> ty
          | None -> raise (Type_error "TmCase sin ramas válidas"))
     | pepe ->
         raise (Type_error "Se esperaba un TyVariant en TmCase"))


;;


(* TERMS MANAGEMENT (EVALUATION) *)


let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

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
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t -> 
      free_vars t
  | TmString _ -> 
    []
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmTuple terms ->
    List.fold_left (fun acc t -> lunion acc (free_vars t)) [] terms
  | TmRecord fields -> 
    List.fold_left (fun acc (name, value) -> lunion acc (free_vars value)) [] fields
  | TmProj (t, _) ->
    free_vars t
  | TmVariant (s, tm, ty) ->
     free_vars tm
  | TmCase (t, cases) ->
    lunion (free_vars t)
      (List.fold_left
          (fun acc (s1, s2, body) ->
              lunion acc (ldif (free_vars body) [s1; s2])) [] cases)
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

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
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
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
  | TmCase (t, cases) ->
      TmCase (subst x s t,
            List.map (fun (label,label2, body) -> (label, label2, subst x s body)) cases)

;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _-> true
  | t when isnumericval t -> true
  | TmVariant (string, term, ty) -> isval term 
  | TmTuple terms -> List.for_all isval terms
  | TmRecord fields -> List.for_all (fun (name, value) -> isval value) fields
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
      print_endline("abs");
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

  | TmFix(TmAbs(x, _, t2)) ->
      subst x tm t2

  | TmFix t1 ->
      let t1' = eval1 ctx t1 in 
      TmFix t1'

  | TmConcat (TmString s1, TmString s2) -> 
      TmString (s1 ^ s2)
  
  | TmConcat (TmString s1, t2) -> 
    let t2' = eval1 ctx t2 in 
    TmConcat (TmString s1, t2')

  | TmConcat (t1, t2) -> 
      let t1' = eval1 ctx t1 in 
      TmConcat (t1', t2)

  | TmVar s -> 
      getvbinding ctx s
  
  | TmTuple terms ->
    let rec eval_terms = function
      | [] -> raise NoRuleApplies
      | t :: ts when isval t -> t :: eval_terms ts
      | t :: ts -> eval1 ctx t :: ts
    in
    TmTuple (eval_terms terms)

  | TmRecord fields ->
      let rec eval_fields = function
        | [] -> raise NoRuleApplies  (* No hay más reglas para aplicar *)
        | (name, value) :: rest when isval value -> (name, value) :: eval_fields rest
        | (name, value) :: rest -> (name, eval1 ctx value) :: rest
      in
      TmRecord (eval_fields fields)

  (*| TmRecordField (record, name) ->
      (match type_check ctx record with
       | TyRecord field_types ->
           (match List.assoc_opt name field_types with
            | Some ty -> ty
            | None -> failwith ("Field " ^ name ^ " not found in type"))
       | _ -> failwith "Expected a record type")*)
  
  | TmProj (TmTuple terms, s) when List.for_all isval terms -> 
      let idx = int_of_string s in
    if idx > 0 && idx <= List.length terms then
      List.nth terms (idx - 1)
    else
      raise (Type_error ("type error: index" ^ s ^ "not found"))

  
  | TmProj (TmRecord terms, s) when List.for_all (fun (name, value) -> isval value) terms ->
      (try 
        List.assoc s terms
      with 
      | Not_found  -> 
        raise (Type_error ("type error: label" ^ s ^ "not found")))

      
  | TmProj (t, s) ->
      TmProj (eval1 ctx t, s)

  | TmCase (TmVariant (label, v, ty), cases) when isval v ->
    (match List.find_opt (fun (case_label, _, _) -> case_label = label) cases with
      | Some (_, var, body) -> subst var v body
      | None -> raise NoRuleApplies)
    
  | TmCase (t, cases) -> 
        let t' = eval1 ctx t in
        TmCase (t', cases)


  | _ ->
    raise NoRuleApplies
;;

let apply_ctx ctx tm = 
  List.fold_left ( fun t x -> subst x (getvbinding  ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;


let execute ctx = function
  Eval tm ->
    let tyTm = typeof ctx tm in 
    let tm' = eval ctx tm in 
    (*pretty printer*) print_endline("- : " ^ string_of_ty ctx tyTm ^ " = " ^ string_of_term ctx tm');
    ctx 

  | Bind(s, tm) ->
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in 
    (*pretty printer*) print_endline(s ^ " : " ^ string_of_ty ctx tyTm ^ " = " ^ string_of_term ctx tm');
    addvbinding ctx s tyTm tm'
  
  | TBind (s, tyVar) ->
      let bty = base_ty ctx tyVar in 
      print_endline("type " ^ s ^ " = " ^ string_of_ty ctx bty);
      addtbinding ctx s bty
    
  | Quit -> 
    raise End_of_file
 ;;

