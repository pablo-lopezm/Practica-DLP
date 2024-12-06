 
(* Definition of the type 'ty', which represents various types in the language. *)
type ty =
    TyBool                          (* Boolean type *)
  | TyNat                           (* Natural number type *)
  | TyArr of ty * ty                (* Function type: ty -> ty *)
  | TyString                        (* String type *)
  | TyRecord of (string * ty) list  (* Record type with named fields *)
  | TyTuple of ty list              (* Tuple type  *)
  | TyVar of string                 (* Type variable *)
  | TyVariant of (string * ty) list (* Variant type *)
  | TyList of ty                    (* List of types *)
;;

(* Definition of the type 'term': expressions in the language. *)
type term =
    TmTrue                          (* Boolean literal: true *)
  | TmFalse                         (* Boolean literal: false *)
  | TmIf of term * term * term      (* If-then-else construct *)
  | TmZero                          (* Zero (used for natural numbers) *)
  | TmSucc of term                  (* Successor (e.g., n + 1) *)
  | TmPred of term                  (* Predecessor (e.g., n - 1) *)
  | TmIsZero of term                (* Check if a term is zero *)
  | TmVar of string                 (* Variable *)
  | TmAbs of string * ty * term     (* Lambda abstraction: function definition *)
  | TmApp of term * term            (* Function application *)
  | TmLetIn of string * term * term (* Let-in expression for variable binding *)
  | TmFix of term                   (* Fixpoint operator for recursion *)
  | TmString of string              (* String literal *)
  | TmConcat of term * term         (* String concatenation *)
  | TmRecord of (string * term) list(* Record literal *)
  | TmTuple of term list            (* Tuple literal *)
  | TmProj of term * string         (* Projection: Access a field in a record *)
  | TmVariant of string * term * ty (* Variant type constructor *)
  | TmCase of term * (string * string * term) list (* Case analysis for variants *)
  | TmNil of ty                     (* Empty list *)
  | TmCons of ty * term * term      (* List constructor: head :: tail *)
  | TmIsNil of ty * term            (* Check if a list is empty *)
  | TmHead of ty * term             (* Head of a list *)
  | TmTail of ty * term             (* Tail of a list *)
;;

(* Commands to interact with the system. *)
type command = 
  Eval of term                    (* Evaluate term *)
  | Bind of string * term         (* Bind variable to term *)
  | TBind of string * ty          (* Bind type to name *)
  | Quit                          (* Exit the system *)
;;

(* Bindings in the context. *)
type binding = 
  TyBind of ty   (* Type binding *)
  | TyTmBind of (ty * term) (* Type-term binding *)
;;

(* Context: list of bindings, associating names with their definitions. *)
type context =
  (string * binding) list
;;

(* Declarations of utility functions and variables. *)
val emptyctx : context;;                                        (* Empty context *)
val addtbinding : context -> string -> ty -> context;;          (* Add type binding *)
val addvbinding : context -> string -> ty -> term -> context;;  (* Add variable binding *)
val gettbinding : context -> string -> ty;;                     (* Get type binding *)
val getvbinding : context -> string -> term;;                   (* Get term binding *)

(* Functions for type inference and checking. *)
val base_ty : context -> ty -> ty;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

(* Evaluation functions. *)
exception NoRuleApplies;;
val eval : context -> term -> term;;
