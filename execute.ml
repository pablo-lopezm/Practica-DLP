open Lambda
open PrettyPrinter

let execute ctx = function
  Eval tm ->
    let tyTm = typeof ctx tm in 
    let tm' = eval ctx tm in 
      print_eval tyTm tm';
      ctx
    (*print_endline("- : " ^ string_of_ty ctx tyTm ^ " = " ^ string_of_term ctx tm')*) 

  | Bind(s, tm) ->
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in 
      print_bind s tyTm tm';
      (addvbinding ctx s tyTm tm')
    (*pretty printer*) (*print_endline(s ^ " : " ^ string_of_ty ctx tyTm ^ " = " ^ string_of_term ctx tm')*)
  
  | TBind (s, tyVar) ->
      let bty = base_ty ctx tyVar in 
      (*print_endline("type " ^ s ^ " = " ^ string_of_ty ctx bty);*)
      print_tbind s tyVar;
      addtbinding ctx s bty
    
  | Quit -> 
    raise End_of_file
 ;;