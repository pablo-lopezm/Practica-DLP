open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let read_until_double_semicolon () =
  let rec loop acc =
    let line = read_line () in
    if String.length line >= 2 && String.sub line (String.length line - 2) 2 = ";;" then
      (* Retornamos la concatenacion de las lineas acumuladas sin el ";;" *)
      String.concat " " (List.rev (line::acc))
      |> fun str -> String.sub str 0 (String.length str - 2)  (* Eliminamos el ";;" *)
    else
      loop (line::acc)  (* Acumulamos las lineas *)
  in
    loop []


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      (* Usar read_until_double_semicolon en lugar de read_line *)
      (*let tm = s token (from_string (read_until_double_semicolon ())) in
      let tyTm = typeof ctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " : " ^ string_of_term (eval tm));*)
      let c = s token (from_string (read_until_double_semicolon())) in 
      loop (execute ctx c)
    with
      Lexical_error ->
        print_endline "lexical error";
        loop ctx
      | Parse_error ->
        print_endline "syntax error";
        loop ctx
      | Type_error e ->
        print_endline ("type error: " ^ e);
        loop ctx
      | End_of_file ->
        print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

