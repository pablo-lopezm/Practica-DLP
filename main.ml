open Parsing;;
open Lexing;;

open Lambda;;
open Execute;;
open Parser;;
open Lexer;;

(* 
  Reads multiple lines of input from the user until a line ends with ";;".
  Once ";;" is encountered, it returns the accumulated lines as a single string,
  with the trailing ";;" removed. Each line is concatenated with a space between them.
  This function is useful for reading multi-line expressions or commands in interactive sessions.
*)

let read_until_double_semicolon () =
  let rec loop acc =
    let line = read_line () in
    if String.length line >= 2 && String.sub line (String.length line - 2) 2 = ";;" then
      String.concat " " (List.rev (line::acc))
      |> fun str -> String.sub str 0 (String.length str - 2)  (* Delete ";;" *)
    else
      loop (line::acc)
  in
    loop []


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
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

