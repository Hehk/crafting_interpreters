open Expr
let rec print = function
  | Binary (left, token, right) ->
    "( " ^ (print left) ^ " " ^(Token.show_token token) ^ " " ^ (print right) ^ " )"
  | Grouping expr -> "( group " ^ (print expr) ^ " )"
  | Literal token -> Token.show_token token
  | Unary (token, right) -> "( " ^ (Token.show_token token) ^ " " ^ (print right) ^ " )"

let%test "AstPrinter the book example" =
  let output = print (Binary
    (Unary (Token.MINUS, Literal(Token.NUMBER(123.)))
    , Token.STAR
    , Grouping (Literal (Token.NUMBER(45.67))))) in
  (* Slightly different because of the nature of show_token but it is in spirit the same *)
  output == "( ( Token.MINUS (Token.NUMBER 123.) ) Token.STAR ( group (Token.NUMBER 45.67) ) )"

(* TODO finish the reverse polish printer *)
let rec reverse_polish_printer = function
  | Binary (left, token, right) ->
    (print left) ^ " " ^(Token.show_token token) ^ " " ^ (print right) ^ " )"
  | Grouping expr -> "( group " ^ (print expr) ^ " )"
  | Literal token -> Token.show_token token
  | Unary (token, right) -> "( " ^ (Token.show_token token) ^ " " ^ (print right) ^ " )"
 