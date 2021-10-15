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
  String.equal output "( ( Token.MINUS (Token.NUMBER 123.) ) Token.STAR ( group (Token.NUMBER 45.67) ) )"

(* TODO finish the reverse polish printer *)
let rec reverse_polish_printer = function
  | Binary (left, token, right) ->
    (reverse_polish_printer left) ^ " " ^ (reverse_polish_printer right) ^ " " ^ (Token.show_token token)

    (* Not super sure about how this should look... *)
  | Grouping expr -> (reverse_polish_printer expr)
  | Literal token -> Token.show_token token
  | Unary (token, right) ->  (Token.show_token token) ^ " " ^ (reverse_polish_printer right)

let%test "AstPrinter in reverse polish" =
  let output = reverse_polish_printer (Binary 
    ( Binary (Literal (Token.NUMBER 1.), Token.PLUS, Literal (Token.NUMBER 2.))
    , Token.STAR
    , Binary (Literal (Token.NUMBER 4.), Token.PLUS, Literal (Token.NUMBER 3.))
  )) in
  (* Slightly different because of the nature of show_token but it is in spirit the same *)
  String.equal output "(Token.NUMBER 1.) (Token.NUMBER 2.) Token.PLUS (Token.NUMBER 4.) (Token.NUMBER 3.) Token.PLUS Token.STAR"


 