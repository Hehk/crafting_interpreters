open Expr
let rec print = function
  | Binary (left, token, right) ->
    "( " ^ (print left) ^ " " ^(Token.show_token token) ^ " " ^ (print right) ^ " )"
  | Grouping expr -> "( group " ^ (print expr) ^ " )"
  | Literal l -> Expr.show_literal l
  | Unary (token, right) -> "( " ^ (Token.show_token token) ^ " " ^ (print right) ^ " )"

let%test "AstPrinter the book example" =
  let output = print (Binary
    (Unary (Token.MINUS, Literal(Expr.Num(123.)))
    , Token.STAR
    , Grouping (Literal (Expr.Num(45.67))))) in
  (* Slightly different because of the nature of show_token but it is in spirit the same *)
  String.equal output "( ( Token.MINUS (Expr.Num 123.) ) Token.STAR ( group (Expr.Num 45.67) ) )"

(* TODO finish the reverse polish printer *)
let rec reverse_polish_printer = function
  | Binary (left, token, right) ->
    (reverse_polish_printer left) ^ " " ^ (reverse_polish_printer right) ^ " " ^ (Token.show_token token)

    (* Not super sure about how this should look... *)
  | Grouping expr -> (reverse_polish_printer expr)
  | Literal l -> show_literal l
  | Unary (token, right) ->  (Token.show_token token) ^ " " ^ (reverse_polish_printer right)

let%test "AstPrinter in reverse polish" =
  let output = reverse_polish_printer (Binary 
    ( Binary (Literal (Num 1.), Token.PLUS, Literal (Num 2.))
    , Token.STAR
    , Binary (Literal (Num 4.), Token.PLUS, Literal (Num 3.))
  )) in
  (* Slightly different because of the nature of show_token but it is in spirit the same *)
  String.equal output "(Expr.Num 1.) (Expr.Num 2.) Token.PLUS (Expr.Num 4.) (Expr.Num 3.) Token.PLUS Token.STAR"


 