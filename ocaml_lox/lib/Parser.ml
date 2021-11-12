open Base

(*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*)

type state = {
  tokens: Token.token list;
  current: int;
}

exception ParseError of string

let is state token =
  match List.nth state.tokens state.current with
  | None -> false
  | Some t -> Token.equal_token t token

(* let advance state =  *)

let genericError () = raise (ParseError "Failed to find an expression")

let rec primary tokens =
  let open Token in
  match tokens with
  (* TODO add specific error message *)
  | [] -> genericError ()
  | token :: tl -> match token with
    | FALSE | TRUE | NIL | NUMBER _ | STRING _ -> 
      (Expr.Literal (Expr.tokenToLiteral token), tl)
    | LEFT_PAREN -> (
      let (expr, tokens) = expression tl in
      match tokens with
      | RIGHT_PAREN :: tl -> (Expr.Grouping expr, tl)
      (* TODO add error, "Expect ')' after expression" *)
      | _ -> genericError ()
    )
    (* TODO add specific error message *)
    | _ -> genericError ()
and unary = function
| [] -> genericError ()
| hd :: tl -> match hd with
  | Token.BANG | Token.MINUS ->  
    let (rightExpr, tokens) = unary tl in
    (Expr.Unary(hd, rightExpr), tokens)
  | _ -> primary (hd :: tl)
and factor tokens =
  let rec nested expr = function
  | [] -> (expr, [])
  | hd::tl -> (match hd with
    | Token.SLASH
    | Token.STAR -> 
      let (right, tl) = unary tl in
      let (left, tl) = nested expr tl in
      (Expr.Binary (left, hd, right), tl)
    | _ -> (expr, hd::tl))
    in
  let (expr, tl) = unary tokens in
  nested expr tl
and term tokens =
  let rec nested expr = function
  | [] -> (expr, [])
  | hd::tl -> (match hd with
    | Token.MINUS
    | Token.PLUS -> 
      let (right, tl) = factor tl in
      let (left, tl) = nested expr tl in
      (Expr.Binary (left, hd, right), tl)
    | _ -> (expr, hd::tl))
    in
  let (expr, tl) = factor tokens in
  nested expr tl
and comparison tokens =
  let rec nested expr = function
  | [] -> (expr, [])
  | hd::tl -> (match hd with
    | Token.GREATER
    | Token.GREATER_EQUAL
    | Token.LESS
    | Token.LESS_EQUAL ->
      let (right, tl) = term tl in
      let (left, tl) = nested expr tl in
      (Expr.Binary (left, hd, right), tl)
    | _ -> (expr, hd::tl))
    in
  let (expr, tl) = term tokens in
  nested expr tl
and equality tokens =
  let rec nested expr = function
  | [] -> (expr, [])
  | hd::tl -> (match hd with
    | Token.EQUAL
    | Token.EQUAL_EQUAL ->
      let (right, tl) = comparison tl in
      let (left, tl) = nested expr tl in
      (Expr.Binary (left, hd, right), tl)
    | _ -> (expr, hd::tl))
    in
  let (expr, tl) = comparison tokens in
  nested expr tl

and expression tokens = equality tokens

let getExpr (expr, _tokens) = expr
let run tokens = 
  try getExpr (expression tokens) with e ->
    let msg = Exn.to_string e
    and stack = Backtrace.to_string (Backtrace.get ()) in
      Stdio.eprintf "there was an error: %s%s\n" msg stack;
      raise e

let%test "initial parse" =
  let open Token in
  let expr = run [NUMBER 5.; STAR; NUMBER 4.; EQUAL_EQUAL; NUMBER 2.; PLUS; NUMBER 18.;] in
  Expr.equal_expr expr (Expr.Binary (
   (Expr.Binary ((Expr.Literal (Expr.Num 5.)), Token.STAR,
      (Expr.Literal (Expr.Num 4.)))),
   Token.EQUAL_EQUAL,
   (Expr.Binary ((Expr.Literal (Expr.Num 2.)), Token.PLUS,
      (Expr.Literal (Expr.Num 18.))))
   ))
