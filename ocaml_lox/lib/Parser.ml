open Base
open Token

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
  | token :: tl -> match token.token with
    | FALSE | TRUE | NIL | NUMBER _ | STRING _ -> 
      (Expr.Literal (Expr.tokenToLiteral token.token), tl)
    | LEFT_PAREN -> (
      let (expr, tokens) = expression tl in
      match tokens with
      | { token= RIGHT_PAREN; _ } :: tl -> (Expr.Grouping expr, tl)
      (* TODO add error, "Expect ')' after expression" *)
      | _ -> genericError ()
    )
    (* TODO add specific error message *)
    | _ -> genericError ()
and unary = function
| [] -> genericError ()
| hd :: tl -> match hd.token with
  | Token.BANG | Token.MINUS ->  
    let (rightExpr, tokens) = unary tl in
    (Expr.Unary(hd, rightExpr), tokens)
  | _ -> primary (hd :: tl)
and factor tokens =
  let rec nested expr = function
  | [] -> (expr, [])
  | hd::tl -> (match hd.token with
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
  | hd::tl -> (match hd.token with
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
  | hd::tl -> (match hd.token with
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
  | hd::tl -> (match hd.token with
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

let createStatement tokens =
  let (statement, tokens) = match tokens with
  | { token = Token.PRINT; _ } :: tokens ->
    let (expr, tl) = expression tokens in
    (Statement.Print expr, tl)
  | { token = Token.VAR; _ } :: {token = Token.IDENTIFIER name; _ } :: { token = Token.EQUAL; _ } :: tokens ->
    let (expr, tl) = expression tokens in
    (Statement.Variable (name, expr), tl)
  | tokens -> 
    let (expr, tl) = expression tokens in
    (Statement.Expression expr, tl)
  in

  let tokens = match tokens with
  | { token = Token.SEMICOLON; _ } :: tl -> tl
  | _ -> raise (ParseError "Statements must end with a semicolon") in
  (Some statement, tokens)

(* Step through the code until there is a point where things might be continuable... *)
let rec syncronize tokens =
  match tokens with
  | { token = Token.SEMICOLON; _ } :: tl -> tl
  | hd :: tl -> (match hd.token with
    | CLASS
    | FUN
    | VAR
    | FOR
    | IF
    | WHILE
    | PRINT
    | RETURN -> tokens
    | _ -> syncronize tl)
  | [] -> []

let noError (statement, tokens) = (statement, tokens, None)
let add x xs = match x with
| Some x -> x :: xs
| None -> xs

let rec run ?(statements = []) ?(errors = []) tokens =
  let (statement, tokens, error) = try noError @@ createStatement tokens with
    ParseError message -> 
      (None, syncronize tokens, Some(message))
  in
  let statements = add statement statements in
  let errors = add error errors in
  if phys_equal (List.length tokens) 0 then
    (List.rev statements, List.rev errors)
  else
    run ~statements ~errors tokens

(* let run tokens = *) 
(*   try getExpr (expression tokens) with e -> *)
(*     let msg = Exn.to_string e *)
(*     and stack = Backtrace.to_string (Backtrace.get ()) in *)
(*       Stdio.eprintf "there was an error: %s%s\n" msg stack; *)
(*       raise e *)


let%test "initial parse" =
  let (statements, _) = run @@ List.map ~f:fakeTi [NUMBER 5.; STAR; NUMBER 4.; EQUAL_EQUAL; NUMBER 2.; PLUS; NUMBER
  18.; SEMICOLON] in
  let statement = List.hd_exn statements in
  Statement.equal_statement statement (Statement.Expression (Expr.Binary (
   (Expr.Binary ((Expr.Literal (Expr.Num 5.)), fakeTi Token.STAR,
      (Expr.Literal (Expr.Num 4.)))),
   fakeTi EQUAL_EQUAL,
   (Expr.Binary ((Expr.Literal (Expr.Num 2.)), fakeTi PLUS,
      (Expr.Literal (Expr.Num 18.))))
   )))

let%test "synchronize, recover after semicolor" =
  let (statements, _) = run @@ List.map ~f:fakeTi [STAR; STAR; STAR; SEMICOLON; NUMBER 5.; SEMICOLON] in
  let statement = List.hd_exn statements in
  Statement.equal_statement statement (Statement.Expression (Expr.Literal (Expr.Num 5.)))

let%test "synchronize, recover on var" =
  let (statements, _) = run @@ List.map ~f:fakeTi [STAR; STAR; STAR; VAR; IDENTIFIER "nice"; EQUAL; NUMBER 69.; SEMICOLON] in
  let statement = List.hd_exn statements in
  Statement.equal_statement statement (Statement.Variable ("nice", (Expr.Literal (Expr.Num 69.))
  ))

