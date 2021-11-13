open Token

type literal =
  | Bool of bool
  | Num of float
  | Str of string
  (* TODO: to be handled later *)
  | Object
  (* This is nil not null, that makes it way better *)
  | Nil
  [@@deriving eq, show]

let tokenToLiteral = function 
| NUMBER n -> Num n
| STRING s -> Str s
| FALSE -> Bool false
| TRUE -> Bool true
| NIL -> Nil
(* TODO: To be handled later *)
| _ -> Object

type expr =
  | Binary of expr * Token.tokenInfo * expr
  | Grouping of expr
  | Literal of literal
  | Unary of Token.tokenInfo * expr
  [@@deriving eq, show]

(* let isFalse *)
exception RuntimeError of tokenInfo * string
exception ExprError of string

let is_truthy = function
| Nil -> false
| Bool b -> b
| _ -> false

let is_num token value f =
  match value with
  | Num n -> f n
  | _ -> raise (RuntimeError (token, "Operand must be a number"))

let both_num token l r f =
  match (l, r) with
  | (Num l, Num r) -> f l r
  | _ -> raise (RuntimeError (token, "Operands must be numbers"))

let is_equal l r =
  match (l, r) with
  | (Nil, Nil) -> true
  | (Num l, Num r) -> l == r
  | (Str l, Str r) -> l == r
  | (Bool l, Bool r) -> l == r
  (* TODO at some point do object compare *)
  | _ -> false

let rec evaluate expr =
  try match expr with
  | Unary (token, expr) -> handleUnary token expr
  | Literal value -> value
  | Binary (left, token, right) -> handleBinary left token right
    (* TODO: handle later *)
  | _ -> Nil
  with
  | RuntimeError (tokenInfo, message) -> 
      Stdio.printf "%s\n[line: %i]" message tokenInfo.line; 
      Nil
and handleUnary token expr = 
  let value = evaluate expr in
  match token.token with
  | BANG -> Bool (not (is_truthy value))
  | MINUS -> is_num token value (fun n -> Num (Float.neg n))
  | _ -> raise (ExprError "You done fucked up")
and handleBinary left token right =
  let left = evaluate left in
  let right = evaluate right in
  match token.token with
  | MINUS -> is_num token left (fun l -> is_num token right (fun r -> Num (Float.sub l r)))
  | PLUS -> (match (left, right) with
    | (Num l, Num r) -> Num (l +. r)
    | (Str l, Str r) -> Str (l ^ r)
    | _ -> raise (RuntimeError (token, "Plus must be used with either a number or string"))
  )
  | SLASH -> both_num token left right (fun l r -> Num (l /. r))
  | STAR -> both_num token left right (fun l r -> Num (l *. r))
  | GREATER -> both_num token left right (fun l r -> Bool (l > r))
  | GREATER_EQUAL-> both_num token left right (fun l r -> Bool (l >= r))
  | LESS-> both_num token left right (fun l r -> Bool (l < r))
  | LESS_EQUAL-> both_num token left right (fun l r -> Bool (l <= r))

  | BANG_EQUAL -> Bool (not (is_equal left right))
  | EQUAL_EQUAL -> Bool (is_equal left right)

  | _ -> raise (ExprError "You done fucked up")


let ti token = { token; lexeme = "..."; line = 0 }

let%test "Unary negative" =
  let literal = evaluate (Unary (ti Token.MINUS, Literal (Num 5.))) in
  equal_literal literal (Num (-5.))

let%test "Binary number add" =
  let literal = evaluate (Binary (Literal (Num 5.), ti PLUS, Literal (Num 5.))) in
  equal_literal literal (Num 10.0)

let%test "Binary string add" =
  let literal = evaluate (Binary (Literal (Str "Hello "), ti PLUS, Literal (Str "World"))) in
  equal_literal literal (Str "Hello World")

