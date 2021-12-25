type statement =
  | Expression of Expr.expr
  | Print of Expr.expr
  | Variable of string * Expr.expr
  [@@deriving eq, show]

