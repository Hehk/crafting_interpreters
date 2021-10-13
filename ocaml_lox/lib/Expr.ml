type expr =
  | Binary of expr * Token.token * expr
  | Grouping of expr
  | Literal of Token.token
  | Unary of Token.token * expr