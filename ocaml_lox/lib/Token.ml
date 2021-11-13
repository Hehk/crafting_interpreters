type token = 
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR

  (* One or two character tokens *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL

  (* literals *)
  | IDENTIFIER of string
  | STRING of string
  | NUMBER of float

  (* Keywords *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE

  | EOF
  (* TODO remove this once error handling is added properly *)
  | ERROR
  [@@deriving eq, show]

type tokenInfo = {
  token: token;
  lexeme: string;
  line: int    
} [@@deriving eq, show]

let getKeyword = function
| "and" -> Some AND
| "class" -> Some CLASS
| "else" -> Some ELSE
| "false" -> Some FALSE
| "fun" -> Some FUN
| "for"-> Some FOR
| "if"-> Some IF
| "nil"-> Some NIL
| "or"-> Some OR
| "print"-> Some PRINT
| "return" -> Some RETURN
| "super" -> Some SUPER
| "this" -> Some THIS
| "true" -> Some TRUE
| "var" -> Some VAR
| "while" -> Some WHILE
| _ -> None

let fakeTi token = { token; lexeme="test"; line=0}
