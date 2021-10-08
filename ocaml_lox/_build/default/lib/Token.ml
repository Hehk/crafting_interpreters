type token = 
  | LeftParen

type t = {
  token: token;
  lexeme: string;
  literal: string;
  line: int    
}

