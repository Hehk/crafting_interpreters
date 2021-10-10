open Token
open Base

type state = {
  tokens: tokenInfo list;

  start: int;
  current: int;
  line: int;
}

module Helpers = struct
  let addToken state source token =
    let lexeme = String.sub ~pos:state.start ~len:(state.current - state.start) source in
    {
      tokens = { token = token; line = state.line; lexeme = lexeme } :: state.tokens;
      start = state.start + String.length(lexeme);
      current = state.start + String.length(lexeme);
      line = state.line;
    }
  
  let matchChar state source c =
    let nextChar = String.get source state.current in
    phys_equal nextChar c

  (* Advance the current location of the scanner *)
  let advance state = { tokens= state.tokens; line = state.line; current = state.current + 1; start = state.start }

  let initialState = { tokens = []; start = 0; current = 0; line = 0}
end

let scanToken state source =
  let c = String.get source state.current in
  let newState = Helpers.advance state in
  let addSimpleToken = Helpers.addToken newState source in
  match c with
  | '(' -> addSimpleToken LEFT_PAREN 
  | ')' -> addSimpleToken RIGHT_PAREN
  | '{' -> addSimpleToken LEFT_BRACE
  | '}' -> addSimpleToken RIGHT_BRACE
  | ',' -> addSimpleToken COMMA
  | '.' -> addSimpleToken DOT
  | '-' -> addSimpleToken MINUS
  | '+' -> addSimpleToken PLUS
  | ';' -> addSimpleToken SEMICOLON
  | '*' -> addSimpleToken STAR

  | '!' -> if Helpers.matchChar newState source '=' then
      let newState = Helpers.advance newState in
      Helpers.addToken newState source BANG_EQUAL
    else
      addSimpleToken BANG
  | '=' -> 
    if Helpers.matchChar newState source '=' then
      let newState = Helpers.advance newState in
      Helpers.addToken newState source EQUAL_EQUAL
    else
      addSimpleToken EQUAL
  | '<' -> if Helpers.matchChar newState source '=' then
      let newState = Helpers.advance newState in
      Helpers.addToken newState source LESS_EQUAL
    else
      addSimpleToken LESS
  | '>' -> if Helpers.matchChar newState source '=' then
      let newState = Helpers.advance newState in
      Helpers.addToken newState source GREATER_EQUAL
    else
      addSimpleToken GREATER


  


  (* TODO fill this out *)
  | _ -> addSimpleToken EOF

let rec scanTokens currentState source = 
  if currentState.current >= String.length source then
    let eof = { token = EOF; lexeme = ""; line = currentState.line } in
    List.rev (eof :: currentState.tokens)
  else
    (* TODO finish this *)
    let nextState = scanToken currentState source in
    scanTokens nextState source

let%test "scanTokens (" =
  let source = "(" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal phys_equal tokenList [LEFT_PAREN; EOF]

let%test "scanTokens (){},.-+;* all the single char tokens" =
  let source = "(){},.-+;*" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal phys_equal tokenList [LEFT_PAREN; RIGHT_PAREN; LEFT_BRACE; RIGHT_BRACE; COMMA; DOT; MINUS; PLUS; SEMICOLON; STAR; EOF]

let%test "scanTokens >>=" =
  let source = ">>=" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal phys_equal tokenList [GREATER; GREATER_EQUAL; EOF]

let run = scanTokens Helpers.initialState


