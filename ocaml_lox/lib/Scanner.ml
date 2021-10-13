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

  let addNoToken state = { state with start = state.current }

  let peek state source = String.get source state.current
  let peekNext state source = String.get source (state.current + 1)
  
  let isAtEnd state source = state.current >= String.length source
  let matchChar state source c =
    let nextChar = peek state source in
    phys_equal nextChar c

  (* Advance the current location of the scanner *)
  let advance state = { state with current = state.current + 1 }

  let initialState = { tokens = []; start = 0; current = 0; line = 0}

  let isDigit c = let open Char in c >= '0' && c <= '9'
  let isAlpha c = let open Char in (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || phys_equal c '_'
  let isAlphaNumeric c = isDigit c || isAlpha c

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

  | '/' -> if Helpers.matchChar newState source '/' then
      let curState = ref (Helpers.advance newState) in
      while not (Helpers.isAtEnd !curState source) && not (phys_equal (Helpers.peek !curState source) '\n') do
        curState := Helpers.advance !curState
      done;
      {
        !curState with 
        start = !curState.current
      }
    else
      addSimpleToken SLASH
  
  | ' '
  | '\r'
  | '\t' -> Helpers.addNoToken newState
  | '\n' -> Helpers.addNoToken {
    newState with
    line = newState.line + 1
  }

  | '"' -> 
    let curState = ref (Helpers.advance newState) in
    let atEnd = fun state -> Helpers.isAtEnd state source in
    while not (atEnd !curState) && not(phys_equal(Helpers.peek !curState source) '"') do 
      let isNewLine = phys_equal (Helpers.peek !curState source) '\n' in
      curState := {
        (Helpers.advance !curState) with
        line = if isNewLine then !curState.line + 1 else !curState.line
      }
    done;

    if atEnd !curState then
      (* TODO add some kind of error logging *)
      addSimpleToken EOF
    else 
      (* move current back one because we hit the end *)
      let current = !curState.current in
      let start = !curState.start in
      let content = String.sub ~pos:(start + 1) ~len:(current - start - 1) source in
      {
        !curState with
        start = current + 1;
        current = current + 1;
        tokens = { token = STRING(content); line = !curState.line; lexeme = content } :: state.tokens;
      }

  (* TODO fill this out *)
  | _ -> 
    if Helpers.isDigit c then
      let curState = ref newState in
      while Helpers.isDigit (Helpers.peek !curState source) do
        curState := Helpers.advance !curState
      done;

      (if phys_equal '.' (Helpers.peek !curState source) && Helpers.isDigit (Helpers.peekNext !curState source) then
        (* consume the . *)
        curState := Helpers.advance !curState;
        while Helpers.isDigit (Helpers.peek !curState source) do
          curState := Helpers.advance !curState
        done;
      );

      let num = String.sub ~pos:!curState.start ~len:(!curState.current - !curState.start) source in
      {
        !curState with
        start = !curState.current;
        tokens = { token = NUMBER(Float.of_string num); line = !curState.line; lexeme = num } :: state.tokens
      }

    else if Helpers.isAlpha c then
      let curState = ref newState in
      while Helpers.isAlphaNumeric (Helpers.peek !curState source) do
        curState := Helpers.advance !curState
      done;
      let text = String.sub ~pos:!curState.start ~len:(!curState.current - !curState.start) source in
      let token = match getKeyword text with
        | Some(k) -> k
        | None -> IDENTIFIER text
      in
      { !curState with start = !curState.current; tokens = { token; line = !curState.line; lexeme = text } :: !curState.tokens }
        

    else
      (* TODO handle error *)
      Helpers.addNoToken newState



let rec scanTokens currentState source = 
  if currentState.current >= String.length source then
    let eof = { token = EOF; lexeme = ""; line = currentState.line } in
    List.rev (eof :: currentState.tokens)
  else
    let nextState = scanToken currentState source in
    scanTokens nextState source

let testScanTokens source expected =
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  let passed = List.equal equal_token tokenList expected in
  if passed then
    passed
  else
    let printTokens = List.map ~f:(fun t -> Stdio.printf "%s " (show_token t)) in
    let _ : unit = Stdio.printf("Actual Tokens:   ") in
    let _ : unit list = printTokens tokenList in
    let _ : unit = Stdio.printf("\nExpected Tokens: ") in
    let _ : unit list = printTokens expected in
    passed

let%test "scanTokens (" =
  let source = "(" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal equal_token tokenList [LEFT_PAREN; EOF]

let%test "scanTokens (){},.-+;* all the single char tokens" =
  let source = "(){},.-+;*" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal equal_token tokenList [LEFT_PAREN; RIGHT_PAREN; LEFT_BRACE; RIGHT_BRACE; COMMA; DOT; MINUS; PLUS; SEMICOLON; STAR; EOF]

let%test "scanTokens >>=" =
  let source = ">>=" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal equal_token tokenList [GREATER; GREATER_EQUAL; EOF]

let%test "scanTokens comment" =
  let source = "//test" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal equal_token tokenList [EOF]

let%test "scanTokens comment with newline" =
  let source = "//test\n==" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal equal_token tokenList [EQUAL_EQUAL; EOF]

let%test "scanTokens string" =
  let source = "\"test\"" in
  let tokenList = List.map ~f:(fun x -> x.token) (scanTokens Helpers.initialState source) in
  List.equal equal_token tokenList [STRING "test"; EOF]

let%test "scanTokens identifiers and keywords" = 
  testScanTokens
    "var xyz123 = true;"
    [VAR; IDENTIFIER "xyz123"; EQUAL; TRUE; SEMICOLON; EOF]


let run = scanTokens Helpers.initialState


