type state = {
  tokens: Token.t list;

  start: int;
  current: int;
  line: int;
}

let runInternal source state =
  state.tokens

let run source =
  runInternal source { tokens = []; start = 0; current = 0; line = 0}

let scanToken 

