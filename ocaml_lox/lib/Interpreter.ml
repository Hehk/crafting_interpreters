open Statement
open Base

(* TODO handle runtime errors *)
let handleStatement environment stat =
  match stat with
  | Expression expr -> snd @@ Expr.evaluate ~environment expr
  | Print expr ->
      let literal, environment = Expr.evaluate ~environment expr in
      let (_ : unit) = literal |> Expr.show_literal |> Stdio.print_endline in
      environment
  | Variable (name, value) ->
      let literal = fst @@ Expr.evaluate ~environment value in
      Environment.add environment name literal

(* TODO add handling for runtime exceptions *)
let run statements =
  let environment = Environment.empty in
  List.fold ~init:environment ~f:handleStatement statements
