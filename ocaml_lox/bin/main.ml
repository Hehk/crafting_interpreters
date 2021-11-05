open Core
let file_param =
  let open Command.Param in
  anon (maybe ("filename" %: string))


let rec run_repl () =
  print_endline "Welcome to the lox repl! Type away!";
  (* printf "\n> "; *)
  match In_channel.(input_line stdin) with
  | None -> run_repl ()
  | Some "exit" -> ()
  | Some input ->
    if phys_equal (String.strip input) "" then
      run_repl ()
    else 
      input
      |> Lox.Scanner.run
      |> List.map ~f:(fun (t: Lox.Token.tokenInfo) -> t.token)
      |> Lox.Parser.run
      |> Lox.Expr.evaluate
      |> Lox.Expr.show_literal
      |> Stdio.printf "Result: %s"

let run filename =
  match filename with
  | None -> run_repl ()
  | Some filename -> Stdio.printf "File %s" filename

let command =
  Command.basic
    ~summary:"Lox language"
    (Command.Param.map file_param ~f:(fun filename -> (fun () -> run filename)))

let () = Command.run command
