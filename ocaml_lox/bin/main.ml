open Core

let file_param =
  let open Command.Param in
  anon (maybe ("filename" %: string))

let runCode code =
  code |> Lox.Scanner.run
  |> Lox.Parser.run |> Lox.Expr.evaluate |> Lox.Expr.show_literal
  |> Stdio.printf "Result: %s"

let rec run_repl () =
  print_endline "Welcome to the lox repl! Type away!";
  (* printf "\n> "; *)
  match In_channel.(input_line stdin) with
  | None -> run_repl ()
  | Some "exit" -> ()
  | Some input ->
      if phys_equal (String.strip input) "" then run_repl () else runCode input;
      run_repl ()

let run filename =
  match filename with
  | None -> run_repl ()
  | Some filename ->
      let lines = In_channel.read_lines filename in
      let code = List.fold ~init:"" ~f:(fun acc s -> acc ^ "\n" ^ s) lines in
      let (() : unit) = printf "CODE: %s\n" code in
      runCode code

let command =
  Command.basic ~summary:"Lox language"
    (Command.Param.map file_param ~f:(fun filename () -> run filename))

let () = Command.run command
