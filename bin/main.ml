open Lambda_cube

let read_all_strings filename =
  let file = open_in filename in
  let s = really_input_string file (in_channel_length file) in
  s

let run code =
  let scanner = Scanner.make_scanner code in
  let tokens = Scanner.scan_tokens scanner in
  let state: Parser.state = {tokens = Array.of_list tokens; cur = 0} in
  let expr = Parser.parse_exp state in
  expr |> Ast.show_exp |> print_endline

let run_file filename =
  read_all_strings filename |> run

let rec repl () =
  print_char '>';
  try
    run (read_line());
    repl()
  with End_of_file -> ()

let run_prompt () =
  repl()


let () =
  let len = Array.length Sys.argv in
  if len > 2
  then (
    print_endline "Usage: lox [script]";
    exit(64);
  )
  else if len = 2
  then run_file Sys.argv.(1)
  else if len = 1
  then run_prompt ()
