open Pkulang

let () =
  while true do
    try
      Printf.printf "> " |> read_line |> Tokenizer.tokenize
      |> Tokenizer.tok_list_to_str |> print_endline
    with
    | Error.ErrorAtLocation e -> Error.print_error e
    | err -> raise err
  done
