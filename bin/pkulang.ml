open Pkulang

let () =
  while true do
    try
      let () = Printf.printf "> " in
      let src = read_line () in
      let toks = Tokenizer.tokenize src in
      let _, root = Parser.parse_expr { src; toks } in
      print_newline ();
      Ast.node_to_str root |> print_endline
    with _ -> ()
  done
