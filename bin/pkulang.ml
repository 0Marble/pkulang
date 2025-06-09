open Pkulang

let read_input () =
  let rec read_input' acc =
    let line = read_line () in
    if String.ends_with ~suffix:"\\" line then
      read_input'
      @@ Printf.sprintf "%s%s\n" acc
           (String.sub line 0 (String.length line - 1))
    else acc ^ line
  in
  read_input' ""

let do_eval src =
  let root = Parser.parse_root src in
  let symatb = SymbolTableBuilder.build_symbol_table root in
  let rt = Codegen.codegen src root symatb in
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  let _ = complete rt (-1) in
  ()

let () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc = 1 then
    while true do
      try
        let () = Printf.printf "> " in
        let src = read_input () in
        do_eval src
      with _ -> ()
    done
  else if argc = 2 then
    let file_name = argv.(1) in
    let chan = In_channel.open_text file_name in
    In_channel.input_all chan |> do_eval
  else failwith @@ Printf.sprintf "Usage: %s [file-name]" argv.(0)
