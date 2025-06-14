open Pkulang

let find_test_files dir =
  let files_in_dir = Sys.readdir dir in
  let get_test files file =
    let expected_name = file ^ ".expected" in
    let stdin_name = file ^ ".stdin" in
    let expected =
      Array.find_opt (fun x -> x = expected_name) files
      |> Option.map (fun x -> Filename.concat dir x)
    in
    let stdin =
      Array.find_opt (fun x -> x = stdin_name) files
      |> Option.map (fun x -> Filename.concat dir x)
    in
    (Filename.concat dir file, expected, stdin)
  in
  Array.to_list files_in_dir
  |> List.filter_map (fun file ->
         if
           Filename.extension file = ".expected"
           || Filename.extension file = ".stdin"
         then None
         else
           let file, expected, stdin = get_test files_in_dir file in
           match expected with
           | Some expected -> Some (file, expected, stdin)
           | None ->
               Printf.eprintf "[WARN] no .expected file for `%s'\n" file;
               None)

let compile_and_run file : string =
  let chan = In_channel.open_text file in
  let src = In_channel.input_all chan in
  let _ = Parser.parse_root src in
  let _ = failwith "Todo: implement symbol table" in
  let _ = failwith "Todo: implement type checking" in
  let _ = failwith "Todo: implement codegen" in
  let _ = failwith "Todo: run the generated code" in
  failwith "Todo: return back stdout"

let () =
  Printexc.record_backtrace true;
  let tests = find_test_files "./examples" in
  let failed_tests =
    tests
    |> List.filter_map (fun (file, expected, _) : string option ->
           try
             let stdout = compile_and_run file in
             let chan = In_channel.open_text expected in
             let expected = In_channel.input_all chan in

             if stdout <> expected then (
               let stdout = String.escaped stdout in
               let expected = String.escaped expected in
               Printf.eprintf
                 "\027[1;31m[%s]\027[0m: Output doesnt match expected output:\n\
                  Got:\n\n\
                  \t`%s'\n\n\
                  Expected:\n\n\
                  \t`%s'\n\n"
                 file stdout expected;
               failwith "Mismatch")
             else (
               Printf.eprintf "\027[1;32m[%s]\027[0m: Ok!\n" file;
               None)
           with e ->
             Printf.eprintf
               "\027[1;31m[%s]\027[0m: Failed with exception:\n\
                \027[1;37m%s\027[0m\n\
                %s\n"
               file (Printexc.to_string e)
               (Printexc.get_backtrace ());
             Some file)
  in
  let failed_cnt = List.length failed_tests in
  let all_cnt = List.length tests in
  Printf.eprintf "Ran %d tests, %d completed, %d failed\n" all_cnt
    (all_cnt - failed_cnt) failed_cnt;
  if failed_cnt <> 0 then (
    Printf.eprintf "Failed tests:\n";
    List.iter (fun x -> Printf.eprintf "\t\027[0;31m%s\027[0m\n" x) failed_tests;
    ())
  else ()
