open Pkulang
module CharMap = Map.Make (Char)

let parse_test file_path =
  let parse_test' file_path =
    let chan = In_channel.open_text file_path in
    let src = In_channel.input_all chan in
    let stdin = ref "" in
    let stdout = ref "" in
    let source = ref "" in
    let escape_map =
      CharMap.of_list [ ('n', '\n'); ('t', '\t'); ('\\', '\\'); ('"', '"') ]
    in
    let unescape s =
      let s = List.fold_left ( ^ ) "" s |> String.trim in
      assert (String.starts_with ~prefix:"\"" s);
      assert (String.ends_with ~suffix:"\"" s);
      let len = String.length s in
      assert (len >= 2);
      let s = String.sub s 1 (len - 2) in
      String.fold_left
        (fun (acc, state) c ->
          match (c, state) with
          | '\\', 0 -> (acc, 1)
          | _, 0 -> (Printf.sprintf "%s%c" acc c, 0)
          | _, 1 -> (
              match CharMap.find_opt c escape_map with
              | Some c -> (Printf.sprintf "%s%c" acc c, 0)
              | None -> failwith "invalid escape sequence")
          | _ -> failwith "unreachable")
        ("", 0) s
      |> fst
    in
    String.split_on_char '\n' src
    |> List.iteri (fun i line ->
           let trimmed = String.trim line in
           try
             match String.split_on_char ':' trimmed with
             | "% stdin" :: s -> stdin := !stdin ^ unescape s
             | "% stdout" :: s -> stdout := !stdout ^ unescape s
             | _ -> source := !source ^ "\n" ^ line
           with err ->
             Printf.eprintf "[%s:%d] ERROR: %s\n" file_path i
               (Printexc.to_string err);
             Printexc.print_backtrace stderr);
    (file_path, !source, !stdout, !stdin)
  in
  if Filename.extension file_path <> ".co" then None
  else Some (parse_test' file_path)

let compile_and_run src stdin : string =
  let stdout = ref "" in
  let stdin = String.split_on_char '\n' stdin |> ref in
  let root = Parser.parse_root src in
  let _ = failwith "Todo: implement symbol table" in
  let _ = failwith "Todo: implement type checking" in
  let r =
    Codegen.codegen src (failwith "Todo: fn list")
      (failwith "Todo: get_definiton")
      root
      (fun () ->
        match !stdin with
        | l :: ls ->
            stdin := ls;
            Some l
        | [] -> None)
      (fun s -> stdout := !stdout ^ s)
  in
  let rec complete r =
    if Runtime.finished r then r else Runtime.step r |> complete
  in
  let _ = complete r in
  !stdout

let () =
  Printexc.record_backtrace true;
  let tests =
    Sys.readdir "./examples" |> Array.to_list
    |> List.fast_sort String.compare
    |> List.map (Filename.concat "./examples/")
    |> List.filter_map parse_test
  in
  let failed_tests =
    tests
    |> List.filter_map (fun (file, source, expected, stdin) : string option ->
           try
             Printf.eprintf "stdin: \"%s\"\n" (String.escaped stdin);
             Printf.eprintf "stdout: \"%s\"\n" (String.escaped expected);

             let stdout = compile_and_run source stdin in

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
