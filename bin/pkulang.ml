let usage_msg = "pkulang <file_name>"
let file_name = ref None

let set_file_name x = 
    file_name := Some x

let () = Arg.parse [] set_file_name usage_msg;

let file_name = match !file_name with
| None -> failwith "No input file given!"
| Some x -> x 
in

Printf.printf "Hello World!\n";
Printf.printf "Input file: %s\n" file_name
