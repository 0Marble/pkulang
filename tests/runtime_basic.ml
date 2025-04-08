open Alcotest
open Pkulang

let interpret cmds n =
  let r =
    Runtime.create ""
      (cmds
      |> List.map (fun c : Runtime.command ->
             { cmd = c; loc = Location.Spot 0 })
      |> Array.of_list)
      0
  in
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  (complete r n).stdout

let nothing () = check string "Empty" "" (interpret [ Halt ] 10)

let print () =
  check string "Print int" "10\n"
    (interpret [ Builtin (Local 0, [| Number 10 |], "print_int"); Halt ] 10);
  check string "Print int local" "10\n"
    (interpret
       [
         Alloca 1;
         Assign (Local 0, Number 10);
         Builtin (Local 0, [| Local 0 |], "print_int");
         Halt;
       ]
       10)

let fib () =
  check string "Fib(20)" "6765\n"
    (interpret
       [
         (* 0: main *)
         Alloca 1;
         Call (Local 0, [| Number 20 |], Static 4);
         Builtin (Local 0, [| Local 0 |], "print_int");
         Halt;
         (* 4: fib *)
         Alloca 2;
         GotoIfZero (Local 0, Static 13);
         Sub (Local 1, Local 0, Number 1);
         GotoIfZero (Local 1, Static 14);
         Sub (Local 2, Local 0, Number 2);
         Call (Local 1, [| Local 1 |], Static 4);
         Call (Local 2, [| Local 2 |], Static 4);
         Add (Local 1, Local 1, Local 2);
         Ret (Local 1);
         Ret (Number 0);
         Ret (Number 1);
         Trap;
       ]
       400000)

let () =
  run "Runtime"
    [
      ("basic", [ ("nothing", `Quick, nothing); ("print", `Quick, print) ]);
      ("programs", [ ("fib", `Quick, fib) ]);
    ]
