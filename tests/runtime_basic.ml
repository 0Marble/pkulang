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
    (interpret [ Builtin (Register 0, [| Number 10 |], "print"); Halt ] 10);
  check string "Print int local" "10\n"
    (interpret
       [
         Alloca 1;
         Assign (Register 0, Number 10);
         Builtin (Register 0, [| Register 0 |], "print");
         Halt;
       ]
       10)

let fib () =
  check string "Fib(20)" "6765\n"
    (interpret
       [
         (* 0: main *)
         Alloca 1;
         Call (Register 0, [| Number 20 |], Static 4);
         Builtin (Register 0, [| Register 0 |], "print");
         Halt;
         (* 4: fib *)
         Alloca 2;
         Sub (Register 0, Argument 0, Number 1);
         Sub (Register 1, Argument 0, Number 2);
         GotoIfNeg (Register 1, Static 12);
         Call (Register 0, [| Register 0 |], Static 4);
         Call (Register 1, [| Register 1 |], Static 4);
         Add (Register 0, Register 0, Register 1);
         Ret (Register 0);
         Ret (Argument 0);
         Trap;
       ]
       150000)

let () =
  run "Runtime"
    [
      ("basic", [ ("nothing", `Quick, nothing); ("print", `Quick, print) ]);
      ("programs", [ ("fib", `Quick, fib) ]);
    ]
