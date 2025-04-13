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
    (interpret [ Builtin ([| Number 10 |], "print"); Halt ] 10);
  check string "Print int local" "10\n"
    (interpret
       [
         Alloca 1;
         Assign (Register 0, Number 10);
         Builtin ([| Register 0 |], "print");
         Halt;
       ]
       10)

let null () =
  check string "null" "10\n"
    (interpret
       [
         GotoIfNull (Null, Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       100)

let call () =
  check string "simple call" "10\n"
    (interpret
       [
         Alloca 1;
         Call (Register 0, [| Number 6; Number 4 |], Static 4);
         Builtin ([| Register 0 |], "print");
         Halt;
         Alloca 1;
         Add (Register 0, Argument 0, Argument 1);
         Ret (Register 0);
       ]
       100);
  check string "nested call" "20\n"
    (interpret
       [
         Alloca 1;
         Call (Register 0, [| Number 6; Number 4 |], Static 4);
         Builtin ([| Register 0 |], "print");
         Halt;
         Alloca 1;
         Add (Register 0, Argument 0, Argument 1);
         Call (Register 0, [| Register 0 |], Static 8);
         Ret (Register 0);
         Alloca 1;
         Add (Register 0, Argument 0, Argument 0);
         Ret (Register 0);
       ]
       100)

let goto () =
  check string "goto" "10\n"
    (interpret
       [ Goto (Static 2); Trap; Builtin ([| Number 10 |], "print"); Halt ]
       10);
  check string "goto if zero" "10\n"
    (interpret
       [
         GotoIfZero (Number 0, Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10);
  check string "goto if negative" "10\n"
    (interpret
       [
         GotoIfNeg (Number (-1), Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10);
  check string "goto if null" "10\n"
    (interpret
       [
         GotoIfNull (Null, Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10);
  check string "goto relative" "10\n"
    (interpret
       [
         Nop;
         Nop;
         Goto (Relative 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10);
  check string "goto dynamic" "10\n"
    (interpret
       [
         Alloca 1;
         Add (Register 0, Ip, Number 3);
         Goto (Dynamic 0);
         Trap;
         Builtin ([| Number 10 |], "print");
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
         Builtin ([| Register 0 |], "print");
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
  run "Runtime: basic"
    [
      ( "basic",
        [
          ("nothing", `Quick, nothing);
          ("print", `Quick, print);
          ("call", `Quick, call);
          ("null", `Quick, null);
          ("goto", `Quick, goto);
        ] );
      ("programs", [ ("fib", `Quick, fib) ]);
    ]
