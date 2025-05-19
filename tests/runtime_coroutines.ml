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

let simple () =
  check string "simple coroutine" "10\n"
    (interpret
       [
         Alloca 1;
         Create (Register 0, [| Number 10 |], Relative 4);
         Resume (Register 0, Location (Register 0), Static 6);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         Yield (Location (Argument 0));
         Trap;
       ]
       100)

let multiple_yield () =
  check string "Multiple yields" "10\n20\n30\n"
    (interpret
       [
         Alloca 2;
         Create (Register 0, [||], Relative 8);
         Resume (Register 1, Location (Register 0), Static 1000);
         Builtin ([| Location (Register 1) |], "print");
         Resume (Register 1, Location (Register 0), Static 1000);
         Builtin ([| Location (Register 1) |], "print");
         Resume (Register 1, Location (Register 0), Static 1000);
         Builtin ([| Location (Register 1) |], "print");
         Halt;
         Yield (Number 10);
         Yield (Number 20);
         Yield (Number 30);
         Trap;
       ]
       100)

let ordering () =
  check string "Ordering" "10\n20\n30\n"
    (interpret
       [
         Alloca 1;
         Create (Register 0, [||], Relative 5);
         Builtin ([| Number 10 |], "print");
         Resume (Void, Location (Register 0), Static 1000);
         Builtin ([| Number 30 |], "print");
         Halt;
         Builtin ([| Number 20 |], "print");
         Yield Null;
         Trap;
       ]
       100)

let nested () =
  check string "Nested" "10\n20\n30\n"
    (interpret
       [
         Alloca 2;
         Create (Register 0, [||], Relative 7);
         Resume (Register 1, Location (Register 0), Static 1000);
         Builtin ([| Location (Register 1) |], "print");
         Resume (Register 1, Location (Register 0), Static 1000);
         Builtin ([| Location (Register 1) |], "print");
         Builtin ([| Number 30 |], "print");
         Halt;
         Yield (Number 10);
         Call (Void, [||], Relative 3);
         Ret Null;
         Trap;
         Yield (Number 20);
         Call (Void, [||], Relative 3);
         Ret Null;
         Trap;
       ]
       100)

let range () =
  check string "range" "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    (interpret
       [
         Alloca 2;
         Create (Register 0, [| Number 0; Number 10 |], Relative 6);
         Resume (Register 1, Location (Register 0), Static 1000);
         GotoIfNeg (Location (Register 1), Relative 3);
         Builtin ([| Location (Register 1) |], "print");
         Goto (Relative (-3));
         Halt;
         Alloca 2;
         Assign (Register 0, Location (Argument 0));
         Sub (Register 1, Location (Register 0), Location (Argument 1));
         GotoIfNeg (Location (Register 1), Relative 2);
         Yield (Number (-1));
         Yield (Location (Register 0));
         Add (Register 0, Location (Register 0), Number 1);
         Goto (Relative (-5));
         Trap;
       ]
       1000)

let () =
  run "Runtime: Coroutines"
    [
      ( "basic",
        [
          ("simple", `Quick, simple);
          ("multiple_yield", `Quick, multiple_yield);
          ("ordering", `Quick, ordering);
          ("nested", `Quick, nested);
          ("range", `Quick, range);
        ] );
    ]
