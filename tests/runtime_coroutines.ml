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
         Resume (Register 0, Null, Location (Register 0));
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         Yield (Void, Location (Argument 0));
         Trap;
       ]
       100)

let multiple_yield () =
  check string "Multiple yields" "10\n20\n30\n"
    (interpret
       [
         Alloca 2;
         Create (Register 0, [||], Relative 8);
         Resume (Register 1, Null, Location (Register 0));
         Builtin ([| Location (Register 1) |], "print");
         Resume (Register 1, Null, Location (Register 0));
         Builtin ([| Location (Register 1) |], "print");
         Resume (Register 1, Null, Location (Register 0));
         Builtin ([| Location (Register 1) |], "print");
         Halt;
         Yield (Void, Number 10);
         Yield (Void, Number 20);
         Yield (Void, Number 30);
         Trap;
       ]
       100)

let communication () =
  check string "Communication" "30\n"
    (interpret
       [
         Alloca 1;
         Create (Register 0, [||], Relative 6);
         Resume (Void, Null, Location (Register 0));
         Resume (Void, Number 10, Location (Register 0));
         Resume (Register 0, Number 20, Location (Register 0));
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         Alloca 2;
         Yield (Register 0, Null);
         Yield (Register 1, Null);
         Add (Register 0, Location (Register 0), Location (Register 1));
         Yield (Void, Location (Register 0));
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
         Resume (Void, Null, Location (Register 0));
         Builtin ([| Number 30 |], "print");
         Halt;
         Builtin ([| Number 20 |], "print");
         Yield (Void, Null);
         Trap;
       ]
       100)

let range () =
  check string "range" "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    (interpret
       [
         Alloca 2;
         Create (Register 0, [| Number 0; Number 10 |], Relative 6);
         Resume (Register 1, Null, Location (Register 0));
         GotoIfNeg (Location (Register 1), Relative 3);
         Builtin ([| Location (Register 1) |], "print");
         Goto (Relative (-3));
         Halt;
         Alloca 2;
         Assign (Register 0, Location (Argument 0));
         Sub (Register 1, Location (Register 0), Location (Argument 1));
         GotoIfNeg (Location (Register 1), Relative 2);
         Yield (Void, Number (-1));
         Yield (Void, Location (Register 0));
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
          ("communication", `Quick, communication);
          ("ordering", `Quick, ordering);
          ("range", `Quick, range);
        ] );
    ]
