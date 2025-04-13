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

let array_print () =
  check string "print empty" "[]\n"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         Resize (Register 0, Number 0);
         Builtin ([| Register 0 |], "print");
         Halt;
       ]
       10);
  check string "print one invalid" "[?]\n"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         Resize (Register 0, Number 1);
         Builtin ([| Register 0 |], "print");
         Halt;
       ]
       10);
  check string "print one" "[10]\n"
    (interpret
       [
         Alloca 2;
         New (Register 0);
         Store (Register 0, Number 10);
         New (Register 1);
         Resize (Register 1, Number 1);
         IndexSet (Register 1, Number 0, Register 0);
         Builtin ([| Register 1 |], "print");
         Halt;
       ]
       10);
  check string "print many" "[10,20,30]\n"
    (interpret
       [
         Alloca 2;
         New (Register 1);
         Resize (Register 1, Number 3);
         New (Register 0);
         Store (Register 0, Number 10);
         IndexSet (Register 1, Number 0, Register 0);
         New (Register 0);
         Store (Register 0, Number 20);
         IndexSet (Register 1, Number 1, Register 0);
         New (Register 0);
         Store (Register 0, Number 30);
         IndexSet (Register 1, Number 2, Register 0);
         Builtin ([| Register 1 |], "print");
         Halt;
       ]
       20)

let range () =
  check string "range(0, 10)" "[0,1,2,3,4,5,6,7,8,9]\n"
    (interpret
       [
         (* 0 fn main() void *)
         Alloca 1;
         Call (Register 0, [| Number 0; Number 10 |], Static 4);
         Builtin ([| Register 0 |], "print");
         Halt;
         (* 4: fn range(min: int, max: int) ptr *)
         Alloca 5;
         (* r0: length of the array *)
         Sub (Register 0, Argument 1, Argument 0);
         (* r1: array *)
         New (Register 1);
         Resize (Register 1, Register 0);
         Assign (Register 2, Number 0);
         (* for r2 = 0; r2 < r1; r2 = r2 + 1*)
         (* 9: *)
         Sub (Register 3, Register 0, Register 2);
         GotoIfZero (Register 3, Static 16);
         New (Register 4);
         Store (Register 4, Register 2);
         IndexSet (Register 1, Register 2, Register 4);
         Add (Register 2, Register 2, Number 1);
         Goto (Static 9);
         (* 16: *)
         Ret (Register 1);
       ]
       10000)

let () =
  run "Runtime: arrays"
    [
      ("basic", [ ("print", `Quick, array_print) ]);
      ("programs", [ ("range", `Quick, range) ]);
    ]
