open Alcotest
open Pkulang

let interpret cmds n =
  let stdout = ref "" in
  let r =
    Runtime.create ""
      (cmds
      |> List.map (fun c : Runtime.command ->
             { cmd = c; loc = Location.Spot 0 })
      |> Array.of_list)
      0 0
      (fun () -> failwith "no stdin")
      (fun (s : string) ->
        prerr_string s;
        stdout := !stdout ^ s)
  in
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  let _ = complete r n in
  !stdout

let array_print () =
  check string "print empty" "[]"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         Resize (Register 0, Number 0);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10);
  check string "print one invalid" "[?]"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         Resize (Register 0, Number 1);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10);
  check string "print one" "[10]"
    (interpret
       [
         Alloca 2;
         New (Register 0);
         Store (Register 0, Number 10);
         New (Register 1);
         Resize (Register 1, Number 1);
         IndexSet (Register 1, Number 0, Location (Register 0));
         Builtin ([| Location (Register 1) |], "print");
         Halt;
       ]
       10);
  check string "print many" "[10,20,30]"
    (interpret
       [
         Alloca 2;
         New (Register 1);
         Resize (Register 1, Number 3);
         New (Register 0);
         Store (Register 0, Number 10);
         IndexSet (Register 1, Number 0, Location (Register 0));
         New (Register 0);
         Store (Register 0, Number 20);
         IndexSet (Register 1, Number 1, Location (Register 0));
         New (Register 0);
         Store (Register 0, Number 30);
         IndexSet (Register 1, Number 2, Location (Register 0));
         Builtin ([| Location (Register 1) |], "print");
         Halt;
       ]
       20)

let string_object () =
  check string "Print a string" "foo"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         StringLiteral (Register 0, "foo");
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10);
  check string "Modify string" "foO"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         StringLiteral (Register 0, "foo");
         IndexSet (Register 0, Number 2, Number (int_of_char 'O'));
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10);
  check string "Manual println" "foo\n"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         StringLiteral (Register 0, "foo\n");
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10);
  check string "Get string index"
    (int_of_char 'f' |> string_of_int)
    (interpret
       [
         Alloca 1;
         New (Register 0);
         StringLiteral (Register 0, "foo");
         IndexGet (Register 0, Location (Register 0), Number 0);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10);
  check string "Resize string" "0123456789"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         StringLiteral (Register 0, "0123");
         Resize (Register 0, Number 10);
         IndexSet (Register 0, Number 4, Number (int_of_char '4'));
         IndexSet (Register 0, Number 5, Number (int_of_char '5'));
         IndexSet (Register 0, Number 6, Number (int_of_char '6'));
         IndexSet (Register 0, Number 7, Number (int_of_char '7'));
         IndexSet (Register 0, Number 8, Number (int_of_char '8'));
         IndexSet (Register 0, Number 9, Number (int_of_char '9'));
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       20);
  check string "concat strings" "foobar"
    (interpret
       [
         Alloca 2;
         New (Register 0);
         StringLiteral (Register 0, "foo");
         New (Register 1);
         StringLiteral (Register 1, "bar");
         Add (Register 0, Location (Register 0), Location (Register 1));
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       100);
  ()

let length () =
  check string "length" "10"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         Resize (Register 0, Number 10);
         Size (Register 0, Location (Register 0));
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10)

let inline_ints () =
  check string "array of ints" "[1,2,3]"
    (interpret
       [
         Alloca 1;
         New (Register 0);
         Resize (Register 0, Number 3);
         IndexSet (Register 0, Number 0, Number 1);
         IndexSet (Register 0, Number 1, Number 2);
         IndexSet (Register 0, Number 2, Number 3);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       100)

let range () =
  check string "range(0, 10)" "[0,1,2,3,4,5,6,7,8,9]"
    (interpret
       [
         (* 0 fn main() void *)
         Alloca 1;
         Call (Register 0, [| Number 0; Number 10 |], Static 4);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         (* 4: fn range(min: int, max: int) ptr *)
         Alloca 5;
         (* r0: length of the array *)
         Sub (Register 0, Location (Argument 1), Location (Argument 0));
         (* r1: array *)
         New (Register 1);
         Resize (Register 1, Location (Register 0));
         Assign (Register 2, Number 0);
         (* for r2 = 0; r2 < r1; r2 = r2 + 1*)
         (* 9: *)
         Sub (Register 3, Location (Register 0), Location (Register 2));
         GotoIfZero (Location (Register 3), Static 16);
         New (Register 4);
         Store (Register 4, Location (Register 2));
         IndexSet (Register 1, Location (Register 2), Location (Register 4));
         Add (Register 2, Location (Register 2), Number 1);
         Goto (Static 9);
         (* 16: *)
         Ret (Location (Register 1));
       ]
       10000)

let sort () =
  check string "Sort" "[3,5,6,4,0,7,8,9,1,2][0,1,2,3,4,5,6,7,8,9]"
    (interpret
       [
         (* 0: main() *)
         Alloca 2;
         New (Register 0);
         Resize (Register 0, Number 10);
         New (Register 1);
         Store (Register 1, Number 3);
         IndexSet (Register 0, Number 0, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 5);
         IndexSet (Register 0, Number 1, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 6);
         IndexSet (Register 0, Number 2, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 4);
         IndexSet (Register 0, Number 3, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 0);
         IndexSet (Register 0, Number 4, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 7);
         IndexSet (Register 0, Number 5, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 8);
         IndexSet (Register 0, Number 6, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 9);
         IndexSet (Register 0, Number 7, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 1);
         IndexSet (Register 0, Number 8, Location (Register 1));
         New (Register 1);
         Store (Register 1, Number 2);
         IndexSet (Register 0, Number 9, Location (Register 1));
         Builtin ([| Location (Register 0) |], "print");
         Call
           ( Register 0,
             [| Location (Register 0); Number 0; Number 10 |],
             Relative 3 );
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         (* qsort(arr: ptr, start: int, end: int) ptr *)
         Alloca 2;
         Sub (Register 0, Location (Argument 2), Location (Argument 1));
         (* base case: [] *)
         GotoIfZero (Location (Register 0), Relative 2);
         Goto (Relative 2);
         Ret (Location (Argument 0));
         (* base case: [x] *)
         Sub (Register 0, Location (Register 0), Number 1);
         GotoIfZero (Location (Register 0), Relative 2);
         Goto (Relative 2);
         Ret (Location (Argument 0));
         (* recursion *)
         Call
           ( Register 0,
             [|
               Location (Argument 0);
               Location (Argument 1);
               Location (Argument 2);
             |],
             Relative 5 );
         Call
           ( Register 1,
             [|
               Location (Argument 0);
               Location (Argument 1);
               Location (Register 0);
             |],
             Relative (-10) );
         Add (Register 0, Location (Register 0), Number 1);
         Call
           ( Register 1,
             [|
               Location (Argument 0);
               Location (Register 0);
               Location (Argument 2);
             |],
             Relative (-12) );
         Ret (Location (Argument 0));
         (* partition(arr: ptr, start: int, end: int) int *)
         (* modifies arr from [x, ...] to [..., x, ...], returns new idx of x *)
         Alloca 8;
         Assign (Register 0, Location (Argument 1));
         Assign (Register 1, Location (Argument 1));
         (* for r0=start+1; r0 < end; r0++: *)
         (* if arr[r1] > arr[r0]: swap(arr, r0, r1+1), swap(arr, r1, r1+1), r1++ *)
         Add (Register 0, Location (Register 0), Number 1);
         Sub (Register 2, Location (Register 0), Location (Argument 2));
         GotoIfZero (Location (Register 2), Relative 15);
         IndexGet (Register 2, Location (Argument 0), Location (Register 0));
         IndexGet (Register 3, Location (Argument 0), Location (Register 1));
         Load (Register 4, Location (Register 2));
         Load (Register 5, Location (Register 3));
         Sub (Register 6, Location (Register 5), Location (Register 4));
         GotoIfNeg (Location (Register 6), Relative (-8));
         Add (Register 6, Location (Register 1), Number 1);
         IndexGet (Register 6, Location (Argument 0), Location (Register 6));
         Load (Register 7, Location (Register 6));
         (* r2=&arr[r0], r3=&arr[r1], r6=&arr[r1+1] *)
         (* r4=arr[r0], r5=arr[r1], r7=arr[r1+1] *)
         Store (Register 2, Location (Register 7));
         Store (Register 3, Location (Register 4));
         Store (Register 6, Location (Register 5));
         Add (Register 1, Location (Register 1), Number 1);
         Goto (Relative (-16));
         Ret (Location (Register 1));
       ]
       100000)

let () =
  run "Runtime: arrays"
    [
      ( "basic",
        [
          ("print", `Quick, array_print);
          ("length", `Quick, length);
          ("inline_ints", `Quick, inline_ints);
          ("string_object", `Quick, string_object);
        ] );
      ("programs", [ ("range", `Quick, range); ("qsort", `Quick, sort) ]);
    ]
