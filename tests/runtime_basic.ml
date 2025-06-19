open Alcotest
open Pkulang

let interpret ?(globals_cnt = 0) ?(stdin = None) cmds n =
  let stdout = ref "" in
  let stdin = Option.map (String.split_on_char '\n') stdin |> ref in
  let stdin () =
    match !stdin with
    | Some (l :: ls) ->
        stdin := Some ls;
        Some l
    | _ -> None
  in
  let r =
    Runtime.create ""
      (cmds
      |> List.map (fun c : Runtime.command ->
             { cmd = c; loc = Location.Spot 0 })
      |> Array.of_list)
      0 globals_cnt stdin
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

let nothing () = check string "Empty" "" (interpret [ Halt ] 10)

let print () =
  check string "Print int" "10"
    (interpret [ Builtin ([| Number 10 |], "print"); Halt ] 10);
  check string "Print int local" "10"
    (interpret
       [
         Alloca 1;
         Assign (Register 0, Number 10);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       10)

let read_line () =
  check string "scan line" "10\n"
    (interpret ~stdin:(Some "10")
       [
         Alloca 1;
         New (Register 0);
         Builtin ([| Location (Register 0) |], "read_line");
         Builtin ([| Location (Register 0) |], "println");
         Halt;
       ]
       10);
  check string "read all lines" "10\n20\n30\n"
    (interpret ~stdin:(Some "10\n20\n30")
       [
         Alloca 1;
         New (Register 0);
         Builtin ([| Location (Register 0) |], "read_line");
         GotoIfZero (Location (Register 0), Relative 3);
         Builtin ([| Location (Register 0) |], "println");
         Goto (Relative (-3));
         Halt;
       ]
       100);
  ()

let null () =
  check string "null" "10"
    (interpret
       [
         GotoIfZero (Null, Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       100)

let call () =
  check string "simple call" "10"
    (interpret
       [
         Alloca 1;
         Call (Register 0, [| Number 6; Number 4 |], Static 4);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         Alloca 1;
         Add (Register 0, Location (Argument 0), Location (Argument 1));
         Ret (Location (Register 0));
       ]
       100);
  check string "nested call" "20"
    (interpret
       [
         Alloca 1;
         Call (Register 0, [| Number 6; Number 4 |], Static 4);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         Alloca 1;
         Add (Register 0, Location (Argument 0), Location (Argument 1));
         Call (Register 0, [| Location (Register 0) |], Static 8);
         Ret (Location (Register 0));
         Alloca 1;
         Add (Register 0, Location (Argument 0), Location (Argument 0));
         Ret (Location (Register 0));
       ]
       100)

let goto () =
  check string "goto" "10"
    (interpret
       [ Goto (Static 2); Trap; Builtin ([| Number 10 |], "print"); Halt ]
       10);
  check string "goto if zero" "10"
    (interpret
       [
         GotoIfZero (Number 0, Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10);
  check string "goto if negative" "10"
    (interpret
       [
         GotoIfNeg (Number (-1), Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10);
  check string "goto if null" "10"
    (interpret
       [
         GotoIfZero (Null, Static 2);
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10);
  check string "goto relative" "10"
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
  check string "goto dynamic" "10"
    (interpret
       [
         Alloca 1;
         Add (Register 0, Ip, Number 3);
         Goto (Dynamic (Register 0));
         Trap;
         Builtin ([| Number 10 |], "print");
         Halt;
       ]
       10)

let globals () =
  check string "Globals" "10, 20\n10, 20\n30, 40\n"
    (interpret
       [
         Assign (Global 0, Number 10);
         Assign (Global 1, Number 20);
         Builtin ([| Location (Global 0); Location (Global 1) |], "println");
         Call (Void, [||], Relative 3);
         Builtin ([| Location (Global 0); Location (Global 1) |], "println");
         Halt;
         Builtin ([| Location (Global 0); Location (Global 1) |], "println");
         Assign (Global 0, Number 30);
         Assign (Global 1, Number 40);
         Ret Null;
       ]
       100 ~globals_cnt:2);
  check string "Globals in coroutines" "10, 20\n10, 20\n30, 40\n"
    (interpret
       [
         Alloca 1;
         Assign (Global 0, Number 10);
         Assign (Global 1, Number 20);
         Builtin ([| Location (Global 0); Location (Global 1) |], "println");
         Create (Register 0, [||], Relative 5);
         Resume (Void, Location (Register 0), Relative 3);
         Builtin ([| Location (Global 0); Location (Global 1) |], "println");
         Halt;
         Trap;
         Builtin ([| Location (Global 0); Location (Global 1) |], "println");
         Assign (Global 0, Number 30);
         Assign (Global 1, Number 40);
         Yield Null;
         Trap;
       ]
       100 ~globals_cnt:2);
  ()

let fib () =
  check string "Fib(20)" "6765"
    (interpret
       [
         (* 0: main *)
         Alloca 1;
         Call (Register 0, [| Number 20 |], Static 4);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
         (* 4: fib *)
         Alloca 2;
         Sub (Register 0, Location (Argument 0), Number 1);
         Sub (Register 1, Location (Argument 0), Number 2);
         GotoIfNeg (Location (Register 1), Static 12);
         Call (Register 0, [| Location (Register 0) |], Static 4);
         Call (Register 1, [| Location (Register 1) |], Static 4);
         Add (Register 0, Location (Register 0), Location (Register 1));
         Ret (Location (Register 0));
         Ret (Location (Argument 0));
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
          ("read_line", `Quick, read_line);
          ("call", `Quick, call);
          ("null", `Quick, null);
          ("goto", `Quick, goto);
          ("globals", `Quick, globals);
        ] );
      ("programs", [ ("fib", `Quick, fib) ]);
    ]
