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
  complete r n

let dont_free_active () =
  let r =
    interpret [ DisableGc; Alloca 1; New (Register 0); ForceGc; Halt ] 100
  in
  check int "Memory used" 1 r.heap.used_size;
  check int "Max address" 1025 r.heap.max_address

let force_gc () =
  let r =
    interpret
      [
        DisableGc;
        Alloca 1;
        New (Register 0);
        Assign (Register 0, Number 0);
        ForceGc;
        Halt;
      ]
      100
  in
  check int "Memory used" 0 r.heap.used_size;
  check int "Max address" 1025 r.heap.max_address

let create_1_page_of_garbage () =
  (* gc is set to free every 4096 objects *)
  (* we allocate 4096 objects in a loop, overwriting r0 *)
  (* at some point gc kicks in, sees we only have the last object in r0 *)
  (* and removes the 4095 previous garbage objects *)
  (* so in total we need space to store 1024+4096 objects (first 1024 are unused) *)
  (* and after gc we will only keep one *)
  (* each object is a HeapNumber, storing the iteration (4096..=1) *)
  let r =
    interpret
      [
        Alloca 2;
        Assign (Register 0, Number 4096);
        GotoIfZero (Location (Register 0), Relative 5);
        New (Register 1);
        Store (Register 1, Location (Register 0));
        Sub (Register 0, Location (Register 0), Number 1);
        Goto (Relative (-4));
        Builtin ([| Location (Register 1) |], "print");
        Halt;
      ]
      100000
  in
  check string "The last object" "1\n" r.stdout;
  check int "Memory used" 1 r.heap.used_size;
  check int "Max address" (1024 + 4096) r.heap.max_address

let a_bunch_of_garbage () =
  (* same setup as last test, but with "a bunch" of objects *)
  (* i.e. gc triggers multiple times *)
  (* note if gc is off, this should OOM *)
  let r =
    interpret
      [
        Alloca 2;
        Assign (Register 0, Number 100000);
        GotoIfZero (Location (Register 0), Relative 5);
        New (Register 1);
        Store (Register 1, Location (Register 0));
        Sub (Register 0, Location (Register 0), Number 1);
        Goto (Relative (-4));
        Halt;
      ]
      10000000
  in
  check bool "Memory used is less than 4096" true (r.heap.used_size < 4096);
  check int "Max address" (1024 + 4096) r.heap.max_address

let oom () =
  check_raises "OOM" Heap.OOM (fun () ->
      ignore
      @@ interpret
           [
             DisableGc;
             Alloca 2;
             Assign (Register 0, Number 100000);
             GotoIfZero (Location (Register 0), Relative 5);
             New (Register 1);
             Store (Register 1, Location (Register 0));
             Sub (Register 0, Location (Register 0), Number 1);
             Goto (Relative (-4));
             Halt;
           ]
           1000000)

let array () =
  let r =
    interpret
      [
        DisableGc;
        Alloca 3;
        New (Register 0);
        Resize (Register 0, Number 100);
        Assign (Register 2, Number 100);
        GotoIfZero (Location (Register 2), Relative 6);
        New (Register 1);
        Store (Register 1, Location (Register 2));
        Sub (Register 2, Location (Register 2), Number 1);
        IndexSet (Register 0, Location (Register 2), Location (Register 1));
        Goto (Relative (-5));
        Assign (Register 0, Number 0);
        Assign (Register 1, Number 0);
        ForceGc;
        Halt;
      ]
      10000000
  in
  check int "Memory used" 0 r.heap.used_size;
  check int "Max address" (1024 + 101) r.heap.max_address

let () =
  run "Runtime: gc"
    [
      ( "basic",
        [
          ("dont_free_active", `Quick, dont_free_active);
          ("force_gc", `Quick, force_gc);
          ("create_1_page_of_garbage", `Quick, create_1_page_of_garbage);
          ("a_bunch_of_garbage", `Quick, a_bunch_of_garbage);
          ("oom", `Quick, oom);
          ("array", `Quick, array);
        ] );
    ]
