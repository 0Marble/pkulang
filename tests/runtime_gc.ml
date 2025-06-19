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
      (fun () -> None)
      (fun (s : string) ->
        prerr_string s;
        stdout := !stdout ^ s)
  in
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  let r = complete r n in
  (r, !stdout)

let dont_free_active () =
  let r, _ =
    interpret [ DisableGc; Alloca 1; New (Register 0); ForceGc; Halt ] 100
  in
  check int "Memory used" 1 r.heap.used_size;
  check int "Max address" 1025 r.heap.max_address

let force_gc () =
  let r, _ =
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
  let r, s =
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
  check string "The last object" "1" s;
  check int "Memory used" 1 r.heap.used_size;
  check int "Max address" (1024 + 4096) r.heap.max_address

let a_bunch_of_garbage () =
  (* same setup as last test, but with "a bunch" of objects *)
  (* i.e. gc triggers multiple times *)
  (* note if gc is off, this should OOM *)
  let r, _ =
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
  let r, _ =
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

let obj () =
  let r, _ =
    interpret
      [
        DisableGc;
        Alloca 2;
        New (Register 0);
        New (Register 1);
        AddField (Register 0, "foo");
        AddField (Register 0, "bar");
        AddField (Register 1, "baz");
        FieldSet (Register 0, "foo", Location (Register 1));
        FieldSet (Register 0, "bar", Number 10);
        FieldSet (Register 1, "baz", Number 20);
        Assign (Register 0, Null);
        Assign (Register 1, Null);
        ForceGc;
        Halt;
      ]
      100
  in
  check int "Memory used" 0 r.heap.used_size;
  check int "Max address" (1024 + 2) r.heap.max_address

let circular_ref () =
  let r, _ =
    interpret
      [
        DisableGc;
        Alloca 1;
        New (Register 0);
        AddField (Register 0, "self");
        FieldSet (Register 0, "self", Location (Register 0));
        Assign (Register 0, Null);
        ForceGc;
        Halt;
      ]
      100
  in
  check int "Memory used" 0 r.heap.used_size;
  check int "Max address" (1024 + 1) r.heap.max_address

let linked_list () =
  let r, _ =
    interpret
      [
        DisableGc;
        Call (Void, [| Number 100; Null |], Relative 3);
        ForceGc;
        Halt;
        Alloca 2;
        Assign (Register 0, Null);
        GotoIfNeg (Location (Argument 0), Relative 8);
        New (Register 0);
        AddField (Register 0, "next");
        AddField (Register 0, "prev");
        FieldSet (Register 0, "prev", Location (Argument 1));
        Sub (Register 1, Location (Argument 0), Number 1);
        Call
          ( Register 1,
            [| Location (Register 1); Location (Register 0) |],
            Relative (-8) );
        FieldSet (Register 0, "next", Location (Register 1));
        Ret (Location (Register 0));
      ]
      10000
  in
  check int "Memory used" 0 r.heap.used_size;
  check int "Max address" (1024 + 101) r.heap.max_address

let reassign () =
  let r, s =
    interpret
      [
        DisableGc;
        Alloca 2;
        New (Register 0);
        Assign (Register 1, Location (Register 0));
        AddField (Register 1, "foo");
        AddField (Register 0, "bar");
        FieldSet (Register 0, "foo", Number 10);
        FieldSet (Register 1, "bar", Number 20);
        Assign (Register 0, Number 0);
        ForceGc;
        Builtin ([| Location (Register 0); Location (Register 1) |], "print");
        Halt;
      ]
      100
  in
  check string "output" "0, {foo: 10, bar: 20}" s;
  check int "Memory used" 1 r.heap.used_size;
  check int "Max address" (1024 + 1) r.heap.max_address

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
          ("object", `Quick, obj);
          ("circular_ref", `Quick, circular_ref);
          ("linked_list", `Quick, linked_list);
          ("reassign", `Quick, reassign);
        ] );
    ]
