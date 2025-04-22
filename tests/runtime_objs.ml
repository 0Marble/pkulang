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

let print () =
  check string "Print object" "{foo: 10, bar: 20}\n"
    (interpret
       [
         Alloca 2;
         New (Register 0);
         AddField (Register 0, "foo");
         AddField (Register 0, "bar");
         FieldSet (Register 0, "foo", Number 10);
         FieldSet (Register 0, "bar", Number 20);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       100)

let nested () =
  check string "Nested" "{foo: {baz: 20}, bar: 10}\n"
    (interpret
       [
         Alloca 2;
         New (Register 0);
         New (Register 1);
         AddField (Register 0, "foo");
         AddField (Register 0, "bar");
         AddField (Register 1, "baz");
         FieldSet (Register 0, "foo", Location (Register 1));
         FieldSet (Register 0, "bar", Number 10);
         FieldSet (Register 1, "baz", Number 20);
         Builtin ([| Location (Register 0) |], "print");
         Halt;
       ]
       100)

let () =
  run "Runtime: objects"
    [ ("basic", [ ("print", `Quick, print); ("nested", `Quick, nested) ]) ]
