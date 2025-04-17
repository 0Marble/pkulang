open Alcotest
open Pkulang

let int_option : int option testable = option int

(* Test basic add, find, and mem in the root scope *)
let test_basic_ops () =
  let env = Symbol_table.create () in
  Symbol_table.add env "count" 10;
  Symbol_table.add env "another_count" 25;

  (* Find and mem for existing variables *)
  check int_option "Find 'count'" (Some 10) (Symbol_table.find env "count");
  check int_option "Find 'another_count'" (Some 25)
    (Symbol_table.find env "another_count");
  check bool "Mem 'count'" true (Symbol_table.mem env "count");
  check bool "Mem 'missing'" false (Symbol_table.mem env "missing");

  (* find_exn for existing and non-existent*)
  check int "Find_exn 'count'" 10 (Symbol_table.find_exn env "count");
  check_raises "Find_exn missing raises"
    (Invalid_argument "Symbol_table.find_exn: “missing” not bound") (fun () ->
      ignore (Symbol_table.find_exn env "missing"))

(* Test entering/exiting scopes, visibility, and shadowing *)
let test_scoping_and_shadowing () =
  let env0 = Symbol_table.create () in
  Symbol_table.add env0 "a" 1;
  let env1 = Symbol_table.enter_scope env0 in
  Symbol_table.add env1 "b" 2;
  Symbol_table.add env1 "a" 3;

  (* shadow 'a' in inner *)

  (* Inner scope: b=2, a shadowed to 3 *)
  check int_option "Inner find 'b'" (Some 2) (Symbol_table.find env1 "b");
  check int_option "Inner find 'a'" (Some 3) (Symbol_table.find env1 "a");

  let env0_restored = Symbol_table.exit_scope env1 in
  (* After exit: b gone, a back to 1 *)
  check int_option "Outer find 'a'" (Some 1)
    (Symbol_table.find env0_restored "a");
  check int_option "Outer find 'b'" None (Symbol_table.find env0_restored "b")

(* Test multiple nested scopes and fold *)
let test_multiple_scopes_and_fold () =
  let env0 = Symbol_table.create () in
  Symbol_table.add env0 "x" 5;
  let env1 = Symbol_table.enter_scope env0 in
  Symbol_table.add env1 "y" 10;
  let env2 = Symbol_table.enter_scope env1 in
  Symbol_table.add env2 "z" 20;

  (* Sum all values across scopes using fold *)
  let sum_all = Symbol_table.fold env2 ~init:0 ~f:(fun _ v acc -> acc + v) in
  check int "Fold sum" 35 sum_all;

  (* Exit scopes and test find_exn *)
  let _ = Symbol_table.exit_scope env2 in
  check_raises "Exit root scope raises No_scope" Symbol_table.No_scope
    (fun () -> ignore (Symbol_table.exit_scope (Symbol_table.create ())))

(* --- Test Suite --- *)
let symbol_table_suite =
  [
    ("Basic Ops & Mem & find_exn", `Quick, test_basic_ops);
    ("Scoping & Shadowing", `Quick, test_scoping_and_shadowing);
    ("Nested & Fold & No_scope", `Quick, test_multiple_scopes_and_fold);
  ]

let () =
  Alcotest.run "Symbol Table Tests"
    [ ("Symbol Table Suite", symbol_table_suite) ]
