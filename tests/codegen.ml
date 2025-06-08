open Pkulang
open Alcotest

let compile src =
  let root = Parser.parse_root src in
  let symtab = SymbolTableBuilder.build_symbol_table root in
  print_endline @@ SymbolTable.dump symtab;
  let r = Codegen.codegen src root symtab in
  r

let interpret r n =
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then (
      Runtime.trace r;
      failwith "Too many steps!")
    else complete (Runtime.step r) (n - 1)
  in
  (complete r n).stdout

let while_loop () =
  let src =
    {|
  fn main() void {
    let i: int = 0;
    let sum: int = 0;
    while(i < 10) {
      sum = sum + i;
      i = i + 1;
    }
    print(sum);
    return;
  }
  |}
  in
  let r = compile src in
  check string "Sum 0..10" "45\n" (interpret r 1000)

let fib_test () =
  let src =
    {|
    fn fib(n: int) int {
      if (n < 2) return n;
      return fib(n-1) + fib(n-2);
    } 

    fn main() void {
      print(fib(10));
      return;
    }
    |}
  in
  let r = compile src in
  check string "Fib(10)" "55\n" (interpret r 10000)

let range_test () =
  let src =
    {|
  co range(a: int, b: int) int {
    while(a < b) {
      yield a;
      a = a + 1;
    }
  }

  fn main() void {
    for (x: create(range, 5, 10)) {
      print(x);
    }
  }
  |}
  in
  let r = compile src in
  Array.iteri
    (fun i (cmd : Runtime.command) ->
      print_endline @@ Runtime.string_of_cmd i cmd.cmd)
    r.code;

  check string "Range(5, 10)" "5\n6\n7\n8\n9\n" (interpret r 1000)

let quicksort () =
  let src =
    {|
  fn partition(arr: [int], start: int, end: int) int {
    let i: int = start;
    let j: int = start + 1;
    while (j < end) {
      if (arr[j] < arr[i]) {
        let a: int = arr[i];
        let b: int = arr[i + 1];
        let c: int = arr[j];
        arr[i] = c;
        arr[j] = b;
        arr[i + 1] = a;
        i = i + 1;
      }
      j = j + 1;
    }
    return i;
  }
  fn qsort(arr: [int], start: int, end: int) void {
    if (start + 1 < end) {
      let mid: int = partition(arr, start, end);
      qsort(arr, start, mid);
      qsort(arr, mid + 1, end);
    }
  }
  fn main() void {
    let arr: [int] = [7, 4, 8, 1, 5, 9, 2, 0, 6, 3];
    qsort(arr, 0, 10);
    print(arr);
  }
  |}
  in
  let r = compile src in
  Array.iteri
    (fun i (cmd : Runtime.command) ->
      print_endline @@ Runtime.string_of_cmd i cmd.cmd)
    r.code;
  check string "Range(5, 10)" "[0,1,2,3,4,5,6,7,8,9]\n" (interpret r 1000)

let () =
  run "Codegen"
    [
      ( "programs",
        [
          ("fib", `Quick, fib_test);
          ("sum", `Quick, while_loop);
          ("range", `Quick, range_test);
          ("qsort", `Quick, quicksort);
        ] );
    ]
