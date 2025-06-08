open Pkulang
open Alcotest

let compile src =
  let root = Parser.parse_root src in
  let symtab = SymbolTableBuilder.build_symbol_table root in
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
    print_int(sum);
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
      print_int(fib(10));
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
      print_int(x);
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

let () =
  run "Codegen"
    [
      ( "programs",
        [
          ("fib", `Quick, fib_test);
          ("sum", `Quick, while_loop);
          ("range", `Quick, range_test);
        ] );
    ]
