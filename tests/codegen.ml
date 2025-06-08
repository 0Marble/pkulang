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
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  (complete r n).stdout

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

let () = run "Codegen" [ ("programs", [ ("fib", `Quick, fib_test) ]) ]
