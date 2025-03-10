open Alcotest
open Pkulang

let run_tok str =
  let toks = str |> Tokenizer.tokenize in
  match List.rev toks with
  | e :: ts ->
      if e.kind = TokEnd then List.rev ts |> Tokenizer.tok_list_to_str
      else failwith "No end token"
  | _ -> failwith "No end token"

let ident_checks () =
  check string "ident" "Ident(hello)" (run_tok "hello");
  check string "fancy ident" "Ident(hello_world_123)"
    (run_tok "hello_world_123");
  check string "many idents" "Ident(Hello),Ident(World)" (run_tok "Hello World");
  check string "iffy" "Ident(hello),If,Ident(iffy)" (run_tok "hello if iffy")

let number_checks () =
  check string "number" "Number(123)" (run_tok "123");
  check string "many numbers" "Number(123),Number(456)" (run_tok "123 456");
  check string "number and ident" "Ident(foo),Number(123)" (run_tok "foo 123");
  check string "ident with digits" "Ident(foo123)" (run_tok "foo123")

let symbol_checks () =
  check string "symbol" "Add" (run_tok "+");
  check string "many symbols" "Add,Add,Add" (run_tok "+ + +");
  check string "symbols together" "Add,Add,Add" (run_tok "+++");
  check string "assign vs eql" "Assign,Eq" (run_tok "= ==");
  check string "eq vs neq" "Eq,Neq" (run_tok "== !=")

let strings_check () =
  check string "string" "String(\"hello\")" (run_tok "\"hello\"");
  check string "empty" "String(\"\")" (run_tok "\"\"");
  check string "multi word" "String(\"a b\")" (run_tok "\"a b\"");
  check string "many strings" "String(\"foo\"),String(\"bar\")"
    (run_tok "\"foo\" \"bar\"");
  check string "string and ident" "String(\"x\"),Ident(y)" (run_tok "\"x\" y")

let fib_example () =
  let src =
    {|
    fn fib(n: int) int {
        if (n < 2) return n;
       return fib(n - 1) + fib(n - 2);
    }|}
  in
  check string "fib"
    "Fn,Ident(fib),Lp,Ident(n),Colon,Ident(int),Rp,Ident(int),Lb,If,Lp,Ident(n),Lt,Number(2),Rp,Return,Ident(n),Semi,Return,Ident(fib),Lp,Ident(n),Sub,Number(1),Rp,Add,Ident(fib),Lp,Ident(n),Sub,Number(2),Rp,Semi,Rb"
    (run_tok src)

let () =
  run "Tokenizer"
    [
      ( "basic",
        [
          ("keyword", `Quick, ident_checks);
          ("number", `Quick, number_checks);
          ("symbols", `Quick, symbol_checks);
          ("strings", `Quick, strings_check);
        ] );
      ("example", [ ("fib", `Quick, fib_example) ]);
    ]
