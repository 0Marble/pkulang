open Alcotest
open Pkulang

let run_parser s =
  let toks = s |> Tokenizer.tokenize in
  Parser.parse_stmt { toks; src = s } |> snd |> Ast.node_to_str

let let_stmt () =
  check string "let" "(let x (type int) (num 10))"
    (run_parser "let x: int = 10;")

let return_stmt () =
  check string "return with value" "(return (var x))" (run_parser "return x;");
  check string "return without value" "(return _)" (run_parser "return;")

let break_stmt () =
  check string "simple break" "(break _ _)" (run_parser "break;");
  check string "break with label" "(break foo _)" (run_parser "break :foo;");
  check string "break with value" "(break _ (var x))" (run_parser "break x;");
  check string "break with value and label" "(break foo (var x))"
    (run_parser "break :foo x;")

let continue_stmt () =
  check string "continue" "(continue _)" (run_parser "continue;");
  check string "continue with label" "(continue foo)"
    (run_parser "continue :foo;")

let block_stmt () =
  check string "empty block" "(block)" (run_parser "{}");
  check string "block" "(block (let x (type int) (num 10)))"
    (run_parser "{let x:int=10;}");
  check string "block with many stmts"
    "(block (let x (type int) (num 10)) (return (num 10)) (break _ _))"
    (run_parser "{let x:int=10;return 10; break;}");
  check string "nested block" "(block (block (break _ _)))"
    (run_parser "{{break;}}");
  check string "expr in block"
    "(block (let x (type int) (num 10)) (bin Assign (var x) (num 20)))"
    (run_parser "{let x:int = 10; x=20;}")

let if_stmt () =
  check string "if" "(if (var x) (block) _)" (run_parser "if (x) {}");
  check string "if else"
    "(if (var x) (block (return (num 10))) (block (return (num 20))))"
    (run_parser "if (x) {return 10;} else {return 20;}");
  check string "if in block" "(block (if (var x) (block) _) (break _ _))"
    (run_parser "{if(x){}break;}")

let while_loop () =
  check string "normal" "(while (var x) (block) _)" (run_parser "while(x){}");
  check string "with else" "(while (var x) (block) (block))"
    (run_parser "while(x){}else{}")

let for_loop () =
  check string "normal" "(for x (var arr) (block) _)"
    (run_parser "for (x : arr) {}");
  check string "with else" "(for x (var arr) (block) (block))"
    (run_parser "for (x : arr) {} else {}")

let labeled () =
  check string "while" "(label foo (while (var x) (block) _))"
    (run_parser ":foo while(x){}")

let function_decl () =
  check string "fib" "(fn fib (arg n (type int)) (type int) (block))"
    (run_parser "fn fib(n:int)int{}")

let struct_decl () =
  check string "empty strcut" "(struct Foo)" (run_parser "struct Foo{}");
  check string "fields"
    "(struct Foo (field x (type int) _) (field y (type Bar) _))"
    (run_parser "struct Foo{x:int,y:Bar,}")

let () =
  run "Parser: statements"
    [
      ( "basic",
        [
          ("let", `Quick, let_stmt);
          ("return", `Quick, return_stmt);
          ("break", `Quick, break_stmt);
          ("continue", `Quick, continue_stmt);
          ("block", `Quick, block_stmt);
          ("if", `Quick, if_stmt);
          ("while", `Quick, while_loop);
          ("for", `Quick, for_loop);
          ("label", `Quick, labeled);
          ("fn", `Quick, function_decl);
          ("struct", `Quick, struct_decl);
        ] );
    ]
