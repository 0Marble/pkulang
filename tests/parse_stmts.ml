open Alcotest
open Pkulang

let run_parser s = Parser.parse_stmt s |> Ast.stmt_to_node |> Ast.node_to_str

let let_stmt () =
  check string "let" "(let x (type int) (num 10))"
    (run_parser "let x: int = 10;")

let return_stmt () =
  check string "return with value" "(return (var x))" (run_parser "return x;");
  check string "return without value" "(return _)" (run_parser "return;")

let block_stmt () =
  check string "empty block" "(block)" (run_parser "{}");
  check string "block" "(block (let x (type int) (num 10)))"
    (run_parser "{let x:int=10;}");
  check string "block with many stmts"
    "(block (let x (type int) (num 10)) (return (num 10)) (break))"
    (run_parser "{let x:int=10;return 10; break;}");
  check string "nested block" "(block (block (break)))"
    (run_parser "{{break;}}");
  check string "expr in block"
    "(block (let x (type int) (num 10)) (bin Assign (var x) (num 20)))"
    (run_parser "{let x:int = 10; x=20;}")

let if_stmt () =
  (* check string "if" "(if (var x) (block) _)" (run_parser "if (x) {}"); *)
  check string "if else"
    "(if (var x) (block (return (num 10))) (block (return (num 20))))"
    (run_parser "if (x) {return 10;} else {return 20;}");
  check string "if in block" "(block (if (var x) (block) _) (break))"
    (run_parser "{if(x){}break;}")

let while_loop () =
  check string "normal" "(while (var x) (block))" (run_parser "while(x){}")

let for_loop () =
  check string "normal" "(for x (var arr) (block))"
    (run_parser "for (x : arr) {}")

let function_decl () =
  check string "fib" "(fn fib (arg n (type int)) (type int) (block))"
    (run_parser "fn fib(n:int)int{}")

let struct_decl () =
  check string "empty strcut" "(struct Foo)" (run_parser "struct Foo{}");
  check string "fields"
    "(struct Foo (field x (type int) _) (field y (type Bar) _))"
    (run_parser "struct Foo{x:int,y:Bar,}")

let if_resume () =
  check string "if_resume void" "(if_resume _ (var foo) (block) _)"
    (run_parser "if resume (foo) {}");
  check string "if_resume var" "(if_resume x (var foo) (block) _)"
    (run_parser "if resume (x:foo) {}");
  check string "if_resume else" "(if_resume _ (var foo) (block) (return _))"
    (run_parser "if resume (foo) {} else return;")

let () =
  run "Parser: statements"
    [
      ( "basic",
        [
          ("let", `Quick, let_stmt);
          ("return", `Quick, return_stmt);
          ("block", `Quick, block_stmt);
          ("if", `Quick, if_stmt);
          ("while", `Quick, while_loop);
          ("for", `Quick, for_loop);
          ("fn", `Quick, function_decl);
          ("struct", `Quick, struct_decl);
          ("if_resume", `Quick, if_resume);
        ] );
    ]
