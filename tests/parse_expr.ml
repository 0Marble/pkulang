open Alcotest
open Pkulang

let run_parser s =
  let toks = s |> Tokenizer.tokenize in
  Parser.parse_expr { toks; src = s } |> snd |> Ast.node_to_str

let simple_terms () =
  check string "one number" "(num 10)" (run_parser "10;");
  check string "one var" "(var foo)" (run_parser "foo;")

let binop () =
  check string "a+b" "(bin Add (var a) (var b))" (run_parser "a+b;");
  check string "a+b+c" "(bin Add (bin Add (var a) (var b)) (var c))"
    (run_parser "a+b+c;");
  check string "a+b*c" "(bin Add (var a) (bin Mul (var b) (var c)))"
    (run_parser "a+b*c;");
  check string "a*b+c" "(bin Add (bin Mul (var a) (var b)) (var c))"
    (run_parser "a*b+c;")

let prefix () =
  check string "-a" "(unary Sub (var a))" (run_parser "-a;");
  check string "--a" "(unary Sub (unary Sub (var a)))" (run_parser "--a;");
  check string "-a-b" "(bin Sub (unary Sub (var a)) (var b))"
    (run_parser "-a-b;");
  check string "a--b" "(bin Sub (var a) (unary Sub (var b)))"
    (run_parser "a--b;");
  check string "&a" "(unary Amp (var a))" (run_parser "&a");
  check string "*a" "(unary Mul (var a))" (run_parser "*a");
  check string "*a" "(bin Mul (var a) (unary Mul (var a)))" (run_parser "a**a")

let parens () =
  check string "(a)" "(var a)" (run_parser "(a);");
  check string "(a+b)" "(bin Add (var a) (var b))" (run_parser "(a+b);");
  check string "a+(b)" "(bin Add (var a) (var b))" (run_parser "a+(b);");
  check string "a+(b+c)" "(bin Add (var a) (bin Add (var b) (var c)))"
    (run_parser "a+(b+c);");
  check string "a*(b+c)" "(bin Mul (var a) (bin Add (var b) (var c)))"
    (run_parser "a*(b+c);")

let call () =
  check string "f(x)" "(call (var f) (var x))" (run_parser "f(x);");
  check string "f(x,y)" "(call (var f) (var x) (var y))" (run_parser "f(x,y);");
  check string "f(x+y)" "(call (var f) (bin Add (var x) (var y)))"
    (run_parser "f(x+y);");
  check string "f(x)(y)" "(call (call (var f) (var x)) (var y))"
    (run_parser "f(x)(y);")

let index () =
  check string "a[i]" "(idx (var a) (var i))" (run_parser "a[i];");
  check string "a[i,j]" "(idx (var a) (var i) (var j))" (run_parser "a[i,j];");
  check string "a[i+j]" "(idx (var a) (bin Add (var i) (var j)))"
    (run_parser "a[i+j];");
  check string "a[i][j]" "(idx (idx (var a) (var i)) (var j))"
    (run_parser "a[i][j];")

let array_literal () =
  check string "empty" "(array_literal)" (run_parser "[]");
  check string "[a]" "(array_literal (var a))" (run_parser "[a]");
  check string "[a,b,c]" "(array_literal (var a) (var b) (var c))"
    (run_parser "[a,b,c]");
  check string "nested" "(array_literal (array_literal (var a)))"
    (run_parser "[[a]]");
  check string "index" "(idx (array_literal (var a)) (num 0))"
    (run_parser "[a][0]")

let struct_literal () =
  check string "no fields" "(new (type Foo) (fields))" (run_parser "new Foo{}");
  check string "fields"
    "(new (type Foo) (fields (field_literal x (num 10)) (field_literal y (num \
     20))))"
    (run_parser "new Foo{x:10,y:20}")

let coroutines () =
  check string "yield" "(yield (var a))" (run_parser "yield a");
  check string "yield nothing" "(yield _)" (run_parser "yield;");
  check string "create coroutine" "(coroutine (var f))"
    (run_parser "coroutine f");
  check string "resume with arg" "(resume (var f) (var a))"
    (run_parser "resume(f, a)");
  check string "resume with no arg" "(resume (var f) _)"
    (run_parser "resume(f)")

let complicated () =
  let src = "foo(a+b(x,y[i])*(x*z(c,q,w,e)+1))(bar)[1,2,3];" in
  let res = run_parser src in
  let expect =
    "(idx (call (call (var foo) (bin Add (var a) (bin Mul (call (var b) (var \
     x) (idx (var y) (var i))) (bin Add (bin Mul (var x) (call (var z) (var c) \
     (var q) (var w) (var e))) (num 1))))) (var bar)) (num 1) (num 2) (num 3))"
  in
  check string "complicated expr" expect res

let () =
  run "Parser: expressions"
    [
      ( "basic",
        [
          ("terms", `Quick, simple_terms);
          ("binops", `Quick, binop);
          ("prefix", `Quick, prefix);
          ("parens", `Quick, parens);
          ("call", `Quick, call);
          ("index", `Quick, index);
          ("array literal", `Quick, array_literal);
          ("struct literal", `Quick, struct_literal);
          ("coroutines", `Quick, coroutines);
        ] );
      ("complicated", [ ("c1", `Quick, complicated) ]);
    ]
