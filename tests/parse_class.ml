open Alcotest
open Pkulang

let run_parser s =
  let toks = s |> Tokenizer.tokenize in
  Parser.parse_expr { toks; src = s } |> snd |> Ast.node_to_str

let impl_list () =
  check string "empty" "(class Foo (impl) (decls))" (run_parser "class Foo{}");
  check string "one" "(class Foo (impl Bar) (decls))"
    (run_parser "class Foo:Bar{}");
  check string "many" "(class Foo (impl Bar Baz Qux) (decls))"
    (run_parser "class Foo:Bar,Baz,Qux{}")

let decls () =
  check string "empty" "(class Foo (impl) (decls))" (run_parser "class Foo{}");
  check string "field" "(class Foo (impl) (decls (field x (type int) _)))"
    (run_parser "class Foo{x:int,}");
  check string "many fields"
    "(class Foo (impl) (decls (field x (type int) _) (field y (type int) (num \
     10)) (field z (type Bar) _)))"
    (run_parser "class Foo{x:int,y:int=10,z:Bar,}")

let methods () =
  check string "empty" "(class Foo (impl) (decls))" (run_parser "class Foo{}");
  check string "one" "(class Foo (impl) (decls (fn foo (type int) (block))))"
    (run_parser "class Foo{fn foo()int{}}");
  check string "many"
    "(class Foo (impl) (decls (fn foo (type int) (block)) (fn bar (type void) \
     (block))))"
    (run_parser "class Foo{fn foo()int{}fn bar()void{}}")

let list_class () =
  let src =
    {| 
  class List { 
    val: int, 
    next: List = null, 

    fn print() void {
      print_int(this.val);
      if (this.next != null) {this.next.print();}
    }
  }
  |}
  in
  check string "list"
    "(class List (impl) (decls (field val (type int) _) (field next (type \
     List) (var null)) (fn print (type void) (block (call (var print_int) (dot \
     (var this) val)) (if (bin Neq (dot (var this) next) (var null)) (block \
     (call (dot (dot (var this) next) print))) _)))))"
    (run_parser src)

let () =
  run "Parser: classes"
    [
      ( "basic",
        [
          ("impl", `Quick, impl_list);
          ("decls", `Quick, decls);
          ("methods", `Quick, methods);
        ] );
      ("examples", [ ("list", `Quick, list_class) ]);
    ]
