open Pkulang
open Alcotest

let fmt_definition fmt d =
  Format.fprintf fmt "%s" (Symbols.string_of_definition d)

let fmt_type fmt t = Format.fprintf fmt "%s" (Symbols.string_of_type t)

let definition : Symbols.definition testable =
  testable fmt_definition Symbols.def_eql

let typ : Symbols.typ testable = testable fmt_type Symbols.typ_eql

let find_let name node =
  Ast.find_child
    (fun n -> match n with LetStmt x when x.name = name -> true | _ -> false)
    node
  |> Option.get

let find_fn name node =
  Ast.find_child
    (fun n -> match n with FnDecl x when x.name = name -> true | _ -> false)
    node
  |> Option.get

let find_struct name node =
  Ast.find_child
    (fun n ->
      match n with StructDecl x when x.name = name -> true | _ -> false)
    node
  |> Option.get

let find_field name node =
  Ast.find_child
    (fun n -> match n with Field x when x.name = name -> true | _ -> false)
    node
  |> Option.get

let find_for name node =
  Ast.find_child
    (fun n -> match n with ForLoop x when x.var = name -> true | _ -> false)
    node
  |> Option.get

let find_resume name node =
  Ast.find_child
    (fun n ->
      match n with IfResumeStmt x when x.var = Some name -> true | _ -> false)
    node
  |> Option.get

let find_var name node =
  Ast.find_child
    (fun n -> match n with VarExpr x when x.name = name -> true | _ -> false)
    node
  |> Option.get

let simple_vars () =
  let src =
    {|
    fn main() void {
      let a: int = 10;
      println(a);
    }
  |}
  in
  let root = Parser.parse_root src in
  let let_a = find_let "a" (Root root) in
  let a_var = find_var "a" (Root root) in
  let def = Symbols.get_definition a_var in
  check definition "Var definition in the same scope" (Node let_a) def;
  check typ "Has int type" IntType (Symbols.get_type a_var)

let scoped_var () =
  let src =
    {|
    let a: str = "foo";
    fn main() void {
      let a: int = 10;
      println(a);
    }
  |}
  in
  let root = Parser.parse_root src in
  let main_fn = find_fn "main" (Root root) in
  let let_a = find_let "a" main_fn in
  let a_var = find_var "a" (Root root) in
  let def = Symbols.get_definition a_var in
  check definition "Var definition in the same scope" (Node let_a) def;
  check typ "Has int type" IntType (Symbols.get_type a_var)

let var_from_outside () =
  let src =
    {|
    let a: int = 10;
    fn main() void {
      println(a);
    }
  |}
  in
  let root = Parser.parse_root src in
  let let_a = find_let "a" (Root root) in
  let a_var = find_var "a" (Root root) in
  let def = Symbols.get_definition a_var in
  check definition "Var definition from outer scope" (Node let_a) def;
  check typ "Has int type" IntType (Symbols.get_type a_var)

let simple_fn () =
  let src =
    {|
    fn foo() void {}
    fn main() void {
      foo();
    }
  |}
  in
  let root = Parser.parse_root src in
  let decl = find_fn "foo" (Root root) in
  let var = find_var "foo" (Root root) in
  let def = Symbols.get_definition var in
  check definition "found decl" (Node decl) def;
  check typ "has correct type"
    (FnType { args = []; ret = VoidType })
    (Symbols.get_type var)

let fn_below () =
  let src =
    {|
    fn main() void {
      foo();
    }
    fn foo() void {}
  |}
  in
  let root = Parser.parse_root src in
  let decl = find_fn "foo" (Root root) in
  let var = find_var "foo" (Root root) in
  let def = Symbols.get_definition var in
  check definition "found decl" (Node decl) def;
  check typ "has correct type"
    (FnType { args = []; ret = VoidType })
    (Symbols.get_type var)

let fn_with_args () =
  let src =
    {|
    fn foo(a: int, b: str) void {}
    fn main() void {
      foo();
    }
  |}
  in
  let root = Parser.parse_root src in
  let decl = find_fn "foo" (Root root) in
  let var = find_var "foo" (Root root) in
  let def = Symbols.get_definition var in
  check definition "found decl" (Node decl) def;
  check typ "has correct type"
    (FnType { args = [ IntType; StringType ]; ret = VoidType })
    (Symbols.get_type var)

let fptr () =
  let src =
    {|
    fn main() void {
      let fptr: fn (int, str) void = null;
    }
  |}
  in
  let root = Parser.parse_root src in
  let decl = find_let "fptr" (Root root) in
  check typ "has correct type"
    (FnType { args = [ IntType; StringType ]; ret = VoidType })
    (Symbols.get_type decl)

let for_loop_var () =
  let src =
    {|
    fn main() void {
      let iter: co int = null;
      for (x: iter) println(x);
    }
  |}
  in
  let root = Parser.parse_root src in
  let decl = find_for "x" (Root root) in
  let var = find_var "x" (Root root) in
  let def = Symbols.get_definition var in
  check definition "found decl" (Node decl) def;
  check typ "has correct type" IntType (Symbols.get_type var)

let if_resume_var () =
  let src =
    {|
    fn main() void {
      let coro: co int = null;
      if resume (x: coro) println(x);
    }
  |}
  in
  let root = Parser.parse_root src in
  let decl = find_resume "x" (Root root) in
  let var = find_var "x" (Root root) in
  let def = Symbols.get_definition var in
  check definition "found decl" (Node decl) def;
  check typ "has correct type" IntType (Symbols.get_type var)

let struct_typ () =
  let src =
    {|
    struct Foo { x: int, }
    fn main() void {
      let x: Foo = new Foo{x: 10};
    }
  |}
  in
  let root = Parser.parse_root src in
  let foo_struct = find_struct "Foo" (Root root) in
  let let_stmt =
    match find_let "x" (Root root) with
    | LetStmt x -> x
    | _ -> failwith "unreachable"
  in
  let def = Symbols.get_definition (Ast.type_to_node let_stmt.typ) in
  check definition "found decl" (Node foo_struct) def;
  check typ "has correct type"
    (StructType
       (match foo_struct with StructDecl x -> x | _ -> failwith "unreachable"))
    (Symbols.get_type (Ast.type_to_node let_stmt.typ))

let field_access () =
  let src =
    {|
    struct Foo { x: int, }
    fn main() void {
      let x: Foo = new Foo{x: 10};
      println(x.x);
    }
  |}
  in
  let root = Parser.parse_root src in
  let foo_struct = find_struct "Foo" (Root root) in
  let x_field = find_field "x" foo_struct in
  let dot = find_var "x" (Root root) |> Ast.node_parent |> Option.get in
  let def = Symbols.get_definition dot in
  check definition "field definition" (Node x_field) def;
  check typ "field type" IntType (Symbols.get_type dot)

let field_chain () =
  let src =
    {|
    struct Foo { bar: Bar, }
    struct Bar { x: int, }
    fn main() void {
      let x: Foo = new Foo{};
      println(x.bar.x);
    }
  |}
  in
  let root = Parser.parse_root src in
  let bar_struct = find_struct "Bar" (Root root) in
  let x_field = find_field "x" bar_struct in
  let dot =
    find_var "x" (Root root) |> Ast.node_parent |> Option.get |> Ast.node_parent
    |> Option.get
  in
  let def = Symbols.get_definition dot in
  check definition "field definition" (Node x_field) def;
  check typ "field type" IntType (Symbols.get_type dot)

let inner_struct () =
  let src =
    {|
    struct Foo {
      x: int,
      struct Bar{
        x: int,
      }
    }
    fn main() void {
      let x: Foo.Bar = new Foo.Bar{x: 10};
      println(x.x);
    }
  |}
  in
  let root = Parser.parse_root src in
  let bar_struct = find_struct "Bar" (Root root) in
  let x_field = find_field "x" bar_struct in
  let dot = find_var "x" (Root root) |> Ast.node_parent |> Option.get in
  let def = Symbols.get_definition dot in
  check definition "field definition" (Node x_field) def;
  check typ "field type" IntType (Symbols.get_type dot)

let binexpr_type () =
  let src = {|
    fn main() void {
      println("a" + "b");
    }
  |} in
  let root = Parser.parse_root src in
  let add = Ast.find_child_of_kind BinExpr (Root root) |> Option.get in
  check typ "\"a\"+\"b\"" StringType (Symbols.get_type add)

let neg_type () =
  let src = {|
    fn main() void {
      println(-10);
    }
  |} in
  let root = Parser.parse_root src in
  let expr = Ast.find_child_of_kind UnaryExpr (Root root) |> Option.get in
  check typ "-10" IntType (Symbols.get_type expr)

let call_type () =
  let src =
    {|
    fn foo() int {}
    fn main() void {
      foo();
    }
  |}
  in
  let root = Parser.parse_root src in
  let expr = Ast.find_child_of_kind CallExpr (Root root) |> Option.get in
  check typ "foo()" IntType (Symbols.get_type expr)

let index_type () =
  let src =
    {|
    fn main() void {
      let a: [int] = [];
      a[0];
    }
  |}
  in
  let root = Parser.parse_root src in
  let expr = Ast.find_child_of_kind IndexExpr (Root root) |> Option.get in
  check typ "a[0]" IntType (Symbols.get_type expr)

let field_of_result () =
  let src =
    {|
    struct Foo{ x: int, }
    fn make_foo() Foo {}
    fn main() void {
      make_foo().x;
    }
  |}
  in
  let root = Parser.parse_root src in
  let expr = Ast.find_child_of_kind DotExpr (Root root) |> Option.get in
  let decl = find_field "x" (Root root) in
  let def = Symbols.get_definition expr in
  check definition "make_foo().x" (Node decl) def;
  check typ "make_foo().x" IntType (Symbols.get_type expr)

let () =
  run "Symbols"
    [
      ( "vars",
        [
          ("simple_vars", `Quick, simple_vars);
          ("scoped_var", `Quick, scoped_var);
          ("var_from_outside", `Quick, var_from_outside);
        ] );
      ( "fns",
        [
          ("simple_fn", `Quick, simple_fn);
          ("fn_below", `Quick, fn_below);
          ("fn_with_args", `Quick, fn_with_args);
          ("fptr", `Quick, fptr);
        ] );
      ( "other_vars",
        [
          ("for_loop_var", `Quick, for_loop_var);
          ("if_resume_var", `Quick, if_resume_var);
        ] );
      ( "structs",
        [
          ("struct_typ", `Quick, struct_typ);
          ("field_access", `Quick, field_access);
          ("field_chain", `Quick, field_chain);
          ("inner_struct", `Quick, inner_struct);
        ] );
      ( "exprs",
        [
          ("binexpr_type", `Quick, binexpr_type);
          ("neg_type", `Quick, neg_type);
          ("call_type", `Quick, call_type);
          ("index_type", `Quick, index_type);
          ("field_of_result", `Quick, field_of_result);
        ] );
    ]
