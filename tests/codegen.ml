open Alcotest
open Pkulang

let scan_root src (root : Ast.root) =
  let fn_list = ref [] in
  let map =
    Hashtbl.fold
      (fun _ (v : Ast.node) acc ->
        let name =
          match v with
          | FnDecl x ->
              fn_list := v :: !fn_list;
              Some x.name
          | CoDecl x ->
              fn_list := v :: !fn_list;
              Some x.name
          | LetStmt x -> Some x.var_name
          | StructDecl x -> Some x.name
          | Argument x -> Some x.name
          | Field x -> Some x.var_name
          | ForLoop x -> Some x.iter_var
          | IfResumeStmt x -> x.var
          | _ -> None
        in
        (match name with
        | Some name -> (
            match Hashtbl.find_opt acc name with
            | Some _ ->
                Error.fail_at_spot
                  "Error: we only support unique names for testing" src
                  (Ast.node_loc v) (Error.Error Unknown)
            | None -> Hashtbl.add acc name v)
        | None -> ());
        acc)
      root.all_nodes (Hashtbl.create 64)
  in
  (map, !fn_list)

let dummy_get_definition map (node : Ast.node) :
    [ `Node of Ast.node | `Builtin of string ] =
  match node with
  | DotExpr x -> `Node (Hashtbl.find map x.field)
  | VarExpr x ->
      Hashtbl.find_opt map x.name
      |> Option.map (fun n : [ `Node of Ast.node | `Builtin of string ] ->
             `Node n)
      |> Option.value ~default:(`Builtin x.name)
  | _ -> failwith "Error: not a node with definition"

let compile src =
  let root = Parser.parse_root src in
  let map, fns = scan_root src root in
  let r = Codegen.codegen src fns (dummy_get_definition map) root in
  Printf.eprintf "\n%s\n" src;
  Array.iteri
    (fun i (cmd : Runtime.command) ->
      Printf.eprintf "%s\n"
        (Runtime.string_of_cmd ~mark:(i = r.stack.top.ip) i cmd.cmd))
    r.code;
  r

let interpret r n =
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  complete r n

let hello_world () =
  let src =
    {|
      fn main() void {
        println("Hello World!");
      }
      |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "hello world" "Hello World!\n" r.stdout;
  ()

let print_variable () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    println(x);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "print variable" "10\n" r.stdout;
  ()

let assign_variable () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    x = 20;
    println(x);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "modify variable" "20\n" r.stdout;
  ()

let multiple_vars () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    let y: int = 20;
    println(x);
    println(y);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "multiple variables" "10\n20\n" r.stdout;
  ()

let global_variable () =
  let src = {|
  let x: int = 10;
  fn main() void {
    println(x);
  }
  |} in
  let r = compile src in
  let r = interpret r 100 in
  check string "print variable" "10\n" r.stdout;
  ()

let add_expression () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    let y: int = 20;
    println(x+y);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "add variables" "30\n" r.stdout;
  ()

let increment_expr () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    x += 1;
    println(x);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "increment" "11\n" r.stdout;
  ()

let negate_expr () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    println(-x);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "negate" "-10\n" r.stdout;
  ()

let gt_expr () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    println(x > 0);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "gt" "1\n" r.stdout;
  ()

let eql_expr () =
  let src =
    {|
  fn main() void {
    let x: int = 10;
    println(x == 10);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "eql" "1\n" r.stdout;
  ()

let print_array () =
  let src =
    {|
  fn main() void {
    let x: [int] = [10,20,30];
    println(x);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "print array" "[10,20,30]\n" r.stdout;
  ()

let array_index_get () =
  let src =
    {|
  fn main() void {
    let x: [int] = [10,20,30];
    println(x[0]);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "index get" "10\n" r.stdout;
  ()

let array_index_set () =
  let src =
    {|
  fn main() void {
    let x: [int] = [10,20,30];
    x[0] = 40;
    println(x);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "index set" "[40,20,30]\n" r.stdout;
  ()

let array_length () =
  let src =
    {|
  fn main() void {
    let x: [int] = [10,20,30];
    println(len(x));
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "array len" "3\n" r.stdout;
  ()

let array_multidimensional () =
  let src =
    {|
  fn main() void {
    let x: [[int]] = [[10,20,30],[40,50,60]];
    println(x);
    println(len(x));
    println(len(x[0]));
    println(x[0]);
    println(x[0][0]);
    println(x[0,0]);
    x[0,0] = 100;
    println(x);
    x[0][0] = 200;
    println(x);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "multidimensional array"
    "[[10,20,30],[40,50,60]]\n\
     2\n\
     3\n\
     [10,20,30]\n\
     10\n\
     10\n\
     [[100,20,30],[40,50,60]]\n\
     [[200,20,30],[40,50,60]]\n"
    r.stdout;
  ()

let print_struct () =
  let src =
    {|
  struct Foo {
    x: int,
  }
  fn main() void {
    let foo: Foo = new Foo{x: 10};
    println(foo);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "create object" "{x: 10}\n" r.stdout;
  ()

let default_field_value () =
  let src =
    {|
  struct Foo {
    x: int = 10,
  }
  fn main() void {
    let foo: Foo = new Foo{};
    println(foo);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "default field value" "{x: 10}\n" r.stdout;
  ()

let multiple_fields () =
  let src =
    {|
  struct Foo {
    x: int,
    y: int,
  }
  fn main() void {
    let foo: Foo = new Foo{x: 10, y: 20};
    println(foo);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "multiple fields" "{x: 10, y: 20}\n" r.stdout;
  ()

let static_var () =
  let src =
    {|
  struct Foo {
    x: int,
    let y: int = 20;
  }
  fn main() void {
    let foo: Foo = new Foo{x: 10};
    println(foo);
    println(foo.y);
    println(Foo.y);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "static variable" "{x: 10}\n20\n20\n" r.stdout;
  ()

let method_call () =
  let src =
    {|
  struct Foo {
    x: int,
    fn log(this: Foo) void {
      println(this);
    }
  }
  fn main() void {
    let foo: Foo = new Foo{x: 10};
    foo.log();
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "method" "{x: 10}\n" r.stdout;
  ()

let function_call () =
  let src =
    {|
  fn foo() void {
    println("Foo!");
  }
  fn main() void {
    foo();
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "function call" "Foo!\n" r.stdout;
  ()

let function_args () =
  let src =
    {|
  fn foo(x: int) void {
    println(x);
  }
  fn main() void {
    foo(10);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "function call" "10\n" r.stdout;
  ()

let function_return () =
  let src =
    {|
  fn add(x: int, y: int) int {
    return x + y;
  }
  fn main() void {
    println(add(10, 20));
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "return" "30\n" r.stdout;
  ()

let nested_calls () =
  let src =
    {|
  fn bar() void {
    println("bar");
  }
  fn foo() void {
    println(">foo");
    bar();
    println("foo>");
  }
  fn main() void {
    foo();
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "nested" ">foo\nbar\nfoo>\n" r.stdout;
  ()

let recursion () =
  let src =
    {|
  fn fib(n: int) void {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
  }
  fn main() void {
    println(fib(10));
  }
  |}
  in
  let r = compile src in
  let r = interpret r 10000 in
  check string "nested" "55\n" r.stdout;
  ()

let condition () =
  let src = {|
  fn main() void {
    if (1) println(1);
  }
  |} in
  let r = compile src in
  let r = interpret r 100 in
  check string "condtions" "1\n" r.stdout;
  ()

let if_else () =
  let src =
    {|
  fn main() void {
    if (0) println(1);
    else println(2);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "if else" "2\n" r.stdout;
  ()

let nested_if () =
  let src =
    {|
  fn main() void {
    if (1) {
      if (0) println(2);
      else println(1);
    } else {
      println(2);
    }
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "nested if" "1\n" r.stdout;
  ()

let while_loop () =
  let src =
    {|
  fn main() void {
    let i: int = 0;
    while(i < 10) {
      println(i);
      i += 1;
    }
    println(i);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "while loop" "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n" r.stdout;
  ()

let while_break () =
  let src =
    {|
  fn main() void {
    let i: int = 0;
    while(1) {
      println(i);
      if (i == 3) break;
      i += 1;
    }
    println(i);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "while loop" "0\n1\n2\n3\n3\n" r.stdout;
  ()

let while_continue () =
  let src =
    {|
  fn main() void {
    let i: int = 0;
    while(i < 10) {
      i += 1;
      if (i == 3) continue;
      println(i);
    }
  }
  |}
  in
  let r = compile src in
  let r = interpret r 200 in
  check string "while loop" "1\n2\n4\n5\n6\n7\n8\n9\n10\n" r.stdout;
  ()

let while_nested_break () =
  let src =
    {|
  fn main() void {
    let i: int = 0;
    while(i < 5) {
      let j: int = 0;
      while (1) {
        if (j == 3) break;
        j += 1;
      }
      println(j);
      i+=1;
    }
    println(i);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 200 in
  check string "while loop" "3\n3\n3\n3\n3\n5\n" r.stdout;
  ()

let coroutine_create () =
  let src =
    {|
  co foo() void {
    println("foo");
    yield;
  }
  fn main() void {
    let coro: co int = create(foo);
    println("main");
    resume(coro);
  }
  |}
  in
  let r = compile src in
  let r = interpret r 200 in
  check string "create" "main\nfoo\n" r.stdout;
  ()

let yield_vals () =
  let src =
    {|
  co foo() int {
    yield 10;
    yield 20;
  }
  fn main() void {
    let coro: co int = create(foo);
    println(resume(coro));
    println(resume(coro));
  }
  |}
  in
  let r = compile src in
  let r = interpret r 200 in
  check string "create" "10\n20\n" r.stdout;
  ()

let if_resume () =
  let src =
    {|
  co foo() int {
    yield 10;
    yield 20;
  }
  fn main() void {
    let coro: co int = create(foo);
    if resume (x: coro) println(x);
    if resume (y: coro) println(y);
    if resume (z: coro) println("error"); else println("done");
  }
  |}
  in
  let r = compile src in
  let r = interpret r 200 in
  check string "create" "10\n20\ndone\n" r.stdout;
  ()

let coro_range () =
  let src =
    {|
  co range(a: int, b: int) int {
    while (a < b) {
      yield a;
      a += 1;
    }
  }
  fn main() void {
    let coro: co int = create(range,0,5);
    while (1) {
      if resume (x: coro) println(x);
      else break;
    }
  }
  |}
  in
  let r = compile src in
  let r = interpret r 200 in
  check string "range" "0\n1\n2\n3\n4\n" r.stdout;
  ()

let () =
  run "Codegen"
    [
      ("hello_world", [ ("hello_world", `Quick, hello_world) ]);
      ( "variables",
        [
          ("print_variable", `Quick, print_variable);
          ("assign_variable", `Quick, assign_variable);
          ("multiple_vars", `Quick, multiple_vars);
          ("global_variable", `Quick, global_variable);
        ] );
      ( "expressions",
        [
          ("add_expression", `Quick, add_expression);
          ("increment_expr", `Quick, increment_expr);
          ("negate_expr", `Quick, negate_expr);
          ("gt_expr", `Quick, gt_expr);
          ("eql_expr", `Quick, eql_expr);
        ] );
      ( "arrays",
        [
          ("print_array", `Quick, print_array);
          ("array_index_get", `Quick, array_index_get);
          ("array_index_set", `Quick, array_index_set);
          ("array_length", `Quick, array_length);
          ("array_multidimensional", `Quick, array_multidimensional);
        ] );
      ( "structs",
        [
          ("print_struct", `Quick, print_struct);
          ("default_field_value", `Quick, default_field_value);
          ("multiple_fields", `Quick, multiple_fields);
          ("static_var", `Quick, static_var);
          ("method_call", `Quick, method_call);
        ] );
      ( "functions",
        [
          ("function_call", `Quick, function_call);
          ("function_args", `Quick, function_args);
          ("function_return", `Quick, function_return);
          ("nested_calls", `Quick, nested_calls);
          ("recursion", `Quick, recursion);
        ] );
      ( "conditions",
        [
          ("condition", `Quick, condition);
          ("if_else", `Quick, if_else);
          ("nested_if", `Quick, nested_if);
        ] );
      ( "while",
        [
          ("while_loop", `Quick, while_loop);
          ("while_break", `Quick, while_break);
          ("while_continue", `Quick, while_continue);
          ("while_nested_break", `Quick, while_nested_break);
        ] );
      ( "coroutines",
        [
          ("create", `Quick, coroutine_create);
          ("yield_vals", `Quick, yield_vals);
          ("if_resume", `Quick, if_resume);
          ("coro_range", `Quick, coro_range);
        ] );
    ]
