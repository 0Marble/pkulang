open Alcotest
open Pkulang

let scan_root (root : Ast.root) =
  let node_name (n : Ast.node) =
    let name =
      match n with
      | FnDecl x -> Some x.name
      | CoDecl x -> Some x.name
      | LetStmt x -> Some x.name
      | StructDecl x -> Some x.name
      | Argument x -> Some x.name
      | Field x -> Some x.name
      | ForLoop x -> Some x.var
      | IfResumeStmt x -> x.var
      | _ -> None
    in
    Option.map (fun name -> (name, n)) name
  in
  let fn_list =
    Ast.visit_all_nodes
      (fun n -> match n with FnDecl _ | CoDecl _ -> Some n | _ -> None)
      root
    |> List.filter_map (fun x -> x)
  in
  let map =
    Ast.visit_all_nodes node_name root
    |> List.filter_map (fun x -> x)
    |> List.to_seq |> Hashtbl.of_seq
  in
  (map, fn_list)

let dummy_get_definition map (node : Ast.node) :
    [ `Node of Ast.node | `Builtin of string ] =
  match node with
  | DotExpr x -> `Node (Hashtbl.find map x.field)
  | NamedType x -> `Node (Hashtbl.find map x.name)
  | VarExpr x ->
      Hashtbl.find_opt map x.name
      |> Option.map (fun n : [ `Node of Ast.node | `Builtin of string ] ->
             `Node n)
      |> Option.value ~default:(`Builtin x.name)
  | _ -> failwith "Error: not a node with definition"

let compile_and_run ?(stdin : string option = None) src n =
  let root = Parser.parse_root src in
  let map, fns = scan_root root in
  let stdout = ref "" in
  let stdin = Option.map (String.split_on_char '\n') stdin |> ref in
  let r =
    Codegen.codegen src fns (dummy_get_definition map) root
      (fun () ->
        match !stdin with
        | Some (l :: ls) ->
            stdin := Some ls;
            Some l
        | _ -> None)
      (fun s ->
        prerr_string s;
        stdout := !stdout ^ s)
  in
  Printf.eprintf "\n%s\n" src;
  Array.iteri
    (fun i (cmd : Runtime.command) ->
      Printf.eprintf "%s\n"
        (Runtime.string_of_cmd ~mark:(i = r.stack.top.ip) i cmd.cmd))
    r.code;
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  let _ = complete r n in
  !stdout

let hello_world () =
  let src =
    {|
      fn main() void {
        let name: string = read_line();
        println("Hello", name);
      }
      |}
  in
  let s = compile_and_run ~stdin:(Some "Alex") src 100 in
  check string "hello world" "Hello, Alex\n" s;
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
  let s = compile_and_run src 100 in
  check string "print variable" "10\n" s;
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
  let s = compile_and_run src 100 in
  check string "modify variable" "20\n" s;
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
  let s = compile_and_run src 100 in
  check string "multiple variables" "10\n20\n" s;
  ()

let global_variable () =
  let src = {|
  let x: int = 10;
  fn main() void {
    println(x);
  }
  |} in
  let s = compile_and_run src 100 in
  check string "print variable" "10\n" s;
  ()

let global_variable_complex_init () =
  let src =
    {|
  fn foo() int {
    return 10;
  }
  let x: int = foo();
  fn main() void {
    println(x);
  }
  |}
  in
  let s = compile_and_run src 100 in
  check string "print variable" "10\n" s;
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
  let s = compile_and_run src 100 in
  check string "add variables" "30\n" s;
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
  let s = compile_and_run src 100 in
  check string "increment" "11\n" s;
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
  let s = compile_and_run src 100 in
  check string "negate" "-10\n" s;
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
  let s = compile_and_run src 100 in
  check string "gt" "1\n" s;
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
  let s = compile_and_run src 100 in
  check string "eql" "1\n" s;
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
  let s = compile_and_run src 100 in
  check string "print array" "[10,20,30]\n" s;
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
  let s = compile_and_run src 100 in
  check string "index get" "10\n" s;
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
  let s = compile_and_run src 100 in
  check string "index set" "[40,20,30]\n" s;
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
  let s = compile_and_run src 100 in
  check string "array len" "3\n" s;
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
  let s = compile_and_run src 100 in
  check string "multidimensional array"
    "[[10,20,30],[40,50,60]]\n\
     2\n\
     3\n\
     [10,20,30]\n\
     10\n\
     10\n\
     [[100,20,30],[40,50,60]]\n\
     [[200,20,30],[40,50,60]]\n"
    s;
  ()

let print_string () =
  let src = {|
  fn main() void {
    println("foo");
  }
  |} in
  let s = compile_and_run src 100 in
  check string "print string" "foo\n" s;
  ()

let string_index () =
  let src =
    {|
  fn main() void {
    let s: string = "foo";
    println(s[0]);
    s[0] = 70;
    println(s);
  }
  |}
  in
  let s = compile_and_run src 100 in
  check string "string index" "102\nFoo\n" s;
  ()

let string_concat () =
  let src = {|
  fn main() void {
    println("foo" + "bar");
  }
  |} in
  let s = compile_and_run src 100 in
  check string "string index" "foobar\n" s;
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
  let s = compile_and_run src 100 in
  check string "create object" "{x: 10}\n" s;
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
  let s = compile_and_run src 100 in
  check string "default field value" "{x: 10}\n" s;
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
  let s = compile_and_run src 100 in
  check string "multiple fields" "{x: 10, y: 20}\n" s;
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
  let s = compile_and_run src 100 in
  check string "static variable" "{x: 10}\n20\n20\n" s;
  ()

let method_call () =
  let src =
    {|
  struct Foo {
    x: int,
    fn log(self: Foo) void { println(self); }
  }
  fn main() void {
    let foo: Foo = new Foo{x: 10};
    foo.log();
    Foo.log(foo);
  }
  |}
  in
  let s = compile_and_run src 100 in
  check string "method call" "{x: 10}\n{x: 10}\n" s;
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
  let s = compile_and_run src 100 in
  check string "function call" "Foo!\n" s;
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
  let s = compile_and_run src 100 in
  check string "function call" "10\n" s;
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
  let s = compile_and_run src 100 in
  check string "return" "30\n" s;
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
  let s = compile_and_run src 100 in
  check string "nested" ">foo\nbar\nfoo>\n" s;
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
  let s = compile_and_run src 10000 in
  check string "nested" "55\n" s;
  ()

let condition () =
  let src = {|
  fn main() void {
    if (1) println(1);
  }
  |} in
  let s = compile_and_run src 100 in
  check string "condtions" "1\n" s;
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
  let s = compile_and_run src 100 in
  check string "if else" "2\n" s;
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
  let s = compile_and_run src 100 in
  check string "nested if" "1\n" s;
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
  let s = compile_and_run src 100 in
  check string "while loop" "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n" s;
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
  let s = compile_and_run src 100 in
  check string "while loop" "0\n1\n2\n3\n3\n" s;
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
  let s = compile_and_run src 200 in
  check string "while loop" "1\n2\n4\n5\n6\n7\n8\n9\n10\n" s;
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
  let s = compile_and_run src 200 in
  check string "while loop" "3\n3\n3\n3\n3\n5\n" s;
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
  let s = compile_and_run src 200 in
  check string "create" "main\nfoo\n" s;
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
  let s = compile_and_run src 200 in
  check string "create" "10\n20\n" s;
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
  let s = compile_and_run src 200 in
  check string "create" "10\n20\ndone\n" s;
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
  let s = compile_and_run src 200 in
  check string "range" "0\n1\n2\n3\n4\n" s;
  ()

let return_coro () =
  let src =
    {|
  fn range(from: int, to: int) co int {
    co range_impl(a: int, b: int) int {
      while (a < b) {
        yield a;
        a += 1;
      }
    }
    return create(range_impl, from, to);
  }
  fn main() void {
    let coro: co int = range(0, 5);
    while (1) {
      if resume (x: coro) println(x);
      else break;
    }
  }
  |}
  in
  let s = compile_and_run src 200 in
  check string "range" "0\n1\n2\n3\n4\n" s;
  ()

let for_loop () =
  let src =
    {|
  fn range(from: int, to: int) co int {
    co range_impl(a: int, b: int) int {
      while (a < b) {
        yield a;
        a += 1;
      }
    }
    return create(range_impl, from, to);
  }
  fn main() void {
    for (x: range(0, 5)) {
      println(x);
    }
  }
  |}
  in
  let s = compile_and_run src 200 in
  check string "range" "0\n1\n2\n3\n4\n" s;
  ()

let for_loop_break () =
  let src =
    {|
  fn range(from: int, to: int) co int {
    co range_impl(a: int, b: int) int {
      while (a < b) {
        yield a;
        a += 1;
      }
    }
    return create(range_impl, from, to);
  }
  fn main() void {
    for (x: range(0, 5)) {
      if (x == 3) break;
      println(x);
    }
  }
  |}
  in
  let s = compile_and_run src 200 in
  check string "range" "0\n1\n2\n" s;
  ()

let for_loop_continue () =
  let src =
    {|
  fn range(from: int, to: int) co int {
    co range_impl(a: int, b: int) int {
      while (a < b) {
        yield a;
        a += 1;
      }
    }
    return create(range_impl, from, to);
  }
  fn main() void {
    for (x: range(0, 5)) {
      if (x == 3) continue;
      println(x);
    }
  }
  |}
  in
  let s = compile_and_run src 200 in
  check string "range" "0\n1\n2\n4\n" s;
  ()

let nested_for_loop () =
  let src =
    {|
  fn range(from: int, to: int) co int {
    co range_impl(a: int, b: int) int {
      while (a < b) {
        yield a;
        a += 1;
      }
    }
    return create(range_impl, from, to);
  }
  fn main() void {
    for (x: range(0, 5)) {
      for (y: range(0, x)) println(x, y);
    }
  }
  |}
  in
  let s = compile_and_run src 400 in
  check string "range"
    (List.init 5 (fun x -> List.init x (fun y -> Printf.sprintf "%d, %d\n" x y))
    |> List.concat |> List.fold_left ( ^ ) "")
    s;
  ()

let dynamic_call () =
  let src =
    {|
  fn foo() void {
    println("foo");
  }
  fn bar() void {
    println("bar");
  }
  fn baz() fn() void {
    if (0) return bar;
    else return foo;
  }
  fn main() void {
    baz()();
  }
  |}
  in
  let s = compile_and_run src 200 in
  check string "dynamic call" "foo\n" s;
  ()

let polymorphism () =
  let src =
    {|
  co dog() void {
    while(1) {
      println("bark");
      yield;
    }
  }
  co cat() void {
    while(1) {
      println("meow");
      yield;
    }
  }
  fn main() void {
    let animals: [co void] = [create(dog), create(cat)];
    resume(animals[0]);
    resume(animals[1]);
  }
  |}
  in
  let s = compile_and_run src 200 in
  check string "polymorphism" "bark\nmeow\n" s;
  ()

let quicksort () =
  let src =
    {|
  fn range(from: int, to: int) co int {
    co range_impl(a: int, b: int) int {
      while (a < b) {
        yield a;
        a += 1;
      }
    }
    return create(range_impl, from, to);
  }

  fn quicksort(arr0: [int]) void {
    fn partition(arr1: [int], start1: int, end1: int) int {
      let j: int = start1;
      for (i : range(start1 + 1, end1)) {
        if (arr1[j] > arr1[i]) {
          let a1: int = arr1[j];
          let b1: int = arr1[j + 1];
          let c1: int = arr1[i];
          arr1[j] = c1;
          arr1[i] = b1;
          arr1[j + 1] = a1;
          j += 1;
        }
      }
      return j;
    }
    fn qsort(arr2: [int], start2: int, end2: int) void {
      if (start2 == end2) return;
      if (start2 + 1 == end2) return;
      let mid: int = partition(arr2, start2, end2);
      qsort(arr2, start2, mid);
      qsort(arr2, mid + 1, end2);
    }
    qsort(arr0, 0, len(arr));
  }
  fn main() void {
    let arr: [int] = [6, 4, 1, 5, 3, 7, 9, 0, 2, 8];
    quicksort(arr);
    println(arr);
  }
  |}
  in
  let s = compile_and_run src 1000 in
  check string "quicksort" "[0,1,2,3,4,5,6,7,8,9]\n" s;
  ()

let preorder () =
  let src =
    {|
  struct Tree {
    val: int,
    left: Tree,
    right: Tree,

    fn iter(root: Tree) co int {
      co iter_impl(r: Tree) int {
        yield r.val;
        if (r.left) iter_impl(r.left);
        if (r.right) iter_impl(r.right);
      }
      return create(iter_impl, root);
    }
  }
  fn main() void {
    let t: Tree = new Tree{
      val: 0,
      left: new Tree{val: 1, left: null, right: null}, 
      right: new Tree{
        val: 2, 
        left: new Tree{val: 3, left: null, right: null}, 
        right: null},
    };
    for (x: Tree.iter(t)) println(x);
  }
  |}
  in
  let s = compile_and_run src 200 in
  check string "preorder" "0\n1\n2\n3\n" s;
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
          ("global_variable_complex_init", `Quick, global_variable_complex_init);
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
      ( "strings",
        [
          ("print_string", `Quick, print_string);
          ("string_index", `Quick, string_index);
          ("string_concat", `Quick, string_concat);
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
          ("return_coro", `Quick, return_coro);
        ] );
      ( "for loop",
        [
          ("for_loop", `Quick, for_loop);
          ("for_loop_break", `Quick, for_loop_break);
          ("for_loop_continue", `Quick, for_loop_continue);
          ("nested_for_loop", `Quick, nested_for_loop);
        ] );
      ( "other",
        [
          ("dynamic_call", `Quick, dynamic_call);
          ("polymorphism", `Quick, polymorphism);
          ("preorder", `Quick, preorder);
          ("quicksort", `Quick, quicksort);
        ] );
    ]
