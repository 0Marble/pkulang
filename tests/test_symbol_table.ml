(* open Alcotest *)
open Pkulang
open Pkulang.Ast
open Pkulang.SymbolTable
open Pkulang.SymbolTableBuilder

let test_basic_let () =
  (* Create a simple let statement: let x : int = 42 *)
  let value_expr : expr =
    NumExpr { num = 42; node_idx = 1; loc = Location.Spot 0 }
  in

  let let_stmt : top_stmt =
    LetStmt
      {
        var_name = "x";
        var_type =
          NamedType { name = "int"; node_idx = 2; loc = Location.Spot 0 };
        value = value_expr;
        node_idx = 0;
        loc = Location.Spot 0;
      }
  in

  let program : root =
    {
      all_nodes = Hashtbl.create 64;
      stmts = [ let_stmt ];
      node_idx = 3;
      loc = Location.Spot 0;
    }
  in

  let symtab = build_symbol_table program in

  (* Test that we can find the variable *)
  match find_symbol symtab "x" with
  | Some sym ->
      assert (sym.name = "x");
      assert (sym.kind = `Variable);
      print_endline "✓ Basic let test passed"
  | None -> failwith "Variable 'x' not found in symbol table"

let test_function_declaration () =
  (* Create a simple function: fn add(a: int, b: int) -> int { return a + b; } *)
  let param_a =
    {
      name = "a";
      arg_type = NamedType { name = "int"; node_idx = 1; loc = Location.Spot 0 };
      node_idx = 2;
      loc = Location.Spot 0;
    }
  in

  let param_b =
    {
      name = "b";
      arg_type = NamedType { name = "int"; node_idx = 3; loc = Location.Spot 0 };
      node_idx = 4;
      loc = Location.Spot 0;
    }
  in

  let return_expr : expr =
    BinExpr
      {
        lhs = VarExpr { name = "a"; node_idx = 5; loc = Location.Spot 0 };
        rhs = VarExpr { name = "b"; node_idx = 6; loc = Location.Spot 0 };
        op = { str = "+"; loc = Location.Spot 0; kind = TokAdd };
        node_idx = 7;
        loc = Location.Spot 0;
      }
  in

  let return_stmt : stmt =
    ReturnStmt { value = Some return_expr; node_idx = 8; loc = Location.Spot 0 }
  in

  let fn_decl : top_stmt =
    FnDecl
      {
        name = "add";
        args = [ param_a; param_b ];
        ret_type =
          NamedType { name = "int"; node_idx = 9; loc = Location.Spot 0 };
        body = return_stmt;
        node_idx = 10;
        loc = Location.Spot 0;
      }
  in

  let program : root =
    {
      all_nodes = Hashtbl.create 64;
      stmts = [ fn_decl ];
      node_idx = 11;
      loc = Location.Spot 0;
    }
  in

  let symtab = build_symbol_table program in

  (* Test that we can find the function *)
  match find_symbol symtab "add" with
  | Some sym ->
      assert (sym.name = "add");
      assert (sym.kind = `Function);
      print_endline "✓ Function declaration test passed"
  | None -> failwith "Function 'add' not found in symbol table"

let test_nested_scopes () =
  (* Create a function with a local variable that shadows a global *)
  let global_let : top_stmt =
    LetStmt
      {
        var_name = "x";
        var_type =
          NamedType { name = "int"; node_idx = 1; loc = Location.Spot 0 };
        value = NumExpr { num = 10; node_idx = 2; loc = Location.Spot 0 };
        node_idx = 3;
        loc = Location.Spot 0;
      }
  in

  let local_let : stmt =
    LetStmt
      {
        var_name = "x";
        var_type =
          NamedType { name = "int"; node_idx = 4; loc = Location.Spot 0 };
        value = NumExpr { num = 20; node_idx = 5; loc = Location.Spot 0 };
        node_idx = 6;
        loc = Location.Spot 0;
      }
  in

  let fn_body : stmt =
    Block { stmts = [ local_let ]; node_idx = 7; loc = Location.Spot 0 }
  in

  let fn_decl : top_stmt =
    FnDecl
      {
        name = "test_fn";
        args = [];
        ret_type =
          NamedType { name = "void"; node_idx = 8; loc = Location.Spot 0 };
        body = fn_body;
        node_idx = 9;
        loc = Location.Spot 0;
      }
  in

  let program : root =
    {
      all_nodes = Hashtbl.create 64;
      stmts = [ global_let; fn_decl ];
      node_idx = 10;
      loc = Location.Spot 0;
    }
  in

  let symtab = build_symbol_table program in

  (* Test that we can find both the global and function *)
  (match find_symbol symtab "x" with
  | Some sym ->
      assert (sym.name = "x");
      assert (sym.kind = `Variable);
      print_endline "✓ Global variable found"
  | None -> failwith "Global variable 'x' not found");

  (match find_symbol symtab "test_fn" with
  | Some sym ->
      assert (sym.name = "test_fn");
      assert (sym.kind = `Function);
      print_endline "✓ Function found"
  | None -> failwith "Function 'test_fn' not found");

  print_endline "✓ Nested scopes test passed"

let run_tests () =
  print_endline "Running symbol table tests...";
  test_basic_let ();
  test_function_declaration ();
  test_nested_scopes ();
  print_endline "All tests passed! ✓"

let () = run_tests ()
