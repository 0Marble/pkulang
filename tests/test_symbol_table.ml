open Alcotest
open Pkulang
open Pkulang.Ast
open Pkulang.SymbolTable
open Pkulang.SymbolTableBuilder

(* Alcotest testable for SymbolTable.symbol_kind *)
let symbol_kind : SymbolTable.symbol_kind Alcotest.testable =
  let pp_symbol_kind fmt kind =
    Format.pp_print_string fmt (SymbolTable.string_of_symbol_kind kind)
  in
  Alcotest.testable pp_symbol_kind ( = )

(* Alcotest testable for SymbolTable.scope_kind *)
let scope_kind : SymbolTable.scope_kind Alcotest.testable =
  let pp_scope_kind fmt kind =
    Format.pp_print_string fmt (SymbolTable.string_of_scope_kind kind)
  in
  Alcotest.testable pp_scope_kind ( = )
[@@warning "-32"]

let pp_typ fmt typ =
  Format.pp_print_string fmt (Ast.node_to_str (Ast.type_to_node typ))

let typ : Ast.typ Alcotest.testable = Alcotest.testable pp_typ ( = )
[@@warning "-32"]

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
    { stmts = [ let_stmt ]; node_idx = 3; loc = Location.Spot 0 }
  in

  let symtab = build_symbol_table program in
  let root = root_scope symtab in
  match find_in_scope root "x" with
  | Some sym ->
      assert (sym.name = "x");
      assert (sym.kind = Variable);
      print_endline "✓ Basic let test passed"
  | None -> failwith "Variable 'x' not found in root scope"

let test_function_declaration () =
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
        op = { str = "+"; kind = TokAdd; loc = Location.Spot 0 };
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
    { stmts = [ fn_decl ]; node_idx = 11; loc = Location.Spot 0 }
  in

  let symtab = build_symbol_table program in
  let root = root_scope symtab in
  match find_in_scope root "add" with
  | Some sym ->
      assert (sym.name = "add");
      assert (sym.kind = Function);
      print_endline "✓ Function declaration test passed"
  | None -> failwith "Function 'add' not found in root scope"
(* 
let test_nested_scopes () =
  let global_let : top_stmt = LetStmt {
    var_name = "x";
    var_type = NamedType { name = "int"; node_idx = 1; loc = Location.Spot 0 };
    value = NumExpr { num = 10; node_idx = 2; loc = Location.Spot 0 };
    node_idx = 3;
    loc = Location.Spot 0;
  } in

  let local_let : stmt = LetStmt {
    var_name = "x";
    var_type = NamedType { name = "int"; node_idx = 4; loc = Location.Spot 0 };
    value = NumExpr { num = 20; node_idx = 5; loc = Location.Spot 0 };
    node_idx = 6;
    loc = Location.Spot 0;
  } in

  let fn_body : stmt = Block {
    stmts = [local_let];
    node_idx = 7;
    loc = Location.Spot 0;
  } in

  let fn_decl : top_stmt = FnDecl {
    name = "test_fn";
    args = [];
    ret_type = NamedType { name = "void"; node_idx = 8; loc = Location.Spot 0 };
    body = fn_body;
    node_idx = 9;
    loc = Location.Spot 0;
  } in

  let program : root = {
    stmts = [global_let; fn_decl];
    node_idx = 10;
    loc = Location.Spot 0;
  } in

  let symtab = build_symbol_table program in
  let root = root_scope symtab in

  (* Check global variable *)
  (match find_in_scope root "x" with
   | Some sym ->
       assert (sym.name = "x");
       print_endline "✓ Global variable 'x' found"
   | None ->
       failwith "Global variable 'x' not found");

  (* Check function *)
  let fn_scope_id =
    match find_in_scope root "test_fn" with
    | Some { node_idx; _ } ->
        (* We must find the scope with Function "test_fn" *)
        let child = List.find_opt (fun sid ->
          match get_scope symtab sid with
          | Some scope -> scope.kind = Function "test_fn"
          | None -> false
        ) (child_scopes symtab root)
        in
        (match child with
         | Some sid -> sid
         | None -> failwith "Function scope for 'test_fn' not found")
    | None ->
        failwith "Function 'test_fn' not found in root scope"
  in

  (* Check shadowed local variable *)
  (match find_in_scope fn_scope_id "x" with
   | Some sym ->
       assert (sym.name = "x");
       print_endline "✓ Local shadowed variable 'x' found in function"
   | None ->
       failwith "Local variable 'x' not found in function");

  print_endline "✓ Nested scopes test passed"
 *)

let test_nested_scopes () =
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
    { stmts = [ global_let; fn_decl ]; node_idx = 10; loc = Location.Spot 0 }
  in

  let symtab = build_symbol_table program in
  let root_scope_obj = root_scope symtab in
  (* Use the new root_scope API *)

  (* Check global variable *)
  (match find_in_scope root_scope_obj "x" with
  (* Use the new find_in_scope API *)
  | Some sym ->
      check string "Global var name" "x" sym.name;
      check symbol_kind "Global var kind" Variable sym.kind;
      (* Add more specific checks if needed, e.g., sym.node_idx, sym.ty *)
      print_endline "✓ Global variable 'x' found"
  | None -> failwith "Global variable 'x' not found");

  (* Check the function symbol in the global scope *)
  let _ =
    match find_in_scope root_scope_obj "test_fn" with
    | Some sym ->
        check string "Function sym name" "test_fn" sym.name;
        check symbol_kind "Function sym kind" Function sym.kind;
        print_endline "✓ Function 'test_fn' symbol found in root scope";
        sym
    | None -> failwith "Function 'test_fn' symbol not found in root scope"
  in

  (* Find the actual function scope object using `scopes_by_kind` *)
  let fn_scope_obj =
    match scopes_by_kind symtab (FunctionScope "test_fn") with
    | [ s ] ->
        (* Expecting exactly one function scope named "test_fn" *)
        s
    | [] -> failwith "Function scope 'test_fn' not found in symbol table"
    | _ ->
        failwith "Multiple function scopes named 'test_fn' found, expected one"
  in
  let fn_body_scope =
    Option.get @@ find_child_scope_by_kind fn_scope_obj BlockScope
  in

  (* Check shadowed local variable within the function's scope *)
  (match find_in_scope fn_body_scope "x" with
  | Some sym ->
      check string "Local var name" "x" sym.name;
      check symbol_kind "Local var kind" Variable sym.kind;
      (* Verify it's the *local* 'x' by checking its node_idx or declared_in scope *)
      check int "Local var node_idx" 6 sym.node_idx;
      print_endline "✓ Local shadowed variable 'x' found in function"
  | None -> failwith "Local variable 'x' not found in function");

  print_endline "✓ Nested scopes test passed"

let run_tests () =
  print_endline "Running symbol table tests...";
  test_basic_let ();
  test_function_declaration ();
  test_nested_scopes ();
  print_endline "All tests passed! ✓"

let () = run_tests ()
