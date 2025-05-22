open Ast
open SymbolTable

let rec build_expr (symtab : t) (expr : expr) : t =
  match expr with
  | BinExpr x -> 
      let symtab = build_expr symtab x.lhs in
      build_expr symtab x.rhs
  | UnaryExpr x -> 
      build_expr symtab x.sub_expr
  | CallExpr x ->
      let symtab = build_expr symtab x.fn in
      List.fold_left build_expr symtab x.params
  | IndexExpr x ->
      let symtab = build_expr symtab x.arr in
      List.fold_left build_expr symtab x.idx
  | DotExpr x ->
      build_expr symtab x.obj
  | ArrayLiteral x ->
      List.fold_left build_expr symtab x.elems
  | NewExpr x ->
      List.fold_left (fun acc field -> build_expr acc field.value) symtab x.fields
  | CreateExpr x ->
      let symtab = build_expr symtab x.coroutine in
      List.fold_left build_expr symtab x.params
  | ResumeExpr x ->
      build_expr symtab x.coroutine
  | VarExpr _ | NumExpr _ | StringExpr _ | NullLiteral _ ->
      symtab

let rec build_stmt (symtab : t) (stmt : stmt) : t =
  match stmt with
  | Block x ->
      enter_scope symtab (Block x.node_idx);
      let symtab = List.fold_left build_stmt symtab x.stmts in
      exit_scope symtab;
      symtab
      
  | LetStmt x ->
      let symtab = build_expr symtab x.value in
      let var_info = {
        name = x.var_name;
        kind = `Variable;
        node_idx = x.node_idx;
        ty = None; (* Could extract type from x.var_type if needed *)
        declared_in = current_scope_kind symtab;
      } in
      add_symbol symtab var_info;
      symtab
      
  | ForLoop x ->
      enter_scope symtab (Block x.node_idx);
      (* Add iterator variable *)
      let iter_info = {
        name = x.iter_var;
        kind = `Variable;
        node_idx = x.node_idx;
        ty = None;
        declared_in = current_scope_kind symtab;
      } in
      add_symbol symtab iter_info;
      let symtab = build_expr symtab x.iterator in
      let symtab = build_stmt symtab x.body in
      exit_scope symtab;
      symtab
      
  | WhileLoop x ->
      let symtab = build_expr symtab x.condition in
      build_stmt symtab x.body
      
  | IfStmt x ->
      let symtab = build_expr symtab x.condition in
      let symtab = build_stmt symtab x.if_true in
      (match x.if_false with
       | Some stmt -> build_stmt symtab stmt
       | None -> symtab)
       
  | IfResumeStmt x ->
      let symtab = build_expr symtab x.coroutine in
      (* Add variable if present *)
      let symtab = 
        match x.var with
        | Some var_name ->
            let var_info = {
              name = var_name;
              kind = `Variable;
              node_idx = x.node_idx;
              ty = None;
              declared_in = current_scope_kind symtab;
            } in
            add_symbol symtab var_info;
            symtab
        | None -> symtab
      in
      let symtab = build_stmt symtab x.if_ok in
      (match x.if_bad with
       | Some stmt -> build_stmt symtab stmt
       | None -> symtab)
       
  | ReturnStmt x ->
      (match x.value with
       | Some expr -> build_expr symtab expr
       | None -> symtab)
       
  | YieldStmt x ->
      (match x.value with
       | Some expr -> build_expr symtab expr
       | None -> symtab)
       
  | Expr expr ->
      build_expr symtab expr
      
  | FnDecl x ->
      build_fn_decl symtab x
      
  | StructDecl x ->
      build_struct_decl symtab x
      
  | CoDecl x ->
      build_co_decl symtab x
      
  | AliasStmt x ->
      let alias_info = {
        name = x.type_name;
        kind = `Struct; (* Using Struct kind for type aliases *)
        node_idx = x.node_idx;
        ty = None;
        declared_in = current_scope_kind symtab;
      } in
      add_symbol symtab alias_info;
      symtab
      
  | ContinueStmt _ | BreakStmt _ ->
      symtab

and build_fn_decl (symtab : t) (fn : fn_decl) : t =
  (* Register the function itself in current scope *)
  let function_info = {
    name = fn.name;
    kind = `Function;
    node_idx = fn.node_idx;
    ty = None;
    declared_in = current_scope_kind symtab;
  } in
  add_symbol symtab function_info;

  (* Enter function scope *)
  enter_scope symtab (Function fn.name);

  (* Add function parameters *)
  List.iter (fun (param : argument) ->
    let param_info = {
      name = param.name;
      kind = `Variable;
      node_idx = param.node_idx;
      ty = None;
      declared_in = current_scope_kind symtab;
    } in
    add_symbol symtab param_info
  ) fn.args;

  (* Process function body *)
  let symtab = build_stmt symtab fn.body in

  (* Exit function scope *)
  exit_scope symtab;
  symtab

and build_co_decl (symtab : t) (co : co_decl) : t =
  (* Register the coroutine itself in current scope *)
  let co_info = {
    name = co.name;
    kind = `Function; (* Treat coroutines as special functions *)
    node_idx = co.node_idx;
    ty = None;
    declared_in = current_scope_kind symtab;
  } in
  add_symbol symtab co_info;

  (* Enter coroutine scope *)
  enter_scope symtab (Function co.name);

  (* Add coroutine parameters *)
  List.iter (fun (param : argument) ->
    let param_info = {
      name = param.name;
      kind = `Variable;
      node_idx = param.node_idx;
      ty = None;
      declared_in = current_scope_kind symtab;
    } in
    add_symbol symtab param_info
  ) co.args;

  (* Process coroutine body *)
  let symtab = build_stmt symtab co.body in

  (* Exit coroutine scope *)
  exit_scope symtab;
  symtab

and build_struct_decl (symtab : t) (struct_decl : struct_decl) : t =
  (* Register the struct itself *)
  let struct_info = {
    name = struct_decl.name;
    kind = `Struct;
    node_idx = struct_decl.node_idx;
    ty = None;
    declared_in = current_scope_kind symtab;
  } in
  add_symbol symtab struct_info;

  (* Process struct declarations (nested functions, etc.) *)
  List.fold_left build_decl symtab struct_decl.decls

and build_decl (symtab : t) (decl : decl) : t =
  match decl with
  | FnDecl fn -> build_fn_decl symtab fn
  | CoDecl co -> build_co_decl symtab co
  | LetStmt let_stmt -> 
      let symtab = build_expr symtab let_stmt.value in
      let var_info = {
        name = let_stmt.var_name;
        kind = `Variable;
        node_idx = let_stmt.node_idx;
        ty = None;
        declared_in = current_scope_kind symtab;
      } in
      add_symbol symtab var_info;
      symtab
  | StructDecl struct_decl -> build_struct_decl symtab struct_decl
  | Field field ->
      let var_info = {
        name = field.var_name;
        kind = `Variable;
        node_idx = field.node_idx;
        ty = None;
        declared_in = current_scope_kind symtab;
      } in
      add_symbol symtab var_info;
      (match field.value with
       | Some expr -> build_expr symtab expr
       | None -> symtab)

let build_top_stmt (symtab : t) (top_stmt : top_stmt) : t =
  match top_stmt with
  | FnDecl fn -> build_fn_decl symtab fn
  | StructDecl struct_decl -> build_struct_decl symtab struct_decl
  | CoDecl co -> build_co_decl symtab co
  | LetStmt let_stmt ->
      let symtab = build_expr symtab let_stmt.value in
      let var_info = {
        name = let_stmt.var_name;
        kind = `Variable;
        node_idx = let_stmt.node_idx;
        ty = None;
        declared_in = current_scope_kind symtab;
      } in
      add_symbol symtab var_info;
      symtab
  | AliasStmt alias ->
      let alias_info = {
        name = alias.type_name;
        kind = `Struct;
        node_idx = alias.node_idx;
        ty = None;
        declared_in = current_scope_kind symtab;
      } in
      add_symbol symtab alias_info;
      symtab

let build_symbol_table (program : root) : SymbolTable.t =
  let symtab = SymbolTable.create () in
  List.fold_left build_top_stmt symtab program.stmts