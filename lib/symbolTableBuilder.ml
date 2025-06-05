open Ast
open SymbolTable

let make_symbol ~name ~kind ~node_idx ~(ty : Ast.typ option) ~(loc : Location.location option) symtab =
  {
    name;
    kind;
    node_idx;
    ty;
    loc;
    declared_in = current_scope_kind symtab;
  }

let add_var (symtab : t) ~name ~node_idx ~(ty : Ast.typ option) ~(loc : Location.location option) : t =
  let var_info = make_symbol ~name ~kind:Variable ~node_idx ~ty ~loc symtab in
  add_symbol symtab var_info;
  symtab

let rec build_expr (symtab : t) (expr : expr) : t =
  match expr with
  | BinExpr x -> build_expr (build_expr symtab x.lhs) x.rhs
  | UnaryExpr x -> build_expr symtab x.sub_expr
  | CallExpr x -> x.params |> List.fold_left build_expr (build_expr symtab x.fn)
  | IndexExpr x -> x.idx |> List.fold_left build_expr (build_expr symtab x.arr)
  | DotExpr x -> build_expr symtab x.obj
  | ArrayLiteral x -> List.fold_left build_expr symtab x.elems
  | NewExpr x -> List.fold_left (fun acc f -> build_expr acc f.value) symtab x.fields
  | CreateExpr x -> x.params |> List.fold_left build_expr (build_expr symtab x.coroutine)
  | ResumeExpr x -> build_expr symtab x.coroutine
  | VarExpr _ | NumExpr _ | StringExpr _ | NullLiteral _ -> symtab

let rec build_stmt (symtab : t) (stmt : stmt) : t =
  match stmt with
  | Block x ->
        let symtab = enter_scope symtab BlockScope in
        let symtab = List.fold_left build_stmt symtab x.stmts in
        exit_scope symtab

  | LetStmt x ->
        let symtab = build_expr symtab x.value in
        add_var symtab ~name:x.var_name ~node_idx:x.node_idx ~ty:(Some x.var_type) ~loc:(Some x.loc)

  | ForLoop x ->
      let symtab = enter_scope symtab BlockScope in
      let symtab = add_var symtab ~name:x.iter_var ~node_idx:x.node_idx ~ty:None ~loc:(Some x.loc) in
      let symtab = build_expr symtab x.iterator in
      let symtab = build_stmt symtab x.body in
      let symtab = exit_scope symtab in
      symtab

  | WhileLoop x -> build_stmt (build_expr symtab x.condition) x.body

  | IfStmt x ->
      let symtab = build_expr symtab x.condition in
      let symtab_if_true = enter_scope symtab BlockScope in
      let symtab_if_true = build_stmt symtab_if_true x.if_true in
      let symtab_if_true_exit = exit_scope symtab_if_true in

      Option.fold ~none:symtab_if_true_exit
        ~some:(fun if_false_stmt ->
          let symtab_if_false = enter_scope symtab_if_true_exit BlockScope in
          let symtab_if_false = build_stmt symtab_if_false if_false_stmt in
          exit_scope symtab_if_false
        ) x.if_false

  | IfResumeStmt x ->
      let symtab_after_coroutine_expr = build_expr symtab x.coroutine in

      let symtab_entered_if_block = enter_scope symtab_after_coroutine_expr BlockScope in

      let symtab_with_var_in_block =
        Option.fold
          ~none:symtab_entered_if_block
          ~some:(fun var_name_str ->
            add_var symtab_entered_if_block ~name:var_name_str ~node_idx:x.node_idx ~ty:None ~loc:(Some x.loc)
          )
          x.var
      in

      let symtab_after_if_ok = build_stmt symtab_with_var_in_block x.if_ok in

      let symtab_after_if_ok_exit = exit_scope symtab_after_if_ok in

      Option.fold ~none:symtab_after_if_ok_exit
        ~some:(fun if_bad_stmt ->
          let symtab_if_bad = enter_scope symtab_after_if_ok_exit BlockScope in
          let symtab_if_bad = build_stmt symtab_if_bad if_bad_stmt in
          exit_scope symtab_if_bad
        ) x.if_bad


  | ReturnStmt x -> Option.fold ~none:symtab ~some:(build_expr symtab) x.value
  | YieldStmt x -> Option.fold ~none:symtab ~some:(build_expr symtab) x.value
  | Expr e -> build_expr symtab e
  | ContinueStmt _ | BreakStmt _ -> symtab

  | FnDecl x -> build_fn_decl symtab x
  | StructDecl x -> build_struct_decl symtab x
  | CoDecl x -> build_co_decl symtab x

  | AliasStmt x ->
      let alias_info = make_symbol ~name:x.type_name ~kind:Struct ~node_idx:x.node_idx
                         ~ty:(Some x.other_type) ~loc:(Some x.loc) symtab in
      add_symbol symtab alias_info;
      symtab

and build_fn_decl (symtab : t) (fn : fn_decl) : t =
  let fn_info = make_symbol ~name:fn.name ~kind:Function ~node_idx:fn.node_idx
                  ~ty:(Some (FnType {args = List.map (fun arg -> arg.arg_type) fn.args; ret = fn.ret_type; node_idx = fn.node_idx; loc = fn.loc})) (* Corrected to arg.arg_type *)
                  ~loc:(Some fn.loc) symtab in
  add_symbol symtab fn_info;

  let symtab = enter_scope symtab (FunctionScope fn.name) in
  let symtab =
    List.fold_left
      (fun (acc : t) (p : Ast.argument) ->
        add_var acc ~name:p.name ~node_idx:p.node_idx ~ty:(Some p.arg_type) ~loc:(Some p.loc)) (* Corrected to p.arg_type *)
      symtab fn.args
  in
  let symtab = build_stmt symtab fn.body in
  exit_scope symtab

and build_co_decl (symtab : t) (co : co_decl) : t =
  let co_info = make_symbol ~name:co.name ~kind:Function ~node_idx:co.node_idx
                  ~ty:(Some (CoType {args = List.map (fun arg -> arg.arg_type) co.args; yield = co.yield_type; node_idx = co.node_idx; loc = co.loc})) (* Corrected to arg.arg_type *)
                  ~loc:(Some co.loc) symtab in
  add_symbol symtab co_info;

  let symtab = enter_scope symtab (FunctionScope co.name) in
  let symtab =
    List.fold_left
      (fun (acc : t) (p : Ast.argument) -> (* p is Ast.argument *)
        add_var acc ~name:p.name ~node_idx:p.node_idx ~ty:(Some p.arg_type) ~loc:(Some p.loc)) (* Corrected to p.arg_type *)
      symtab co.args
  in
  let symtab = build_stmt symtab co.body in
  exit_scope symtab

and build_struct_decl (symtab : t) (s : struct_decl) : t =
  let struct_info = make_symbol ~name:s.name ~kind:Struct ~node_idx:s.node_idx
                      ~ty:None
                      ~loc:(Some s.loc) symtab in
  add_symbol symtab struct_info;

  let symtab = enter_scope symtab (StructScope s.name) in
  let symtab = List.fold_left build_decl symtab s.decls in
  exit_scope symtab

and build_decl (symtab : t) (decl : decl) : t =
  match decl with
  | FnDecl fn -> build_fn_decl symtab fn
  | CoDecl co -> build_co_decl symtab co
  | LetStmt let_stmt ->
        let symtab = build_expr symtab let_stmt.value in
        add_var symtab ~name:let_stmt.var_name ~node_idx:let_stmt.node_idx
          ~ty:(Some let_stmt.var_type) ~loc:(Some let_stmt.loc)
  | StructDecl s -> build_struct_decl symtab s
  | Field f ->
      let symtab = add_var symtab ~name:f.var_name ~node_idx:f.node_idx
                     ~ty:(Some f.field_type) ~loc:(Some f.loc) in
      Option.fold ~none:symtab ~some:(build_expr symtab) f.value

let build_top_stmt (symtab : t) (stmt : top_stmt) : t =
  match stmt with
  | FnDecl fn -> build_fn_decl symtab fn
  | StructDecl s -> build_struct_decl symtab s
  | CoDecl co -> build_co_decl symtab co
  | LetStmt let_stmt ->
        let symtab = build_expr symtab let_stmt.value in
        add_var symtab ~name:let_stmt.var_name ~node_idx:let_stmt.node_idx
          ~ty:(Some let_stmt.var_type) ~loc:(Some let_stmt.loc)
  | AliasStmt x ->
      let alias_info = make_symbol ~name:x.type_name ~kind:Struct ~node_idx:x.node_idx
                         ~ty:(Some x.other_type) ~loc:(Some x.loc) symtab in
      add_symbol symtab alias_info;
      symtab

let build_symbol_table (program : root) : t =
  let symtab = SymbolTable.create () in
  List.fold_left build_top_stmt symtab program.stmts