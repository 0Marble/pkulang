type expr_value = Operand of Runtime.operand | Address of Runtime.jump_target

let codegen (src : string) (root : Ast.root) (symtab : SymbolTable.t) :
    Runtime.runtime =
  let cmds =
    Array.init 16384 (fun _ ->
        ({ cmd = Runtime.Trap; loc = Location.Spot 0 } : Runtime.command))
  in
  let ptr = ref 0 in
  let cur_scope = ref symtab.root in
  let function_addresses = Hashtbl.create 64 in

  let emit (cmd : Runtime.command) : unit =
    cmds.(!ptr) <- cmd;
    ptr := !ptr + 1
  in
  let temp_register (registers : (int, Stack.location) Hashtbl.t) :
      Stack.location =
    let reg = Hashtbl.length registers in
    Hashtbl.add registers (-reg) (Register reg);
    Register reg
  in

  let expr_to_operand (e : expr_value) : Runtime.operand =
    match e with Operand e -> e | _ -> failwith "not an operand"
  in
  let expr_to_address (e : expr_value) : Runtime.jump_target =
    match e with Address e -> e | _ -> failwith "not an operand"
  in

  SymbolTable.add_symbol symtab
    {
      name = "print_int";
      kind = Function;
      node_idx = 10;
      (* node indices start with 100*)
      ty = None;
      declared_in = GlobalScope;
      loc = None;
    };
  let print_loc = !ptr in
  emit
    {
      cmd = Builtin ([| Location (Argument 0) |], "print");
      loc = Location.Spot 0;
    };
  emit { cmd = Ret Null; loc = Location.Spot 0 };
  Hashtbl.add function_addresses 10 print_loc;

  let rec codegen_root (root : Ast.root) = List.iter codegen_top_stmt root.stmts
  and codegen_top_stmt (stmt : Ast.top_stmt) =
    match stmt with
    | Ast.FnDecl x -> codegen_fn x
    | Ast.StructDecl _ -> failwith "Todo"
    | Ast.CoDecl x -> codegen_co x
    | Ast.LetStmt _ -> failwith "Todo"
    | Ast.AliasStmt _ -> failwith "Todo"
  and codegen_fn (f : Ast.fn_decl) =
    let registers = Hashtbl.create 64 in
    let backpatch_locals_cnt = !ptr in
    Hashtbl.add function_addresses f.node_idx !ptr;
    let prev_scope = !cur_scope in
    cur_scope :=
      SymbolTable.find_child_scope_by_kind !cur_scope (FunctionScope f.name)
      |> Option.get;
    emit { cmd = Trap; loc = Location.Spot 0 };
    List.iteri (fun i arg -> codegen_arg i arg registers) f.args;
    codegen_stmt f.body registers;
    let locals_cnt = Hashtbl.length registers in
    cmds.(backpatch_locals_cnt) <- { cmd = Alloca locals_cnt; loc = f.loc };
    cur_scope := prev_scope;
    emit { cmd = Ret Null; loc = f.loc };
    ()
  and codegen_co (f : Ast.co_decl) =
    let registers = Hashtbl.create 64 in
    let backpatch_locals_cnt = !ptr in
    Hashtbl.add function_addresses f.node_idx !ptr;
    let prev_scope = !cur_scope in
    cur_scope :=
      SymbolTable.find_child_scope_by_kind !cur_scope (FunctionScope f.name)
      |> Option.get;
    emit { cmd = Trap; loc = Location.Spot 0 };
    List.iteri (fun i arg -> codegen_arg i arg registers) f.args;
    codegen_stmt f.body registers;
    let locals_cnt = Hashtbl.length registers in
    cmds.(backpatch_locals_cnt) <- { cmd = Alloca locals_cnt; loc = f.loc };
    cur_scope := prev_scope;
    emit { cmd = Ret Null; loc = f.loc };
    ()
  and codegen_arg i (a : Ast.argument)
      (registers : (int, Stack.location) Hashtbl.t) =
    Hashtbl.add registers a.node_idx (Argument i);
    ()
  and codegen_stmt (s : Ast.stmt) registers =
    match s with
    | Ast.Block x ->
        let prev_scope = !cur_scope in
        (*TODO: how do I find this scope? *)
        cur_scope :=
          SymbolTable.find_child_scope_by_kind !cur_scope BlockScope
          |> Option.get;
        List.iter (fun s -> codegen_stmt s registers) x.stmts;
        cur_scope := prev_scope
    | Ast.LetStmt x ->
        let reg = Hashtbl.length registers in
        Hashtbl.add registers x.node_idx (Register reg);
        let value = codegen_expr x.value registers in
        emit
          { cmd = Assign (Register reg, value |> expr_to_operand); loc = x.loc }
    | Ast.ForLoop _ -> failwith "Todo"
    | Ast.WhileLoop x ->
        let start_addr = !ptr in
        let cond = codegen_expr x.condition registers |> expr_to_operand in
        let backpatch_goto_end = !ptr in
        emit { cmd = Trap; loc = Location.Spot 0 };
        codegen_stmt x.body registers;
        emit { cmd = Goto (Static start_addr); loc = x.loc };
        cmds.(backpatch_goto_end) <-
          { cmd = GotoIfZero (cond, Static !ptr); loc = x.loc }
    | Ast.ContinueStmt _ -> failwith "Todo"
    | Ast.BreakStmt _ -> failwith "Todo"
    | Ast.IfStmt x -> (
        let cond = codegen_expr x.condition registers |> expr_to_operand in
        let backpatch_goto_false = !ptr in
        emit { cmd = Trap; loc = Location.Spot 0 };
        codegen_stmt x.if_true registers;
        match x.if_false with
        | Some if_false ->
            let backpatch_goto_end = !ptr in
            emit { cmd = Trap; loc = Location.Spot 0 };
            cmds.(backpatch_goto_false) <-
              { cmd = GotoIfZero (cond, Static !ptr); loc = x.loc };
            codegen_stmt if_false registers;
            cmds.(backpatch_goto_end) <-
              { cmd = Goto (Static !ptr); loc = x.loc }
        | None ->
            cmds.(backpatch_goto_false) <-
              { cmd = GotoIfZero (cond, Static !ptr); loc = x.loc })
    | Ast.ReturnStmt x ->
        let ret =
          match x.value with
          | Some v -> codegen_expr v registers |> expr_to_operand
          | None -> Null
        in
        emit { cmd = Ret ret; loc = x.loc }
    | Ast.Expr x ->
        let _ = codegen_expr x registers in
        ()
    | Ast.FnDecl _ -> failwith "Todo"
    | Ast.StructDecl _ -> failwith "Todo"
    | Ast.CoDecl _ -> failwith "Todo"
    | Ast.AliasStmt _ -> failwith "Todo"
    | Ast.IfResumeStmt x ->
        let parent_scope = !cur_scope in
        let inner_scopes = !(parent_scope.children) |> List.rev in
        let if_ok_scope = List.hd inner_scopes in
        cur_scope := if_ok_scope;
        let coro = codegen_expr x.coroutine registers |> expr_to_operand in

        let var_dest =
          match x.var with
          | Some _ ->
              let reg = Hashtbl.length registers in
              Hashtbl.add registers x.node_idx (Register reg);
              Stack.Register reg
          | None -> Stack.Void
        in
        let backpatch_if_bad = !ptr in
        emit { cmd = Trap; loc = Location.Spot 0 };
        codegen_stmt x.if_ok registers;

        (match x.if_bad with
        | Some if_bad ->
            let if_bad_scope = List.nth inner_scopes 1 in
            cur_scope := if_bad_scope;
            let backpatch_goto_end = !ptr in
            emit { cmd = Trap; loc = Location.Spot 0 };
            cmds.(backpatch_if_bad) <-
              { cmd = Resume (var_dest, coro, Static !ptr); loc = x.loc };
            codegen_stmt if_bad registers;
            cmds.(backpatch_goto_end) <-
              { cmd = Goto (Static !ptr); loc = x.loc }
        | None ->
            cmds.(backpatch_if_bad) <-
              { cmd = Resume (var_dest, coro, Static !ptr); loc = x.loc });
        cur_scope := parent_scope
    | Ast.YieldStmt x ->
        let v =
          match x.value with
          | Some v -> codegen_expr v registers |> expr_to_operand
          | None -> Runtime.Null
        in
        emit { cmd = Yield v; loc = x.loc }
  and codegen_expr (e : Ast.expr) (registers : (int, Stack.location) Hashtbl.t)
      : expr_value =
    match e with
    | Ast.BinExpr x -> (
        let lhs = codegen_expr x.lhs registers |> expr_to_operand in
        let rhs = codegen_expr x.rhs registers |> expr_to_operand in
        match x.op.kind with
        | Tokenizer.TokAdd ->
            let reg = temp_register registers in
            emit { cmd = Add (reg, lhs, rhs); loc = x.loc };
            Operand (Location reg)
        | Tokenizer.TokSub ->
            let reg = temp_register registers in
            emit { cmd = Sub (reg, lhs, rhs); loc = x.loc };
            Operand (Location reg)
        | Tokenizer.TokLt ->
            let reg = temp_register registers in
            emit { cmd = Lt (reg, lhs, rhs); loc = x.loc };
            Operand (Location reg)
        | Tokenizer.TokAssign ->
            let lhs =
              match lhs with
              | Location lhs -> lhs
              | _ ->
                  Error.fail_at_spot
                    "Can not assign to a value with no location" src x.loc
                    (Error.Error Unknown)
            in
            emit { cmd = Assign (lhs, rhs); loc = x.loc };
            Operand (Location lhs)
        | _ -> failwith "Todo")
    | Ast.UnaryExpr _ -> failwith "Todo"
    | Ast.CallExpr x ->
        let reg = temp_register registers in
        let (fn : Runtime.jump_target) =
          codegen_expr x.fn registers |> expr_to_address
        in
        let args =
          List.map
            (fun arg -> codegen_expr arg registers |> expr_to_operand)
            x.params
        in
        emit { cmd = Call (reg, Array.of_list args, fn); loc = x.loc };
        Operand (Location reg)
    | Ast.IndexExpr _ -> failwith "Todo"
    | Ast.DotExpr _ -> failwith "Todo"
    | Ast.VarExpr x -> (
        let info =
          match SymbolTable.find_in_scope !cur_scope x.name with
          | Some info -> info
          | None ->
              Error.fail_at_spot "Undefined variable" src x.loc
                (Error.Error Unknown)
        in
        match info.kind with
        | Variable -> (
            match Hashtbl.find_opt registers info.node_idx with
            | Some reg -> Operand (Location reg)
            | None -> failwith "unreachable")
        | Function -> (
            match Hashtbl.find_opt function_addresses info.node_idx with
            | Some addr -> Address (Static addr)
            | None ->
                Error.fail_at_spot "Undefined variable" src x.loc
                  (Error.Error Unknown))
        | _ -> failwith "Todo")
    | Ast.NumExpr x -> Operand (Number x.num)
    | Ast.StringExpr _ -> failwith "Todo"
    | Ast.ArrayLiteral _ -> failwith "Todo"
    | Ast.NullLiteral _ -> failwith "Todo"
    | Ast.NewExpr _ -> failwith "Todo"
    | Ast.CreateExpr x ->
        let reg = temp_register registers in
        let coro_ptr = codegen_expr x.coroutine registers |> expr_to_address in
        let args =
          List.map
            (fun a -> codegen_expr a registers |> expr_to_operand)
            x.params
        in
        emit { cmd = Create (reg, Array.of_list args, coro_ptr); loc = x.loc };
        Operand (Location reg)
    | Ast.ResumeExpr _ -> failwith "Todo"
  in

  codegen_root root;
  let main_fn =
    match SymbolTable.find_in_scope symtab.root "main" with
    | Some info when info.kind = Function ->
        Hashtbl.find function_addresses info.node_idx
    | _ -> failwith "Missing `main' function"
  in
  let start = !ptr in
  emit { cmd = Call (Void, [||], Static main_fn); loc = Location.Spot 0 };
  emit { cmd = Halt; loc = Location.Spot 0 };
  Runtime.create src (Array.sub cmds 0 (!ptr + 1)) start

