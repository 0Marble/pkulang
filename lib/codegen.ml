let codegen (src : string) (fn_list : Ast.node list)
    (get_definition : Ast.node -> Ast.node) (root : Ast.root) : Runtime.runtime
    =
  let cmds =
    Array.init 65536 (fun _ ->
        ({ cmd = Runtime.Trap; loc = Location.Spot 0 } : Runtime.command))
  in
  let trap_for_resume : Runtime.jump_target = Static 0 in

  let ptr = ref 1 in
  let emit cmd =
    cmds.(!ptr) <- cmd;
    ptr := !ptr + 1
  in
  let patch cmd ptr = cmds.(ptr) <- { cmd; loc = cmds.(ptr).loc } in

  let (function_table : (Ast.node, int) Hashtbl.t) = Hashtbl.create 64 in
  List.iter
    (fun f ->
      Hashtbl.add function_table f !ptr;
      emit { cmd = Trap; loc = Ast.node_loc f })
    fn_list;

  let allocate_reg node_idx registers =
    let reg = Stack.Register (Hashtbl.length registers) in
    Hashtbl.add registers node_idx reg;
    reg
  in
  let op_to_jump (op : Runtime.operand) : Runtime.jump_target =
    match op with
    | Location y -> Dynamic y
    | Number y -> Static y
    | _ -> failwith "Error: unsupported function pointer"
  in

  let rec codegen_expr e registers =
    try codegen_expr' e registers
    with err ->
      Error.fail_at_spot "Codegen error" src
        (Ast.expr_to_node e |> Ast.node_loc)
        err
  and codegen_expr' (e : Ast.expr) (registers : (int, Stack.location) Hashtbl.t)
      : Runtime.operand =
    match e with
    | BinExpr x when x.op.kind = TokAssign -> (
        let rhs = codegen_expr x.rhs registers in
        match x.lhs with
        | DotExpr y -> (
            let obj =
              match codegen_expr y.obj registers with
              | Location obj -> obj
              | _ -> failwith "Error: not an object"
            in
            let def = get_definition (DotExpr y) in
            match def with
            | Field z ->
                let reg = allocate_reg y.node_idx registers in
                emit { cmd = FieldSet (obj, z.var_name, rhs); loc = x.loc };
                Location reg
            | LetStmt _ -> failwith "Todo: assign to static vars"
            | _ -> failwith "Error: unsupported assignment to dot expr")
        | IndexExpr y ->
            let rec index_set_expr arr ids =
              match ids with
              | [] -> failwith "Error: no indices given"
              | [ idx ] ->
                  let idx = codegen_expr idx registers in
                  emit { cmd = IndexSet (arr, idx, rhs); loc = y.loc };
                  arr
              | idx :: ids ->
                  let idx = codegen_expr idx registers in
                  emit { cmd = IndexGet (arr, Location arr, idx); loc = y.loc };
                  index_set_expr arr ids
            in
            let arr = codegen_expr y.arr registers in
            let reg = allocate_reg y.node_idx registers in
            emit { cmd = Assign (reg, arr); loc = y.loc };
            Location (index_set_expr reg y.idx)
        | _ -> (
            let lhs = codegen_expr x.lhs registers in
            match lhs with
            | Location loc ->
                emit { cmd = Assign (loc, rhs); loc = x.loc };
                Location loc
            | _ -> failwith "Error: not an lvalue"))
    | BinExpr x ->
        let lhs = codegen_expr x.lhs registers in
        let rhs = codegen_expr x.rhs registers in
        let res = allocate_reg x.node_idx registers in
        let (cmd : Runtime.command_kind) =
          match x.op.kind with
          | TokAdd -> Add (res, lhs, rhs)
          | TokSub -> Sub (res, lhs, rhs)
          | TokMul -> Mul (res, lhs, rhs)
          | TokDiv -> Div (res, lhs, rhs)
          | TokEq -> Eql (res, lhs, rhs)
          | TokLt -> Lt (res, lhs, rhs)
          | TokGt ->
              emit { cmd = Lt (res, lhs, rhs); loc = x.loc };
              Sub (res, lhs, rhs)
          | TokNeq ->
              emit { cmd = Eql (res, lhs, rhs); loc = x.loc };
              Sub (res, Number 1, Location res)
          | _ -> failwith "Error: unsupported binop"
        in
        emit { cmd; loc = x.loc };
        Location res
    | UnaryExpr x ->
        let sub = codegen_expr x.sub_expr registers in
        let reg = allocate_reg x.node_idx registers in
        (match x.op.kind with
        | TokSub -> emit { cmd = Sub (reg, Number 0, sub); loc = x.loc }
        | TokNot -> emit { cmd = Sub (reg, Number 1, sub); loc = x.loc }
        | _ -> failwith "Error: unsupported unop");
        Location reg
    | CallExpr x ->
        let fptr = codegen_expr x.fn registers |> op_to_jump in
        let res = allocate_reg x.node_idx registers in
        let args =
          List.map (fun a -> codegen_expr a registers) x.params |> Array.of_list
        in
        emit { cmd = Call (res, args, fptr); loc = x.loc };
        Location res
    | IndexExpr x ->
        let arr = codegen_expr x.arr registers in
        let reg = allocate_reg x.node_idx registers in
        emit { cmd = Assign (reg, arr); loc = x.loc };
        let elem =
          List.fold_left
            (fun arr idx ->
              let idx = codegen_expr idx registers in
              emit { cmd = IndexGet (arr, Location arr, idx); loc = x.loc };
              arr)
            reg x.idx
        in
        Location elem
    | DotExpr x -> (
        let obj = codegen_expr x.obj registers in
        let def = get_definition (DotExpr x) in
        match def with
        | Field y ->
            let reg = allocate_reg x.node_idx registers in
            emit { cmd = FieldGet (reg, obj, y.var_name); loc = x.loc };
            Location reg
        | LetStmt _ -> failwith "Todo: static vars"
        | FnDecl y -> Number (Hashtbl.find function_table (FnDecl y))
        | CoDecl y -> Number (Hashtbl.find function_table (CoDecl y))
        | _ -> failwith "Error: unsupported definition for field")
    | VarExpr x -> (
        let def = get_definition (VarExpr x) in
        match def with
        | LetStmt y -> Location (Hashtbl.find registers y.node_idx)
        | ForLoop y -> Location (Hashtbl.find registers y.node_idx)
        | FnDecl y -> Number (Hashtbl.find function_table (FnDecl y))
        | CoDecl y -> Number (Hashtbl.find function_table (CoDecl y))
        | _ -> failwith "Error: unsupported definition for var")
    | NumExpr x -> Number x.num
    | StringExpr x ->
        let reg = allocate_reg x.node_idx registers in
        emit { cmd = New reg; loc = x.loc };
        emit { cmd = StringLiteral (reg, x.str); loc = x.loc };
        Location reg
    | ArrayLiteral x ->
        let arr = allocate_reg x.node_idx registers in
        emit { cmd = New arr; loc = x.loc };
        emit { cmd = Resize (arr, Number (List.length x.elems)); loc = x.loc };
        List.iteri
          (fun i elem ->
            let elem = codegen_expr elem registers in
            emit { cmd = IndexSet (arr, Number i, elem); loc = x.loc })
          x.elems;
        Location arr
    | NullLiteral _ -> Null
    | NewExpr x ->
        let obj = allocate_reg x.node_idx registers in
        emit { cmd = New obj; loc = x.loc };
        List.iter
          (fun (field : Ast.field_literal) ->
            let elem = codegen_expr field.value registers in
            emit { cmd = FieldSet (obj, field.name, elem); loc = x.loc })
          x.fields;
        Location obj
    | CreateExpr x ->
        let fptr = codegen_expr x.coroutine registers |> op_to_jump in
        let res = allocate_reg x.node_idx registers in
        let args =
          List.map (fun a -> codegen_expr a registers) x.params |> Array.of_list
        in
        emit { cmd = Create (res, args, fptr); loc = x.loc };
        Location res
    | ResumeExpr x ->
        let res = allocate_reg x.node_idx registers in
        let coro = codegen_expr x.coroutine registers in
        emit { cmd = Resume (res, coro, trap_for_resume); loc = x.loc };
        Location res
  and codegen_stmt this_loop_start s registers =
    try codegen_stmt' this_loop_start s registers
    with err ->
      Error.fail_at_spot "Codegen error" src
        (Ast.stmt_to_node s |> Ast.node_loc)
        err
  and codegen_stmt' (this_loop_start : int option) (s : Ast.stmt) registers :
      int list =
    match s with
    | Block x ->
        List.map (fun y -> codegen_stmt this_loop_start y registers) x.stmts
        |> List.concat
    | LetStmt x ->
        let reg = allocate_reg x.node_idx registers in
        let value = codegen_expr x.value registers in
        emit { cmd = Assign (reg, value); loc = x.loc };
        []
    | ForLoop x ->
        let coro = codegen_expr x.iterator registers in
        let reg = allocate_reg x.node_idx registers in
        let start = !ptr in
        emit { cmd = Trap; loc = x.loc };
        let breaks = codegen_stmt (Some start) x.body registers in
        emit { cmd = Goto (Static start); loc = x.loc };
        List.iter (patch (Goto (Static !ptr))) breaks;
        patch (Resume (reg, coro, Static !ptr)) start;
        []
    | WhileLoop x ->
        let start = !ptr in
        let cond = codegen_expr x.condition registers in
        let jump_to_end = !ptr in
        emit { cmd = Trap; loc = x.loc };
        let breaks = codegen_stmt (Some start) x.body registers in
        emit { cmd = Goto (Static start); loc = x.loc };
        List.iter (patch (Goto (Static !ptr))) breaks;
        cmds.(jump_to_end) <-
          { cmd = GotoIfZero (cond, Static !ptr); loc = x.loc };
        []
    | ContinueStmt x -> (
        match this_loop_start with
        | Some start ->
            emit { cmd = Goto (Static start); loc = x.loc };
            []
        | None -> failwith "Error: continue outside of loop")
    | BreakStmt x ->
        if this_loop_start = None then failwith "Error: break outside of loop"
        else ();
        emit { cmd = Trap; loc = x.loc };
        [ !ptr ]
    | IfStmt x -> (
        let cond = codegen_expr x.condition registers in
        let if_true_start = !ptr in
        emit { cmd = Trap; loc = x.loc };
        let if_true_breaks = codegen_stmt this_loop_start x.if_true registers in
        match x.if_false with
        | Some if_false ->
            let if_true_end = !ptr in
            emit { cmd = Trap; loc = x.loc };
            patch (GotoIfZero (cond, Static !ptr)) if_true_start;
            let if_false_breaks =
              codegen_stmt this_loop_start if_false registers
            in
            patch (Goto (Static !ptr)) if_true_end;
            List.append if_false_breaks if_true_breaks
        | None ->
            patch (GotoIfZero (cond, Static !ptr)) if_true_start;
            if_true_breaks)
    | ReturnStmt x ->
        let res =
          x.value
          |> Option.map (fun v -> codegen_expr v registers)
          |> Option.value ~default:Runtime.Null
        in
        emit { cmd = Ret res; loc = x.loc };
        []
    | Expr x ->
        let _ = codegen_expr x registers in
        []
    | FnDecl x ->
        let (registers : (int, Stack.location) Hashtbl.t) = Hashtbl.create 64 in
        List.iteri
          (fun i (arg : Ast.argument) ->
            Hashtbl.add registers arg.node_idx (Argument i))
          x.args;
        let table_entry = Hashtbl.find function_table (FnDecl x) in
        patch (Goto (Static !ptr)) table_entry;
        let breaks = codegen_stmt None x.body registers in
        if List.length breaks <> 0 then failwith "unreachable" else ();
        []
    | StructDecl x ->
        let breaks =
          List.map
            (fun (y : Ast.decl) ->
              match y with
              | FnDecl z -> codegen_stmt None (FnDecl z) registers
              | CoDecl z -> codegen_stmt None (CoDecl z) registers
              | StructDecl z -> codegen_stmt None (StructDecl z) registers
              | LetStmt _ -> failwith "Todo: global variables"
              | Field _ -> [])
            x.decls
          |> List.concat
        in
        if List.length breaks <> 0 then failwith "unreachable" else ();

        []
    | CoDecl x ->
        let (registers : (int, Stack.location) Hashtbl.t) = Hashtbl.create 64 in
        List.iteri
          (fun i (arg : Ast.argument) ->
            Hashtbl.add registers arg.node_idx (Argument i))
          x.args;
        let table_entry = Hashtbl.find function_table (CoDecl x) in
        patch (Goto (Static !ptr)) table_entry;
        let breaks = codegen_stmt None x.body registers in
        if List.length breaks <> 0 then failwith "unreachable" else ();
        []
    | AliasStmt _ -> []
    | IfResumeStmt x -> (
        let coro = codegen_expr x.coroutine registers in
        let reg = allocate_reg x.node_idx registers in
        let resume_start = !ptr in
        emit { cmd = Trap; loc = x.loc };
        let if_ok_breaks = codegen_stmt this_loop_start x.if_ok registers in
        match x.if_bad with
        | Some if_bad ->
            let if_ok_end = !ptr in
            emit { cmd = Trap; loc = x.loc };
            patch (Resume (reg, coro, Static !ptr)) resume_start;
            let if_bad_breaks = codegen_stmt this_loop_start if_bad registers in
            patch (Goto (Static !ptr)) if_ok_end;
            List.append if_ok_breaks if_bad_breaks
        | None ->
            patch (Resume (reg, coro, Static !ptr)) resume_start;
            if_ok_breaks)
    | YieldStmt x ->
        let value =
          x.value
          |> Option.map (fun v -> codegen_expr v registers)
          |> Option.value ~default:Runtime.Null
        in
        emit { cmd = Yield value; loc = x.loc };
        []
  in
  List.iter
    (fun (x : Ast.top_stmt) ->
      let breaks =
        codegen_stmt None (Ast.top_stmt_to_stmt x) (Hashtbl.create 0)
      in
      if List.length breaks <> 0 then failwith "unreachable" else ())
    root.stmts;
  failwith "Todo"
