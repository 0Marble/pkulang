let codegen (src : string) (fn_list : Ast.node list)
    (get_definition : Ast.node -> [ `Node of Ast.node | `Builtin of string ])
    (root : Ast.root) (stdin : unit -> string) (stdout : string -> unit) :
    Runtime.runtime =
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

  let (builtins_table : (string, int) Hashtbl.t) = Hashtbl.create 64 in
  let create_builtin name cmds =
    Hashtbl.add builtins_table name !ptr;
    List.iter (fun cmd -> emit { cmd; loc = Location.Spot 0 }) cmds;
    ()
  in

  create_builtin "len"
    [ Size (Argument 0, Location (Argument 0)); Ret (Location (Argument 0)) ];

  let (globals_table : (Ast.node, Stack.location) Hashtbl.t) =
    Hashtbl.create 64
  in
  let get_or_define_global node =
    match Hashtbl.find_opt globals_table node with
    | Some reg -> reg
    | None ->
        let (reg : Stack.location) = Global (Hashtbl.length globals_table) in
        Hashtbl.add globals_table node reg;
        reg
  in

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
      Printexc.print_backtrace stderr;
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
            | `Node (Field z) ->
                let reg = allocate_reg y.node_idx registers in
                emit { cmd = FieldSet (obj, z.var_name, rhs); loc = x.loc };
                Location reg
            | `Node (LetStmt z) ->
                let reg = get_or_define_global (LetStmt z) in
                emit { cmd = Assign (reg, rhs); loc = x.loc };
                Location reg
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
              emit { cmd = Sub (res, lhs, rhs); loc = x.loc };
              Lt (res, Number 0, Location res)
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
    | CallExpr x -> (
        let args =
          List.map (fun a -> codegen_expr a registers) x.params |> Array.of_list
        in
        match x.fn with
        | VarExpr _
          when get_definition (Ast.expr_to_node x.fn) = `Builtin "println" ->
            emit { cmd = Builtin (args, "println"); loc = Location.Spot 0 };
            Null
        | VarExpr _
          when get_definition (Ast.expr_to_node x.fn) = `Builtin "print" ->
            emit { cmd = Builtin (args, "print"); loc = Location.Spot 0 };
            Null
        | DotExpr y ->
            let applied_len = List.length x.params in
            let expected_len =
              match get_definition (DotExpr y) with
              | `Node (FnDecl z) -> List.length z.args
              | `Node (CoDecl z) -> List.length z.args
              | _ -> failwith "Error: unspported function call"
            in
            let args =
              if applied_len = expected_len then args
              else if applied_len + 1 = expected_len then
                let obj = codegen_expr y.obj registers in
                Array.append [| obj |] args
              else failwith "Error: unsupported argument count"
            in
            let fptr = codegen_expr x.fn registers |> op_to_jump in
            let res = allocate_reg x.node_idx registers in
            emit { cmd = Call (res, args, fptr); loc = x.loc };
            Location res
        | _ ->
            let fptr = codegen_expr x.fn registers |> op_to_jump in
            let res = allocate_reg x.node_idx registers in
            emit { cmd = Call (res, args, fptr); loc = x.loc };
            Location res)
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
        | `Node (Field y) ->
            let reg = allocate_reg x.node_idx registers in
            emit { cmd = FieldGet (reg, obj, y.var_name); loc = x.loc };
            Location reg
        | `Node (LetStmt y) -> Location (get_or_define_global (LetStmt y))
        | `Node (FnDecl y) -> Number (Hashtbl.find function_table (FnDecl y))
        | `Node (CoDecl y) -> Number (Hashtbl.find function_table (CoDecl y))
        | `Builtin s -> Number (Hashtbl.find builtins_table s)
        | _ -> failwith "Error: unsupported definition for field")
    | VarExpr x -> (
        let def = get_definition (VarExpr x) in
        match def with
        | `Node (LetStmt y) ->
            let loc =
              match Hashtbl.find_opt registers y.node_idx with
              | Some local -> local
              | _ -> get_or_define_global (LetStmt y)
            in
            Location loc
        | `Node (Argument y) -> Location (Hashtbl.find registers y.node_idx)
        | `Node (ForLoop y) -> Location (Hashtbl.find registers y.node_idx)
        | `Node (IfResumeStmt y) -> Location (Hashtbl.find registers y.node_idx)
        | `Node (FnDecl y) -> Number (Hashtbl.find function_table (FnDecl y))
        | `Node (CoDecl y) -> Number (Hashtbl.find function_table (CoDecl y))
        | `Builtin s -> Number (Hashtbl.find builtins_table s)
        | `Node (StructDecl _) -> Null
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
        (match get_definition (Ast.type_to_node x.typ) with
        | `Node (StructDecl y) ->
            List.iter
              (fun (d : Ast.decl) ->
                match d with
                | Field z -> (
                    emit { cmd = AddField (obj, z.var_name); loc = x.loc };
                    match z.value with
                    | Some v ->
                        let v = codegen_expr v registers in
                        emit
                          { cmd = FieldSet (obj, z.var_name, v); loc = x.loc }
                    | _ -> ())
                | _ -> ())
              y.decls
        | _ -> failwith "Error: unsupported type for new expression");
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
      Printexc.print_backtrace stderr;
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
        patch (GotoIfZero (cond, Static !ptr)) jump_to_end;
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
        let res = [ !ptr ] in
        emit { cmd = Trap; loc = x.loc };
        res
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
    | FnDecl _ | CoDecl _ -> []
    | StructDecl x ->
        let breaks =
          List.map
            (fun (y : Ast.decl) ->
              match y with
              | FnDecl z -> codegen_stmt None (FnDecl z) registers
              | CoDecl z -> codegen_stmt None (CoDecl z) registers
              | StructDecl z -> codegen_stmt None (StructDecl z) registers
              | LetStmt z ->
                  let _ = get_or_define_global (LetStmt z) in
                  []
              | Field _ -> [])
            x.decls
          |> List.concat
        in
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
  and codegen_top_stmt (s : Ast.top_stmt) : unit =
    match s with
    | LetStmt x ->
        let _ = get_or_define_global (LetStmt x) in
        ()
    | _ ->
        let breaks =
          codegen_stmt None (Ast.top_stmt_to_stmt s) (Hashtbl.create 0)
        in
        if List.length breaks <> 0 then failwith "unreachable" else ()
  and codegen_fn (x : Ast.node) : unit =
    let args, body, loc =
      match x with
      | FnDecl f -> (f.args, f.body, f.loc)
      | CoDecl f -> (f.args, f.body, f.loc)
      | _ -> failwith "unreachable"
    in
    let table_entry = Hashtbl.find function_table x in
    patch (Goto (Static !ptr)) table_entry;

    let (registers : (int, Stack.location) Hashtbl.t) = Hashtbl.create 64 in
    List.iteri
      (fun i (arg : Ast.argument) ->
        Hashtbl.add registers arg.node_idx (Argument i))
      args;
    let alloca = !ptr in
    emit { cmd = Trap; loc };
    let breaks = codegen_stmt None body registers in
    emit { cmd = Ret Null; loc };
    patch (Alloca (Hashtbl.length registers)) alloca;
    if List.length breaks <> 0 then failwith "unreachable" else ()
  in

  List.iter codegen_top_stmt root.stmts;
  List.iter codegen_fn fn_list;

  let generate_start main : int =
    let start = !ptr in
    let registers = Hashtbl.create 64 in
    emit { cmd = Trap; loc = Location.Spot 0 };
    Hashtbl.iter
      (fun (node : Ast.node) (glob_reg : Stack.location) ->
        match node with
        | LetStmt x ->
            let reg = codegen_expr x.value registers in
            emit { cmd = Assign (glob_reg, reg); loc = x.loc }
        | _ -> failwith "Error: not a global var definition")
      globals_table;
    emit { cmd = Call (Void, [||], Static main); loc = Location.Spot 0 };
    emit { cmd = Halt; loc = Location.Spot 0 };
    patch (Alloca (Hashtbl.length registers)) start;
    start
  in
  let main =
    match
      List.find_map
        (fun (node : Ast.node) ->
          match node with
          | FnDecl f when f.name = "main" ->
              Hashtbl.find_opt function_table node
          | _ -> None)
        fn_list
    with
    | Some main -> main
    | None -> failwith "Error: no fn main() void declared"
  in
  let start = generate_start main in
  Runtime.create src (Array.sub cmds 0 !ptr) start
    (Hashtbl.length globals_table)
    stdin stdout
