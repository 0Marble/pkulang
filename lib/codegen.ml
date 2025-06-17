let codegen (fn_list : Ast.node list) (get_definition : Ast.node -> Ast.node)
    (root : Ast.root) : Runtime.runtime =
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

  let rec codegen_expr (e : Ast.expr)
      (registers : (int, Stack.location) Hashtbl.t) : Runtime.operand =
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
        | FnDecl _ -> failwith "Todo: function locations"
        | CoDecl _ -> failwith "Todo: function locations"
        | _ -> failwith "Error: unsupported definition for field")
    | VarExpr x -> (
        let def = get_definition (VarExpr x) in
        match def with
        | LetStmt y -> Location (Hashtbl.find registers y.node_idx)
        | ForLoop y -> Location (Hashtbl.find registers y.node_idx)
        | FnDecl _ -> failwith "Todo: function locations"
        | CoDecl _ -> failwith "Todo: function locations"
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
  in
  failwith "Todo"
