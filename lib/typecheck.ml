type ctx = {
  yield_typ : Symbols.typ option;
  in_loop : bool;
  ret_typ : Symbols.typ option;
}

let typecheck (ss : Symbols.symbol_store) (root : Ast.root) =
  let rec typecheck_expr ctx ss e =
    try typecheck_expr' ctx ss e
    with err ->
      Error.fail_at_spot "Type Error:" ss.src
        (Ast.expr_to_node e |> Ast.node_loc)
        err
  and typecheck_expr' (ctx : ctx) (ss : Symbols.symbol_store) (e : Ast.expr) :
      unit =
    match e with
    | BinExpr x ->
        let lhs = Symbols.get_type ss (Ast.expr_to_node x.lhs) in
        let rhs = Symbols.get_type ss (Ast.expr_to_node x.rhs) in
        (match (lhs, x.op.kind, rhs) with
        | IntType, TokAdd, IntType -> ()
        | StringType, TokAdd, StringType -> ()
        | IntType, _, IntType -> ()
        | _ -> failwith "Error: type mismatch");
        typecheck_expr ctx ss x.lhs;
        typecheck_expr ctx ss x.rhs
    | UnaryExpr x ->
        let sub = Symbols.get_type ss (Ast.expr_to_node x.sub_expr) in
        (match (sub, x.op.kind) with
        | IntType, _ -> ()
        | _, _ -> failwith "Error: type mismatch");
        typecheck_expr ctx ss x.sub_expr
    | CallExpr x ->
        let args =
          List.map (fun x -> Symbols.get_type ss (Ast.expr_to_node x)) x.args
        in
        let expected =
          match Symbols.get_type ss (Ast.expr_to_node x.fn) with
          | FnType x -> x.args
          | CoType x -> x.args
          | BuiltinFn x ->
              let _ = x.typecheck ss args in
              args
          | _ -> failwith "Error: not a callable value"
        in
        List.combine expected args
        |> List.iteri (fun i (a, b) ->
               if Symbols.compatible_types a b then ()
               else
                 failwith
                   (Printf.sprintf "Error: type mismatch for argument %d" i));
        typecheck_expr ctx ss x.fn;
        List.iter (typecheck_expr ctx ss) x.args
    | IndexExpr x ->
        List.iteri
          (fun i idx ->
            match Symbols.get_type ss (Ast.expr_to_node idx) with
            | IntType -> ()
            | _ ->
                failwith
                  (Printf.sprintf "Error: expected type int for index %d" i))
          x.ids;
        typecheck_expr ctx ss x.arr;
        List.iter (typecheck_expr ctx ss) x.ids
    | DotExpr x ->
        let _ = Symbols.get_type ss (DotExpr x) in
        typecheck_expr ctx ss x.obj
    | VarExpr x -> (
        match Symbols.get_definition ss (VarExpr x) with
        | Node (LetStmt def) ->
            if Ast.is_before (LetStmt def) (VarExpr x) then ()
            else failwith "Error: variable used before definition"
        | Node _ -> ()
        | Builtin _ -> ())
    | NumExpr _ -> ()
    | StringExpr _ -> ()
    | ArrayLiteral _ -> ()
    | NullLiteral _ -> ()
    | NewExpr x ->
        let typ = Symbols.get_type ss (Ast.type_to_node x.typ) in
        let expected =
          match typ with
          | StructType s ->
              s.decls
              |> List.filter_map (fun (d : Ast.decl) ->
                     match d with Field d -> Some d | _ -> None)
              |> List.sort (fun (a : Ast.field) b ->
                     String.compare a.name b.name)
          | _ -> failwith "Error: expected a struct type"
        in
        let given =
          List.map
            (fun (f : Ast.field_literal) ->
              (f.name, Symbols.get_type ss (Ast.expr_to_node f.value)))
            x.fields
          |> List.sort (fun a b -> String.compare (fst a) (fst b))
        in
        let rec check_fields (a : Ast.field list) b =
          match (a, b) with
          | x :: xs, y :: ys when x.name = fst y ->
              let expected = Symbols.get_type ss (Field x) in
              if Symbols.compatible_types expected (snd y) then
                check_fields xs ys
              else
                failwith
                  (Printf.sprintf "Error: type mismatch for field `%s'" x.name)
          | x :: xs, _ when Option.is_some x.value -> check_fields xs b
          | x :: _, [] ->
              failwith (Printf.sprintf "Error: missing field `%s'" x.name)
          | _, y :: _ ->
              failwith (Printf.sprintf "Error: unknown field `%s'" (fst y))
          | [], [] -> ()
        in
        check_fields expected given;
        List.iter
          (fun (f : Ast.field_literal) -> typecheck_expr ctx ss f.value)
          x.fields
    | CreateExpr x ->
        let args =
          List.map (fun x -> Ast.expr_to_node x |> Symbols.get_type ss) x.args
        in
        (match Symbols.get_type ss (Ast.expr_to_node x.coroutine) with
        | CoType co ->
            List.combine co.args args
            |> List.iteri (fun i (a, b) ->
                   if Symbols.compatible_types a b then ()
                   else
                     failwith
                       (Printf.sprintf "Error: type mismatch for argument %d" i))
        | _ -> failwith "Error: expected a coroutine function");
        typecheck_expr ctx ss x.coroutine;
        List.iter (typecheck_expr ctx ss) x.args
    | ResumeExpr x ->
        let coro = Symbols.get_type ss (Ast.expr_to_node x.coroutine) in
        if Symbols.can_be_coro coro then ()
        else failwith "Error: expected a coroutine object";
        typecheck_expr ctx ss x.coroutine
  and typecheck_stmt ctx ss s =
    try typecheck_stmt' ctx ss s
    with err ->
      Error.fail_at_spot "Type Error:" ss.src
        (Ast.stmt_to_node s |> Ast.node_loc)
        err
  and typecheck_stmt' (ctx : ctx) (ss : Symbols.symbol_store) (s : Ast.stmt) :
      unit =
    match s with
    | Block x -> List.iter (typecheck_stmt ctx ss) x.stmts
    | LetStmt x ->
        let expected = Symbols.get_type ss (Ast.type_to_node x.typ) in
        let given = Symbols.get_type ss (Ast.expr_to_node x.value) in
        if Symbols.compatible_types expected given then
          typecheck_expr ctx ss x.value
        else failwith "Error: type mismatch"
    | ForLoop x ->
        let coro = Symbols.get_type ss (Ast.expr_to_node x.iterator) in
        if Symbols.can_be_coro coro then ()
        else failwith "Error: expected a coroutine object as an iterator";
        typecheck_expr ctx ss x.iterator;
        typecheck_stmt { ctx with in_loop = true } ss x.body
    | WhileLoop x ->
        typecheck_expr ctx ss x.condition;
        typecheck_stmt { ctx with in_loop = true } ss x.body
    | ContinueStmt _ ->
        if ctx.in_loop then () else failwith "Error: continue outside of loop"
    | BreakStmt _ ->
        if ctx.in_loop then () else failwith "Error: break outside of loop"
    | IfStmt x ->
        typecheck_expr ctx ss x.condition;
        typecheck_stmt ctx ss x.if_true;
        Option.iter (typecheck_stmt ctx ss) x.if_false
    | ReturnStmt x -> (
        match ctx.ret_typ with
        | Some expected ->
            let returned =
              Option.map
                (fun x -> Symbols.get_type ss (Ast.expr_to_node x))
                x.value
              |> Option.value ~default:Symbols.VoidType
            in
            if Symbols.compatible_types expected returned then
              Option.iter (typecheck_expr ctx ss) x.value
            else failwith "Error: type mismatch"
        | None when Option.is_some ctx.yield_typ && Option.is_none x.value -> ()
        | _ -> failwith "Error: return outside of a function")
    | Expr x -> typecheck_expr ctx ss x
    | FnDecl x ->
        let ret_typ = Symbols.get_type ss (Ast.type_to_node x.ret) in
        let ctx = { ctx with ret_typ = Some ret_typ } in
        typecheck_stmt ctx ss x.body
    | StructDecl x ->
        List.iter
          (fun (d : Ast.decl) ->
            match d with
            | Field f -> Option.iter (typecheck_expr ctx ss) f.value
            | FnDecl x -> typecheck_stmt ctx ss (FnDecl x)
            | CoDecl x -> typecheck_stmt ctx ss (CoDecl x)
            | LetStmt x -> typecheck_stmt ctx ss (LetStmt x)
            | StructDecl x -> typecheck_stmt ctx ss (StructDecl x))
          x.decls
    | CoDecl x ->
        let yield_typ = Symbols.get_type ss (Ast.type_to_node x.yield) in
        let ctx = { ctx with yield_typ = Some yield_typ; ret_typ = None } in
        typecheck_stmt ctx ss x.body
    | AliasStmt _ -> failwith "Todo: AliasStmt"
    | IfResumeStmt x ->
        let coro = Symbols.get_type ss (Ast.expr_to_node x.coroutine) in
        if Symbols.can_be_coro coro then ()
        else failwith "Error: expected a coroutine object";
        typecheck_expr ctx ss x.coroutine;
        typecheck_stmt ctx ss x.if_ok;
        Option.iter (typecheck_stmt ctx ss) x.if_bad
    | YieldStmt x ->
        let expected =
          match ctx.yield_typ with
          | Some t -> t
          | None -> failwith "Error: yield outside of a coroutine"
        in
        let given =
          Option.map (fun x -> Symbols.get_type ss (Ast.expr_to_node x)) x.value
          |> Option.value ~default:Symbols.VoidType
        in
        if Symbols.compatible_types expected given then
          Option.iter (typecheck_expr ctx ss) x.value
        else failwith "Error: type mismatch"
  in
  let ctx = { yield_typ = None; in_loop = false; ret_typ = None } in
  List.iter
    (fun s -> Ast.top_stmt_to_stmt s |> typecheck_stmt ctx ss)
    root.stmts
