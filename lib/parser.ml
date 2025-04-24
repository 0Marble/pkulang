open Tokenizer

type parser_state = { src : string; toks : token list; next_idx : int }

let priority = Hashtbl.create 20;;

Hashtbl.add priority TokAdd 100;;
Hashtbl.add priority TokSub 100;;
Hashtbl.add priority TokMul 200;;
Hashtbl.add priority TokDiv 200;;
Hashtbl.add priority TokLt 50;;
Hashtbl.add priority TokLe 50;;
Hashtbl.add priority TokGt 50;;
Hashtbl.add priority TokGe 50;;
Hashtbl.add priority TokEq 10;;
Hashtbl.add priority TokNeq 10;;
Hashtbl.add priority TokAssign 0;;
Hashtbl.add priority TokAddAssign 0

let next_tok p =
  match p.toks with
  | t :: ts -> (t, { p with toks = ts })
  | [] ->
      Error.fail_at_spot "Unexpected end" p.src
        (Location.Spot (String.length p.src))
        Error.UnexpectedEnd

let peek_tok p = match p.toks with t :: _ -> Some t | [] -> None
let peek_tok_kind p = match p.toks with t :: _ -> Some t.kind | [] -> None
let next_idx p = { p with next_idx = p.next_idx + 1 }

let eat_tok t p =
  let next, p = next_tok p in
  if next.kind = t then p
  else
    Error.fail_at_spot
      (Printf.sprintf "Unexpected token %s, expected %s" (tok_to_str next)
         (tok_kind_to_str t))
      p.src next.loc Error.Unknown

let map_tok t f p =
  let next, p = next_tok p in
  if next.kind = t then (p, f next p)
  else
    Error.fail_at_spot
      (Printf.sprintf "Unexpected token %s, expected %s" (tok_to_str next)
         (tok_kind_to_str t))
      p.src next.loc Error.Unknown

let if_tok t f p =
  let next, p' = next_tok p in
  if next.kind = t then
    let p, res = f next p' in
    (p, Some res)
  else (p, None)

let if_not_tok t f p =
  let next, _ = next_tok p in
  if next.kind <> t then
    let p, res = f next p in
    (p, Some res)
  else (p, None)

let cur_loc p =
  match p.toks with t :: _ -> t.loc | _ -> Location.Spot (String.length p.src)

let rec skip_until_tok t p =
  match p.toks with
  | x :: rest ->
      if x.kind = t then Some p else skip_until_tok t { p with toks = rest }
  | _ -> None

let rec parse_expr_list p =
  let rec parse_expr_list' p l delim =
    let x, p' = next_tok p in
    if x.kind = delim then (p', l, x.loc)
    else if x.kind == TokComa then parse_expr_list' p' l delim
    else
      let p, e = parse_expr p in
      parse_expr_list' p (e :: l) delim
  in
  let t, p = next_tok p in
  match t.kind with
  | TokLp ->
      let l1 = t.loc in
      let p, l, l2 = parse_expr_list' p [] TokRp in
      (p, List.rev l, Location.union l1 l2)
  | TokLs ->
      let l1 = t.loc in
      let p, l, l2 = parse_expr_list' p [] TokRs in
      (p, List.rev l, Location.union l1 l2)
  | _ ->
      Error.fail_at_spot "Expected a list of expressions" p.src t.loc
        Error.Unknown

and parse_expr p =
  let rec parse_expr' p (stack : Ast.expr list) stage =
    let reduce (stack : Ast.expr list) : Ast.expr list =
      match stack with
      | a :: Ast.BinExpr op :: b :: ts ->
          Ast.BinExpr { op with rhs = a; lhs = b } :: ts
      | _ -> failwith "Unreachable? in reduce"
    in

    let t, p' = next_tok p in
    match (t.kind, stack, stage) with
    | (TokSemi | TokRp | TokRs | TokRb | TokComa | TokEnd), [ root ], 1 ->
        (p, [ root ])
    | (TokSemi | TokRp | TokRs | TokRb | TokComa | TokEnd), _ :: _ :: _, 1 ->
        parse_expr' p (reduce stack) 1
    | _, _, 0 ->
        let p, leaf = parse_term p in
        parse_expr' p (leaf :: stack) 1
    | x, _ :: Ast.BinExpr y :: _, 1 -> (
        match
          (Hashtbl.find_opt priority x, Hashtbl.find_opt priority y.op.kind)
        with
        | Some a, Some b ->
            if a <= b then parse_expr' p (reduce stack) 1
            else
              parse_expr'
                { p' with next_idx = p'.next_idx + 1 }
                (Ast.BinExpr
                   {
                     lhs = Ast.Invalid;
                     rhs = Ast.Invalid;
                     op = t;
                     loc = t.loc;
                     node_idx = p'.next_idx;
                   }
                :: stack)
                0
        | _ ->
            Error.fail_at_spot "Not a binary expression" p.src t.loc
              Error.InvalidExpression)
    | _, _, 1 ->
        if Hashtbl.find_opt priority t.kind = None then
          Error.fail_at_spot "Not a binary expression" p.src t.loc
            Error.InvalidExpression
        else
          parse_expr'
            { p' with next_idx = p'.next_idx + 1 }
            (Ast.BinExpr
               {
                 lhs = Ast.Invalid;
                 rhs = Ast.Invalid;
                 op = t;
                 loc = t.loc;
                 node_idx = p'.next_idx;
               }
            :: stack)
            0
    | _ ->
        Error.fail_at_spot "Invalid expression" p.src t.loc
          Error.InvalidExpression
  in
  let p, rest = parse_expr' p [] 0 in
  match rest with
  | t :: _ :: _ ->
      Error.fail_at_spot "Unfinished expression" p.src
        (t |> Ast.expr_to_node |> Ast.node_loc)
        Error.InvalidExpression
  | [ t ] -> (p, t)
  | _ -> failwith "Unreachable? in parse_expr with empty stack as result"

and parse_term p : parser_state * Ast.expr =
  let p' = p in
  let t, p = next_tok p in
  let p, (leaf : Ast.expr) =
    match t.kind with
    | TokNull ->
        (next_idx p, Ast.NullLiteral { loc = t.loc; node_idx = p.next_idx })
    | TokIdent ->
        ( next_idx p,
          Ast.VarExpr { name = t.str; loc = t.loc; node_idx = p.next_idx } )
    | TokNumber ->
        ( next_idx p,
          Ast.NumExpr
            { num = int_of_string t.str; loc = t.loc; node_idx = p.next_idx } )
    | TokString ->
        ( next_idx p,
          Ast.StringExpr { str = t.str; loc = t.loc; node_idx = p.next_idx } )
    | TokLs ->
        let start = t.loc in
        let p, elems, _ = parse_expr_list p' in
        ( next_idx p,
          Ast.ArrayLiteral
            {
              elems;
              loc = Location.union start (cur_loc p);
              node_idx = p.next_idx;
            } )
    | TokLp ->
        let p, e = parse_expr p in
        let t, p = next_tok p in
        if t.kind <> TokRp then
          Error.fail_at_spot "Unbalanced ')'" p.src t.loc
            Error.UnbalancedBrackets
        else (p, e)
    | TokSub | TokNot | TokAmp | TokMul ->
        let p, e = parse_term p in
        ( next_idx p,
          Ast.UnaryExpr
            {
              op = t;
              sub_expr = e;
              loc = Location.union t.loc (e |> Ast.expr_to_node |> Ast.node_loc);
              node_idx = p.next_idx;
            } )
    | TokYield ->
        let p, value =
          if_not_tok TokSemi
            (fun _ p ->
              let p = eat_tok TokLp p in
              let p, v = parse_expr p in
              let p = eat_tok TokRp p in
              (p, v))
            p
        in
        (next_idx p, Ast.YieldExpr { value; loc = t.loc; node_idx = p.next_idx })
    | TokResume ->
        let p = eat_tok TokLp p in
        let p, coroutine = parse_expr p in
        let p, value = if_tok TokComa (fun _ p -> parse_expr p) p in
        let p = eat_tok TokRp p in
        ( next_idx p,
          Ast.ResumeExpr
            { coroutine; value; loc = t.loc; node_idx = p.next_idx } )
    | TokCreate ->
        let p, exprs, _ = parse_expr_list p in
        let coroutine, params =
          match exprs with
          | a :: b -> (a, b)
          | _ ->
              Error.fail_at_spot p.src "Expected a coroutine" t.loc
                Error.Unknown
        in
        ( next_idx p,
          Ast.CreateExpr
            { coroutine; params; loc = t.loc; node_idx = p.next_idx } )
    | TokNew ->
        let p, typ = parse_type p in
        let p = eat_tok TokLb p in
        let rec parse_fields p : parser_state * Ast.field_literal list =
          let peek, _ = next_tok p in
          if peek.kind == TokRb then (p, [])
          else
            let p, (name, loc) =
              map_tok TokIdent (fun t _ -> (t.str, t.loc)) p
            in
            let p = eat_tok TokColon p in
            let p, value = parse_expr p in
            let next, p' = next_tok p in
            match next.kind with
            | TokRb ->
                (next_idx p, [ { loc; name; value; node_idx = p.next_idx } ])
            | TokComa ->
                let p, rest = parse_fields p' in
                (next_idx p, { loc; name; value; node_idx = p.next_idx } :: rest)
            | _ ->
                Error.fail_at_spot "Expected a coma" p.src next.loc
                  Error.Unknown
        in
        let p, fields = parse_fields p in
        let p = eat_tok TokRb p in
        ( next_idx p,
          Ast.NewExpr
            {
              loc = typ |> Ast.type_to_node |> Ast.node_loc;
              typ;
              fields;
              node_idx = p.next_idx;
            } )
    | _ -> Error.fail_at_spot "Invalid term" p.src t.loc Error.Unknown
  in
  let rec parse_postfix p (leaf : Ast.expr) : parser_state * Ast.expr =
    let t, p' = next_tok p in
    match t.kind with
    | TokLp ->
        let p, args, loc = parse_expr_list p in
        parse_postfix (next_idx p)
          (Ast.CallExpr
             {
               fn = leaf;
               params = args;
               loc = Location.union t.loc loc;
               node_idx = p.next_idx;
             })
    | TokLs ->
        let p, coords, loc = parse_expr_list p in
        parse_postfix (next_idx p)
          (Ast.IndexExpr
             {
               arr = leaf;
               idx = coords;
               loc = Location.union t.loc loc;
               node_idx = p.next_idx;
             })
    | TokDot ->
        let p, field = map_tok TokIdent (fun t _ -> t.str) p' in
        parse_postfix (next_idx p)
          (Ast.DotExpr
             {
               field;
               obj = leaf;
               loc =
                 Location.union
                   (leaf |> Ast.expr_to_node |> Ast.node_loc)
                   (cur_loc p);
               node_idx = p.next_idx;
             })
    | _ -> (p, leaf)
  in
  parse_postfix p leaf

and parse_type p : parser_state * Ast.typ =
  let rec parse_type_list p stage : parser_state * Ast.typ list =
    let next, _ = next_tok p in
    match (next.kind, stage) with
    | TokRp, 0 -> (p, [])
    | TokComa, 0 -> parse_type_list p 1
    | _, 1 ->
        let p, t = parse_type p in
        let p, rest = parse_type_list p 0 in
        (p, t :: rest)
    | _ -> Error.fail_at_spot p.src "Expected a coma" next.loc Error.Unknown
  in
  let parse_type_prefix p : parser_state * Ast.typ =
    let cur, p = next_tok p in
    match cur.kind with
    | TokFn ->
        let loc = cur.loc in
        let p = eat_tok TokLp p in
        let p, args = parse_type_list p 1 in
        let p = eat_tok TokRp p in
        let p, ret = parse_type p in
        (next_idx p, Ast.FnType { args; ret; loc; node_idx = p.next_idx })
    | TokCo ->
        let loc = cur.loc in
        let p, args =
          if_tok TokLp
            (fun _ p ->
              let p, args = parse_type_list p 1 in
              let p = eat_tok TokRp p in
              (p, args))
            p
        in
        let p, param =
          if_tok TokColon
            (fun _ p ->
              let p = eat_tok TokLp p in
              let p, param = parse_type p in
              let p = eat_tok TokRp p in
              (p, param))
            p
        in
        let p, yield = parse_type p in
        let (node : Ast.typ) =
          match args with
          | None -> Ast.CoObjType { param; yield; loc; node_idx = p.next_idx }
          | Some args ->
              Ast.CoType { args; yield; param; loc; node_idx = p.next_idx }
        in
        (next_idx p, node)
    | TokLs ->
        let start = cur.loc in
        let p, elem = parse_type p in
        let p = eat_tok TokRs p in
        ( next_idx p,
          Ast.ArrayType
            {
              elem;
              loc = Location.union start (cur_loc p);
              node_idx = p.next_idx;
            } )
    | TokIdent ->
        ( next_idx p,
          Ast.NamedType { name = cur.str; loc = cur.loc; node_idx = p.next_idx }
        )
    | _ -> Error.fail_at_spot "Invalid type" p.src cur.loc Error.Unknown
  in
  let rec parse_type_suffix p (prev : Ast.typ) : parser_state * Ast.typ =
    let next, p' = next_tok p in
    match next.kind with
    | TokDot ->
        let p, name = map_tok TokIdent (fun t _ -> t.str) p' in
        parse_type_suffix (next_idx p)
          (Ast.DotType
             {
               parent = prev;
               child = name;
               loc = next.loc;
               node_idx = p.next_idx;
             })
    | _ -> (p, prev)
  in
  let p, leaf = parse_type_prefix p in
  parse_type_suffix p leaf

and parse_args p : parser_state * Ast.argument list =
  let cur, p' = next_tok p in
  match cur.kind with
  | TokRp -> (p, [])
  | TokComa -> parse_args p'
  | TokIdent ->
      let start = cur.loc in
      let name = cur.str in
      let p = eat_tok TokColon p' in
      let p, typ = parse_type p in
      let p, rest = parse_args p in
      let arg : Ast.argument =
        { name; arg_type = typ; loc = start; node_idx = p.next_idx }
      in
      (next_idx p, arg :: rest)
  | _ -> Error.fail_at_spot "Expected an argument" p.src cur.loc Error.Unknown

and parse_stmt p : parser_state * Ast.stmt =
  let p0 = p in
  let t, p = next_tok p in
  match t.kind with
  | TokLet ->
      let start = t.loc in
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokColon p in
      let p, typ = parse_type p in
      let p = eat_tok TokAssign p in
      let p, value = parse_expr p in
      let p = eat_tok TokSemi p in
      ( next_idx p,
        Ast.LetStmt
          {
            var_name = name;
            var_type = typ;
            value;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokWhile ->
      let start = t.loc in
      let p = eat_tok TokLp p in
      let p, condition = parse_expr p in
      let p = eat_tok TokRp p in
      let p, body = parse_stmt p in
      let () =
        match body with
        | IfStmt _ | WhileLoop _ | ForLoop _ ->
            Error.fail_at_spot "Surround with a block" p.src
              (body |> Ast.stmt_to_node |> Ast.node_loc)
              Error.Unknown
        | _ -> ()
      in
      ( next_idx p,
        Ast.WhileLoop
          {
            condition;
            body;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokIf ->
      let start = t.loc in
      let p = eat_tok TokLp p in
      let p, condition = parse_expr p in
      let p = eat_tok TokRp p in
      let p, if_true = parse_stmt p in
      let p, if_false = if_tok TokElse (fun _ p -> parse_stmt p) p in
      let () =
        match if_true with
        | IfStmt _ | WhileLoop _ | ForLoop _ ->
            Error.fail_at_spot "Surround with a block" p.src
              (if_true |> Ast.stmt_to_node |> Ast.node_loc)
              Error.Unknown
        | _ -> ()
      in
      ( next_idx p,
        Ast.IfStmt
          {
            condition;
            if_true;
            if_false;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokFor ->
      let start = t.loc in
      let p = eat_tok TokLp p in
      let p, var_name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokColon p in
      let p, iter = parse_expr p in
      let p = eat_tok TokRp p in
      let p, body = parse_stmt p in
      let () =
        match body with
        | IfStmt _ | WhileLoop _ | ForLoop _ ->
            Error.fail_at_spot "Surround by a block" p.src
              (body |> Ast.stmt_to_node |> Ast.node_loc)
              Error.Unknown
        | _ -> ()
      in
      ( next_idx p,
        Ast.ForLoop
          {
            iter_var = var_name;
            iterator = iter;
            body;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokReturn ->
      let start = t.loc in
      let p, value = if_not_tok TokSemi (fun _ p -> parse_expr p) p in
      let p = eat_tok TokSemi p in
      ( next_idx p,
        Ast.ReturnStmt
          {
            value;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokBreak ->
      let start = t.loc in
      let p = eat_tok TokSemi p in
      ( next_idx p,
        Ast.BreakStmt
          { loc = Location.union start (cur_loc p); node_idx = p.next_idx } )
  | TokContinue ->
      let start = t.loc in
      let p = eat_tok TokSemi p in
      ( p,
        Ast.ContinueStmt
          { loc = Location.union start (cur_loc p); node_idx = p.next_idx } )
  | TokLb ->
      let start = t.loc in
      let rec parse_stmts p =
        let cur, _ = next_tok p in
        match cur.kind with
        | TokRb -> (p, [])
        | _ ->
            let p, stmt = parse_stmt p in
            let p, rest = parse_stmts p in
            (p, stmt :: rest)
      in
      let p, stmts = parse_stmts p in
      let p = eat_tok TokRb p in
      ( next_idx p,
        Ast.Block
          {
            stmts;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokCo ->
      let start = t.loc in
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokLp p in

      let p, args = parse_args p in
      let p = eat_tok TokRp p in
      let p, param_type =
        if_tok TokColon
          (fun _ p ->
            let p = eat_tok TokLp p in
            let p, param = parse_type p in
            let p = eat_tok TokRp p in
            (p, param))
          p
      in
      let p, yield_type = parse_type p in
      let p, body = parse_stmt p in
      ( p,
        Ast.CoDecl
          {
            name;
            args;
            yield_type;
            param_type;
            body;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokFn ->
      let start = t.loc in
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokLp p in

      let p, args = parse_args p in
      let p = eat_tok TokRp p in
      let p, ret_type = parse_type p in
      let p, body = parse_stmt p in
      ( p,
        Ast.FnDecl
          {
            name;
            args;
            ret_type;
            body;
            loc = Location.union start (cur_loc p);
            node_idx = p.next_idx;
          } )
  | TokStruct ->
      let rec parse_decl_list p : parser_state * Ast.decl list =
        let next, _ = next_tok p in
        match next.kind with
        | TokRb -> (p, [])
        | TokLet | TokFn | TokCo | TokStruct ->
            let p, decl = parse_stmt p in
            let decl : Ast.decl =
              match decl with
              | LetStmt x -> LetStmt x
              | FnDecl x -> FnDecl x
              | StructDecl x -> StructDecl x
              | CoDecl x -> CoDecl x
              | _ -> failwith "Unreachable"
            in
            let p, rest = parse_decl_list p in
            (p, decl :: rest)
        | TokIdent ->
            let p, decl = parse_field_decl p in
            let p, rest = parse_decl_list p in
            (p, decl :: rest)
        | _ ->
            Error.fail_at_spot "Not a declaration" p.src next.loc Error.Unknown
      in
      let start = t.loc in
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokLb p in
      let p, decls = parse_decl_list p in
      let p = eat_tok TokRb p in

      ( next_idx p,
        Ast.StructDecl
          {
            loc = Location.union (cur_loc p) start;
            name;
            decls;
            node_idx = p.next_idx;
          } )
  | TokType ->
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokAssign p in
      let p, typ = parse_type p in
      let p = eat_tok TokSemi p in
      ( next_idx p,
        Ast.AliasStmt
          {
            loc = t.loc;
            type_name = name;
            other_type = typ;
            node_idx = p.next_idx;
          } )
  | _ ->
      let p, expr = parse_expr p0 in
      let p = eat_tok TokSemi p in
      (p, Expr expr)

and parse_field_decl p : parser_state * Ast.decl =
  let start = cur_loc p in
  let p, name = map_tok TokIdent (fun t _ -> t.str) p in
  let p = eat_tok TokColon p in
  let p, typ = parse_type p in
  let p, value = if_tok TokAssign (fun _ p -> parse_expr p) p in
  let p = eat_tok TokComa p in
  ( next_idx p,
    Ast.Field
      {
        loc = start;
        var_name = name;
        field_type = typ;
        value;
        node_idx = p.next_idx;
      } )

and parse_root p : parser_state * Ast.root =
  let rec parse_program' p : parser_state * Ast.top_stmt list =
    match peek_tok_kind p with
    | Some TokEnd -> (p, [])
    | Some _ ->
        let p, stmt = parse_stmt p in
        let stmt : Ast.top_stmt =
          match stmt with
          | FnDecl x -> FnDecl x
          | StructDecl y -> StructDecl y
          | CoDecl y -> CoDecl y
          | LetStmt y -> LetStmt y
          | AliasStmt y -> AliasStmt y
          | _ ->
              Error.fail_at_spot p.src "Not a top-level statement"
                (stmt |> Ast.stmt_to_node |> Ast.node_loc)
                Error.Unknown
        in
        let p, rest = parse_program' p in
        (p, stmt :: rest)
    | None -> failwith "Unreachable (in parse_program')"
  in
  let start = cur_loc p in
  let p, stmts = parse_program' p in
  (next_idx p, { stmts; loc = start; node_idx = p.next_idx })
