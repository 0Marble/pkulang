open Tokenizer

type parser_state = { src : string; toks : token list }

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

let print_expr_situation p stack =
  Printf.printf "[";
  List.iter (fun n -> Printf.printf "%s," (Ast.node_to_str n)) @@ List.rev stack;
  Printf.printf "] [";
  List.iter (fun t -> Printf.printf "%s," (tok_to_str t)) p.toks;
  Printf.printf "]\n"

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
  let rec parse_expr' p stack stage =
    print_expr_situation p stack;
    let reduce stack =
      match stack with
      | a :: Ast.BinOp op :: b :: ts ->
          Ast.BinOp
            {
              op with
              rhs = a;
              lhs = b;
              loc = Location.union (Ast.node_loc a) (Ast.node_loc b);
            }
          :: ts
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
    | x, _ :: Ast.BinOp y :: _, 1 -> (
        match
          (Hashtbl.find_opt priority x, Hashtbl.find_opt priority y.op.kind)
        with
        | Some a, Some b ->
            if a <= b then parse_expr' p (reduce stack) 1
            else
              parse_expr' p'
                (Ast.BinOp
                   { lhs = Ast.Invalid; rhs = Ast.Invalid; op = t; loc = t.loc }
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
          parse_expr' p'
            (Ast.BinOp
               { lhs = Ast.Invalid; rhs = Ast.Invalid; op = t; loc = t.loc }
            :: stack)
            0
    | _ ->
        Error.fail_at_spot "Invalid expression" p.src t.loc
          Error.InvalidExpression
  in
  let p, rest = parse_expr' p [] 0 in
  match rest with
  | t :: _ :: _ ->
      Error.fail_at_spot "Unfinished expression" p.src (Ast.node_loc t)
        Error.InvalidExpression
  | [ t ] -> (p, t)
  | _ -> failwith "Unreachable? in parse_expr with empty stack as result"

and parse_term p =
  let p' = p in
  let t, p = next_tok p in
  let p, leaf =
    match t.kind with
    | TokIdent -> (p, Ast.Variable { name = t.str; loc = t.loc })
    | TokNumber -> (p, Ast.Number { num = int_of_string t.str; loc = t.loc })
    | TokString -> (p, Ast.String { str = t.str; loc = t.loc })
    | TokLs ->
        let start = t.loc in
        let p, elems, _ = parse_expr_list p' in
        (p, Ast.ArrayLiteral { elems; loc = Location.union start (cur_loc p) })
    | TokLp ->
        let p, e = parse_expr p in
        let t, p = next_tok p in
        if t.kind <> TokRp then
          Error.fail_at_spot "Unbalanced ')'" p.src t.loc
            Error.UnbalancedBrackets
        else (p, e)
    | TokSub | TokNot | TokAmp | TokMul ->
        let p, e = parse_term p in
        ( p,
          Ast.UnaryOp
            { op = t; sub = e; loc = Location.union t.loc (Ast.node_loc e) } )
    | TokNew ->
        let loc = t.loc in
        let p, typ = parse_type p in
        let p = eat_tok TokLb p in
        let rec parse_fields p =
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
            | TokRb -> (p, [ Ast.FieldLiteral { loc; name; value } ])
            | TokComa ->
                let p, rest = parse_fields p' in
                (p, Ast.FieldLiteral { loc; name; value } :: rest)
            | _ ->
                Error.fail_at_spot "Expected a coma" p.src next.loc
                  Error.Unknown
        in
        let p, fields = parse_fields p in
        let p = eat_tok TokRb p in
        (p, Ast.NewExpr { loc; typ; fields })
    | _ -> Error.fail_at_spot "Invalid term" p.src t.loc Error.Unknown
  in
  let rec parse_postfix p leaf =
    let t, p' = next_tok p in
    match t.kind with
    | TokLp ->
        let p, args, loc = parse_expr_list p in
        parse_postfix p
          (Ast.Call { fn = leaf; args; loc = Location.union t.loc loc })
    | TokLs ->
        let p, coords, loc = parse_expr_list p in
        parse_postfix p
          (Ast.Index { arr = leaf; coords; loc = Location.union t.loc loc })
    | TokDot ->
        let p, field = map_tok TokIdent (fun t _ -> t.str) p' in
        parse_postfix p
          (Ast.DotExpr
             {
               field;
               obj = leaf;
               loc = Location.union (Ast.node_loc leaf) (cur_loc p);
             })
    | _ -> (p, leaf)
  in
  parse_postfix p leaf

and parse_type p =
  let parse_type_prefix p =
    let cur, p = next_tok p in
    match cur.kind with
    | TokLs -> (
        let start = cur.loc in
        let p, elem = parse_type p in
        let p, size = if_tok TokSemi (fun _ p -> parse_expr p) p in
        let p = eat_tok TokRs p in
        match size with
        | None ->
            (p, Ast.SliceType { elem; loc = Location.union start (cur_loc p) })
        | Some size ->
            ( p,
              Ast.ArrayType
                { elem; size; loc = Location.union start (cur_loc p) } ))
    | TokIdent -> (p, Ast.NamedType { name = cur.str; loc = cur.loc })
    | TokAmp ->
        let p, sub = parse_type p in
        (p, Ast.PtrType { sub; loc = cur.loc })
    | _ -> Error.fail_at_spot "Invalid type" p.src cur.loc Error.Unknown
  in
  let rec parse_type_suffix p prev : parser_state * Ast.node =
    let next, p' = next_tok p in
    match next.kind with
    | TokDot ->
        let p, name = map_tok TokIdent (fun t _ -> t.str) p' in
        parse_type_suffix p
          (Ast.DotType { parent = prev; child = name; loc = next.loc })
    | _ -> (p, prev)
  in
  let p, leaf = parse_type_prefix p in
  parse_type_suffix p leaf

and parse_stmt p =
  let p0 = p in
  let t, p = next_tok p in
  match t.kind with
  | TokColon ->
      let start = t.loc in
      let p, label = map_tok TokIdent (fun t _ -> t.str) p in
      let p, stmt = parse_stmt p in
      let () =
        match stmt with
        | Ast.Block _ | Ast.WhileLoop _ | Ast.ForLoop _ | Ast.IfStmt _ -> ()
        | x ->
            Error.fail_at_spot "Not a declaration" p.src (Ast.node_loc x)
              Error.Unknown
      in
      ( p,
        Ast.LabeledStmt { label; stmt; loc = Location.union start (cur_loc p) }
      )
  | TokLet ->
      let start = t.loc in
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokColon p in
      let p, typ = parse_type p in
      let p = eat_tok TokAssign p in
      let p, value = parse_expr p in
      let p = eat_tok TokSemi p in
      ( p,
        Ast.LetStmt { name; typ; value; loc = Location.union start (cur_loc p) }
      )
  | TokWhile ->
      let start = t.loc in
      let p = eat_tok TokLp p in
      let p, cond = parse_expr p in
      let p = eat_tok TokRp p in
      let p, body = parse_stmt p in
      let p, finally = if_tok TokElse (fun _ p -> parse_stmt p) p in
      let () =
        match body with
        | IfStmt _ | WhileLoop _ | ForLoop _ ->
            Error.fail_at_spot "Surround with a block" p.src (Ast.node_loc body)
              Error.Unknown
        | _ -> ()
      in
      ( p,
        Ast.WhileLoop
          { cond; body; finally; loc = Location.union start (cur_loc p) } )
  | TokIf ->
      let start = t.loc in
      let p = eat_tok TokLp p in
      let p, cond = parse_expr p in
      let p = eat_tok TokRp p in
      let p, if_true = parse_stmt p in
      let p, if_false = if_tok TokElse (fun _ p -> parse_stmt p) p in
      let () =
        match if_true with
        | IfStmt _ | WhileLoop _ | ForLoop _ ->
            Error.fail_at_spot "Surround with a block" p.src
              (Ast.node_loc if_true) Error.Unknown
        | _ -> ()
      in
      ( p,
        Ast.IfStmt
          { cond; if_true; if_false; loc = Location.union start (cur_loc p) } )
  | TokFor ->
      let start = t.loc in
      let p = eat_tok TokLp p in
      let p, var_name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokColon p in
      let p, iter = parse_expr p in
      let p = eat_tok TokRp p in
      let p, body = parse_stmt p in
      let p, finally = if_tok TokElse (fun _ p -> parse_stmt p) p in
      let () =
        match body with
        | IfStmt _ | WhileLoop _ | ForLoop _ ->
            Error.fail_at_spot "Surround by a block" p.src (Ast.node_loc body)
              Error.Unknown
        | _ -> ()
      in
      ( p,
        Ast.ForLoop
          {
            var_name;
            iter;
            body;
            finally;
            loc = Location.union start (cur_loc p);
          } )
  | TokReturn ->
      let start = t.loc in
      let p, value = if_not_tok TokSemi (fun _ p -> parse_expr p) p in
      let p = eat_tok TokSemi p in
      (p, Ast.Return { value; loc = Location.union start (cur_loc p) })
  | TokBreak ->
      let start = t.loc in
      let p, label =
        if_tok TokColon (fun _ p -> map_tok TokIdent (fun t _ -> t.str) p) p
      in
      let p, value = if_not_tok TokSemi (fun _ p -> parse_expr p) p in
      let p = eat_tok TokSemi p in
      (p, Ast.Break { label; value; loc = Location.union start (cur_loc p) })
  | TokContinue ->
      let start = t.loc in
      let p, label =
        if_tok TokColon (fun _ p -> map_tok TokIdent (fun t _ -> t.str) p) p
      in
      let p = eat_tok TokSemi p in
      (p, Ast.Continue { label; loc = Location.union start (cur_loc p) })
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
      (p, Ast.Block { stmts; loc = Location.union start (cur_loc p) })
  | TokFn ->
      let start = t.loc in
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokLp p in

      let rec parse_args p =
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
            (p, Ast.FunctionArg { name; typ; loc = start } :: rest)
        | _ ->
            Error.fail_at_spot "Expected an argument" p.src cur.loc
              Error.Unknown
      in
      let p, args = parse_args p in
      let p = eat_tok TokRp p in
      let p, ret_type = parse_type p in
      let p, body = parse_stmt p in
      ( p,
        Ast.Function
          { name; args; ret_type; body; loc = Location.union start (cur_loc p) }
      )
  | TokStruct ->
      let rec parse_decl_list p =
        let next, _ = next_tok p in
        match next.kind with
        | TokRb -> (p, [])
        | TokLet | TokFn | TokStruct ->
            let p, decl = parse_stmt p in
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

      (p, Ast.Struct { loc = Location.union (cur_loc p) start; name; decls })
  | TokType ->
      let p, name = map_tok TokIdent (fun t _ -> t.str) p in
      let p = eat_tok TokAssign p in
      let p, typ = parse_type p in
      (p, Ast.TypeAlias { loc = t.loc; name; typ })
  | _ ->
      let p, expr = parse_expr p0 in
      Printf.printf "%s\n" (Ast.node_to_str expr);
      let p = eat_tok TokSemi p in
      (p, expr)

and parse_field_decl p =
  let start = cur_loc p in
  let p, name = map_tok TokIdent (fun t _ -> t.str) p in
  let p = eat_tok TokColon p in
  let p, typ = parse_type p in
  let p, value = if_tok TokAssign (fun _ p -> parse_expr p) p in
  let p = eat_tok TokComa p in
  (p, Ast.Field { loc = start; name; typ; value })
