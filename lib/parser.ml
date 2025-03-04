open Tokenizer

type parser_state = { src : string; toks : token list }

let priority = Hashtbl.create 4;;

Hashtbl.add priority TokAdd 100;;
Hashtbl.add priority TokSub 100;;
Hashtbl.add priority TokMul 200;;
Hashtbl.add priority TokDiv 200

let next_tok p =
  match p.toks with
  | t :: ts -> (t, { p with toks = ts })
  | [] -> Error.fail_at_spot "Unexpected end" p.src (String.length p.src)

let peek_tok p = match p.toks with t :: _ -> Some t | [] -> None

let print_expr_situation p stack =
  Printf.printf "[";
  List.iter (fun n -> Printf.printf "%s," (Ast.node_to_str n)) @@ List.rev stack;
  Printf.printf "] [";
  List.iter (fun t -> Printf.printf "%s," (tok_to_str t)) p.toks;
  Printf.printf "]\n"

let rec parse_expr_list p =
  let rec parse_expr_list' p l delim =
    let x, p' = next_tok p in
    if x.kind = delim then (p', l)
    else if x.kind == TokComa then parse_expr_list' p' l delim
    else
      let p, e = parse_expr p in
      parse_expr_list' p (e :: l) delim
  in
  let t, p = next_tok p in
  match t.kind with
  | TokLp ->
      let p, l = parse_expr_list' p [] TokRp in
      (p, List.rev l)
  | TokLs ->
      let p, l = parse_expr_list' p [] TokRs in
      (p, List.rev l)
  | _ -> Error.fail_at_spot "Expected a list of expressions" p.src t.loc

and parse_expr p =
  let rec parse_leaf_expr p =
    let t, p = next_tok p in
    let p, leaf =
      match t.kind with
      | TokIdent -> (p, Ast.Variable { name = t.str; loc = t.loc })
      | TokNumber -> (p, Ast.Number { num = int_of_string t.str; loc = t.loc })
      | TokLp ->
          let p, e = parse_expr p in
          let t, p = next_tok p in
          if t.kind <> TokRp then
            Error.fail_at_spot "Unbalanced ')'" p.src t.loc
          else (p, e)
      | TokSub ->
          let p, e = parse_leaf_expr p in
          (p, Ast.UnaryOp { op = t; sub = e; loc = t.loc })
      | _ -> Error.fail_at_spot "Invalid leaf expression" p.src t.loc
    in
    let rec parse_postfix p leaf =
      let t = peek_tok p in
      match t with
      | None -> (p, leaf)
      | Some t -> (
          match t.kind with
          | TokLp ->
              let p, args = parse_expr_list p in
              parse_postfix p (Ast.Call { fn = leaf; args; loc = t.loc })
          | TokLs ->
              let p, coords = parse_expr_list p in
              parse_postfix p (Ast.Index { arr = leaf; coords; loc = t.loc })
          | _ -> (p, leaf))
    in
    parse_postfix p leaf
  in

  let rec parse_expr' p stack stage =
    (* print_expr_situation p stack; *)
    let reduce stack =
      match stack with
      | a :: Ast.BinOp op :: b :: ts ->
          Ast.BinOp { op with rhs = a; lhs = b } :: ts
      | _ -> failwith "Unreachable? in reduce"
    in

    let t, p' = next_tok p in
    match (t.kind, stack, stage) with
    | (TokSemi | TokRp | TokRs | TokComa), [ root ], 1 -> (p, [ root ])
    | (TokSemi | TokRp | TokRs | TokComa), _ :: _ :: _, 1 ->
        parse_expr' p (reduce stack) 1
    | _, _, 0 ->
        let p, leaf = parse_leaf_expr p in
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
        | _ -> Error.fail_at_spot "Not a binary expression" p.src t.loc)
    | _, _, 1 ->
        parse_expr' p'
          (Ast.BinOp
             { lhs = Ast.Invalid; rhs = Ast.Invalid; op = t; loc = t.loc }
          :: stack)
          0
    | _ -> Error.fail_at_spot "Invalid expression" p.src t.loc
  in
  let p, rest = parse_expr' p [] 0 in
  match rest with
  | t :: _ :: _ ->
      Error.fail_at_spot "Unfinished expression" p.src (Ast.node_loc t)
  | [ t ] -> (p, t)
  | _ -> failwith "Unreachable? in parse_expr with empty stack as result"
