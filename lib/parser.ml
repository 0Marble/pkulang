let parse_root src =
  let convert_token (t : Tokenizer.token) : Lalr.token =
    match t.kind with
    | TokNumber -> TokNumber (t.str, t.loc)
    | TokIdent -> TokIdent (t.str, t.loc)
    | TokString -> TokString (t.str, t.loc)
    | TokNull -> TokNull (t.str, t.loc)
    | TokAdd -> TokAdd (t.str, t.loc)
    | TokSub -> TokSub (t.str, t.loc)
    | TokMul -> TokMul (t.str, t.loc)
    | TokDiv -> TokDiv (t.str, t.loc)
    | TokMod -> TokMod (t.str, t.loc)
    | TokLt -> TokLt (t.str, t.loc)
    | TokLe -> TokLe (t.str, t.loc)
    | TokGt -> TokGt (t.str, t.loc)
    | TokGe -> TokGe (t.str, t.loc)
    | TokAssign -> TokAssign (t.str, t.loc)
    | TokAddAssign -> TokAddAssign (t.str, t.loc)
    | TokSubAssign -> TokSubAssign (t.str, t.loc)
    | TokMulAssign -> TokMulAssign (t.str, t.loc)
    | TokDivAssign -> TokDivAssign (t.str, t.loc)
    | TokEq -> TokEq (t.str, t.loc)
    | TokNeq -> TokNeq (t.str, t.loc)
    | TokNot -> TokNot (t.str, t.loc)
    | TokAnd -> TokAnd (t.str, t.loc)
    | TokOr -> TokOr (t.str, t.loc)
    | TokYield -> TokYield (t.str, t.loc)
    | TokResume -> TokResume (t.str, t.loc)
    | TokCreate -> TokCreate (t.str, t.loc)
    | TokNew -> TokNew (t.str, t.loc)
    | TokLp -> TokLp (t.str, t.loc)
    | TokRp -> TokRp (t.str, t.loc)
    | TokLb -> TokLb (t.str, t.loc)
    | TokRb -> TokRb (t.str, t.loc)
    | TokLs -> TokLs (t.str, t.loc)
    | TokRs -> TokRs (t.str, t.loc)
    | TokSemi -> TokSemi (t.str, t.loc)
    | TokColon -> TokColon (t.str, t.loc)
    | TokComa -> TokComa (t.str, t.loc)
    | TokDot -> TokDot (t.str, t.loc)
    | TokReturn -> TokReturn (t.str, t.loc)
    | TokBreak -> TokBreak (t.str, t.loc)
    | TokContinue -> TokContinue (t.str, t.loc)
    | TokIf -> TokIf (t.str, t.loc)
    | TokElse -> TokElse (t.str, t.loc)
    | TokFor -> TokFor (t.str, t.loc)
    | TokWhile -> TokWhile (t.str, t.loc)
    | TokLet -> TokLet (t.str, t.loc)
    | TokFn -> TokFn (t.str, t.loc)
    | TokCo -> TokCo (t.str, t.loc)
    | TokStruct -> TokStruct (t.str, t.loc)
    | TokType -> TokType (t.str, t.loc)
    | TokEnd -> TokEnd (t.str, t.loc)
    | TokAmp -> failwith "TokAmp not supported"
    | TokPub -> failwith "TokPub not supported"
  in

  let tokens = Tokenizer.tokenize src |> Array.of_list in
  let idx = ref 0 in
  let dummy_lexbuf : Lexing.lexbuf =
    {
      refill_buff = (fun _ -> ());
      lex_buffer = Bytes.create 100;
      lex_buffer_len = 100;
      lex_abs_pos = 0;
      lex_start_pos = 0;
      lex_curr_pos = 0;
      lex_last_pos = 0;
      lex_last_action = 0;
      lex_eof_reached = false;
      lex_mem = [||];
      lex_start_p = Lexing.dummy_pos;
      lex_curr_p = Lexing.dummy_pos;
    }
  in

  try
    let root =
      Lalr.root
        (fun (lex : Lexing.lexbuf) ->
          lex.lex_curr_pos <- !idx;
          idx := !idx + 1;
          convert_token tokens.(!idx - 1))
        dummy_lexbuf
    in
    let _ =
      Ast.visit_all_nodes
        (fun n -> List.iter (Ast.set_parent n) (Ast.node_children n))
        root
    in
    root
  with e -> Error.fail_at_spot "Parser error" src tokens.(!idx - 1).loc e

let parse_stmt src =
  let root = parse_root ("fn foo() void {\n" ^ src ^ "\n}") in
  let fn_decl =
    match List.hd root.stmts with FnDecl f -> f | _ -> failwith "unreachable"
  in
  let block =
    match fn_decl.body with Block b -> b | _ -> failwith "unreachable"
  in
  List.hd block.stmts

let parse_expr src =
  let root = parse_root ("fn foo() void {\n" ^ src ^ "\n;}") in
  let fn_decl =
    match List.hd root.stmts with FnDecl f -> f | _ -> failwith "unreachable"
  in
  let block =
    match fn_decl.body with Block b -> b | _ -> failwith "unreachable"
  in
  match List.hd block.stmts with Expr e -> e | _ -> failwith "unreachable"
