module CharMap = Map.Make (Char)

type token_kind =
  | TokNumber
  | TokIdent
  | TokString
  (* Expressions *)
  | TokNull
  | TokAdd
  | TokSub
  | TokMul
  | TokDiv
  | TokLt
  | TokLe
  | TokGt
  | TokGe
  | TokAssign
  | TokAddAssign
  | TokSubAssign
  | TokMulAssign
  | TokDivAssign
  | TokEq
  | TokNeq
  | TokNot
  | TokAmp
  | TokYield
  | TokResume
  | TokCreate
  | TokNew
  (* Brackets *)
  | TokLp
  | TokRp
  | TokLb
  | TokRb
  | TokLs
  | TokRs
  (* Punctuation *)
  | TokSemi
  | TokColon
  | TokComa
  | TokDot
  (* Statements *)
  | TokReturn
  | TokBreak
  | TokContinue
  | TokIf
  | TokElse
  | TokFor
  | TokWhile
  | TokLet
  (* Declarations *)
  | TokPub
  | TokFn
  | TokCo
  | TokStruct
  | TokType
  (* special token placed at the end of the token list *)
  | TokEnd

type token = { str : string; loc : Location.location; kind : token_kind }

let token_map = Trie.init ();;

Trie.add_word token_map "null" TokNull;;
Trie.add_word token_map "return" TokReturn;;
Trie.add_word token_map "break" TokBreak;;
Trie.add_word token_map "continue" TokContinue;;
Trie.add_word token_map "let" TokLet;;
Trie.add_word token_map "if" TokIf;;
Trie.add_word token_map "else" TokElse;;
Trie.add_word token_map "while" TokWhile;;
Trie.add_word token_map "for" TokFor;;
Trie.add_word token_map "fn" TokFn;;
Trie.add_word token_map "co" TokCo;;
Trie.add_word token_map "pub" TokPub;;
Trie.add_word token_map "struct" TokStruct;;
Trie.add_word token_map "type" TokType;;
Trie.add_word token_map "yield" TokYield;;
Trie.add_word token_map "resume" TokResume;;
Trie.add_word token_map "create" TokCreate;;
Trie.add_word token_map "new" TokNew;;
Trie.add_word token_map "+" TokAdd;;
Trie.add_word token_map "-" TokSub;;
Trie.add_word token_map "*" TokMul;;
Trie.add_word token_map "/" TokDiv;;
Trie.add_word token_map "=" TokAssign;;
Trie.add_word token_map "+=" TokAddAssign;;
Trie.add_word token_map "==" TokEq;;
Trie.add_word token_map "!=" TokNeq;;
Trie.add_word token_map "!" TokNot;;
Trie.add_word token_map "<" TokLt;;
Trie.add_word token_map "<=" TokLe;;
Trie.add_word token_map ">" TokGt;;
Trie.add_word token_map ">=" TokGe;;
Trie.add_word token_map "(" TokLp;;
Trie.add_word token_map ")" TokRp;;
Trie.add_word token_map "{" TokLb;;
Trie.add_word token_map "}" TokRb;;
Trie.add_word token_map "[" TokLs;;
Trie.add_word token_map "]" TokRs;;
Trie.add_word token_map ";" TokSemi;;
Trie.add_word token_map ":" TokColon;;
Trie.add_word token_map "," TokComa;;
Trie.add_word token_map "." TokDot;;
Trie.add_word token_map "&" TokAmp

let tok_kind_to_str tk =
  match tk with
  | TokNumber -> "Number"
  | TokIdent -> "Ident"
  | TokString -> "String"
  | TokAdd -> "Add"
  | TokMul -> "Mul"
  | TokSub -> "Sub"
  | TokAmp -> "Amp"
  | TokAssign -> "Assign"
  | TokEq -> "Eq"
  | TokNeq -> "Neq"
  | TokIf -> "If"
  | TokReturn -> "Return"
  | TokLt -> "Lt"
  | TokLp -> "Lp"
  | TokRp -> "Rp"
  | TokLb -> "Lb"
  | TokRb -> "Rb"
  | TokLs -> "Ls"
  | TokRs -> "Rs"
  | TokFn -> "Fn"
  | TokColon -> "Colon"
  | TokSemi -> "Semi"
  | TokComa -> "Coma"
  | TokEnd -> "End"
  | _ -> "Unimplemented"

let tok_to_str tok =
  match tok.kind with
  | TokNumber -> "Number(" ^ tok.str ^ ")"
  | TokIdent -> "Ident(" ^ tok.str ^ ")"
  | TokString -> "String(\"" ^ tok.str ^ "\")"
  | _ -> tok_kind_to_str tok.kind

let tok_list_to_str toks =
  toks |> List.map tok_to_str
  |> List.fold_left
       (fun acc s ->
         match acc with None -> Some s | Some acc -> Some (acc ^ "," ^ s))
       None
  |> Option.value ~default:""

type state =
  | Normal
  | InIdent
  | InComment
  | InNumber
  | InString
  | InEscape
  | Other

type builder = {
  src : string;
  state : state;
  start : int;
  loc : int;
  toks : token list;
  finder : token_kind Trie.finder_type;
}

let tokenize src : token list =
  let b : builder =
    {
      src;
      loc = 0;
      start = 0;
      state = Normal;
      toks = [];
      finder = Trie.finder token_map;
    }
  in

  let cur_str b = String.sub b.src b.start (b.loc - b.start) in

  let emit_tok kind b =
    let escape_map =
      CharMap.of_list [ ('n', '\n'); ('t', '\t'); ('\\', '\\'); ('"', '"') ]
    in
    let loc = Location.Range (b.start, b.loc) in
    let str =
      match kind with
      | TokString ->
          String.fold_left
            (fun (acc, state) c ->
              match (c, state) with
              | '\\', 0 -> (acc, 1)
              | _, 0 -> (Printf.sprintf "%s%c" acc c, 0)
              | _, 1 -> (
                  match CharMap.find_opt c escape_map with
                  | Some c -> (Printf.sprintf "%s%c" acc c, 0)
                  | None ->
                      Error.fail_at_spot "Unrecognized escape sequence" b.src
                        loc (Error.Error Unknown))
              | _ -> failwith "unreachable")
            ("", 0) (cur_str b)
          |> fst
      | _ -> cur_str b
    in
    let t = { loc; kind; str } in
    { b with toks = t :: b.toks; state = Normal }
  in

  let step b = { b with loc = b.loc + 1 } in

  let rec add_char b c =
    match (c, b.state) with
    (* Whitespace *)
    | (' ' | '\t' | '\n'), Normal -> step b
    (* Idents and keywords *)
    | ('a' .. 'z' | 'A' .. 'Z' | '_'), Normal ->
        step { b with start = b.loc; state = InIdent }
    | ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9'), InIdent -> step b
    | '%', Normal -> step { b with state = InComment }
    | '\n', InComment -> step { b with state = Normal }
    | _, InComment -> step b
    | _, InIdent -> (
        match Trie.get_word token_map (cur_str b) with
        | Some kw -> add_char (emit_tok kw b) c
        | None -> add_char (emit_tok TokIdent b) c)
    (* Numbers *)
    | '0' .. '9', Normal -> step { b with start = b.loc; state = InNumber }
    | '0' .. '9', InNumber -> step b
    | _, InNumber -> add_char (emit_tok TokNumber b) c
    (* Strings *)
    | '\"', Normal -> step { b with start = b.loc + 1; state = InString }
    | '\\', InString -> step { b with state = InEscape }
    | _, InEscape -> step { b with state = InString }
    | '\"', InString -> step (emit_tok TokString b)
    | _, InString -> step b
    (* Symbols *)
    | _, Normal ->
        add_char
          {
            b with
            finder = Trie.finder token_map;
            start = b.loc;
            state = Other;
          }
          c
    | c, Other -> (
        match Trie.push b.finder c with
        | Some next -> step { b with finder = next }
        | None -> (
            match Trie.get b.finder with
            | Some t -> add_char (emit_tok t b) c
            | None ->
                Error.fail_at_spot "Invalid token" b.src (Location.Spot b.loc)
                  (Error.Error Unknown)))
  in

  b.src ^ " "
  |> String.fold_left add_char b
  |> (fun b -> b.toks)
  |> (fun x ->
  { loc = Location.Spot (String.length src); kind = TokEnd; str = "" } :: x)
  |> List.rev
