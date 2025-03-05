type token_kind =
  | TokNumber
  | TokIdent
  | TokString
  | TokAdd
  | TokSub
  | TokMul
  | TokDiv
  | TokLess
  | TokLessEql
  | TokLp
  | TokRp
  | TokLb
  | TokRb
  | TokLs
  | TokRs
  | TokSemi
  | TokColon
  | TokComa
  | TokAssign
  | TokEql
  | TokFn
  | TokReturn
  | TokIf
  | TokElse
  | TokWhile

type token = { str : string; loc : Location.location; kind : token_kind }

let keywords = Hashtbl.create 10;;

Hashtbl.add keywords "return" TokReturn;;
Hashtbl.add keywords "fn" TokFn;;
Hashtbl.add keywords "if" TokIf;;
Hashtbl.add keywords "else" TokElse;;
Hashtbl.add keywords "while" TokWhile

let symbols = Hashtbl.create 10;;

Hashtbl.add symbols '+' TokAdd;;
Hashtbl.add symbols '-' TokSub;;
Hashtbl.add symbols '*' TokMul;;
Hashtbl.add symbols '/' TokDiv;;
Hashtbl.add symbols '(' TokLp;;
Hashtbl.add symbols ')' TokRp;;
Hashtbl.add symbols '{' TokLb;;
Hashtbl.add symbols '}' TokRb;;
Hashtbl.add symbols '[' TokLs;;
Hashtbl.add symbols ']' TokRs;;
Hashtbl.add symbols ';' TokSemi;;
Hashtbl.add symbols ':' TokColon;;
Hashtbl.add symbols ',' TokComa

let tok_to_str tok =
  match tok.kind with
  | TokNumber -> "Number(" ^ tok.str ^ ")"
  | TokIdent -> "Ident(" ^ tok.str ^ ")"
  | TokString -> "String(\"" ^ tok.str ^ "\")"
  | TokAdd -> "Add"
  | TokMul -> "Mul"
  | TokSub -> "Sub"
  | TokAssign -> "Assign"
  | TokEql -> "Eql"
  | TokIf -> "If"
  | TokReturn -> "Return"
  | TokLess -> "Less"
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
  | _ -> "Unimplemented"

let tok_list_to_str toks =
  toks |> List.map tok_to_str
  |> List.fold_left
       (fun acc s ->
         match acc with None -> Some s | Some acc -> Some (acc ^ "," ^ s))
       None
  |> Option.value ~default:""

type builder = {
  src : string;
  state : int;
  start : int;
  loc : int;
  toks : token list;
}

let tokenize src : token list =
  let b : builder = { src; loc = 0; start = 0; state = 0; toks = [] } in

  let cur_str b = String.sub b.src b.start (b.loc - b.start) in

  let emit_tok kind b =
    {
      b with
      toks =
        { kind; loc = Location.Range (b.start, b.loc); str = cur_str b }
        :: b.toks;
      state = 0;
    }
  in

  let step b = { b with loc = b.loc + 1 } in

  let rec add_char b c =
    match (c, b.state) with
    (* Whitespace *)
    | (' ' | '\t' | '\n'), 0 -> step b
    (* Idents and keywords *)
    | ('a' .. 'z' | 'A' .. 'Z' | '_'), 0 ->
        step { b with start = b.loc; state = 1 }
    | ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9'), 1 -> step b
    | _, 1 -> (
        match Hashtbl.find_opt keywords (cur_str b) with
        | Some kw -> add_char (emit_tok kw b) c
        | None -> add_char (emit_tok TokIdent b) c)
    (* Numbers *)
    | '0' .. '9', 0 -> step { b with start = b.loc; state = 2 }
    | '0' .. '9', 2 -> step b
    | _, 2 -> add_char (emit_tok TokNumber b) c
    (* Strings *)
    | '\"', 0 -> step { b with start = b.loc + 1; state = 5 }
    | '\"', 5 -> step (emit_tok TokString b)
    | _, 5 -> step b
    (* Compound Symbols *)
    | '=', 0 -> step { b with start = b.loc; state = 3 }
    | '=', 3 -> emit_tok TokEql (step b)
    | _, 3 -> add_char (emit_tok TokAssign b) c
    | '<', 0 -> step { b with start = b.loc; state = 4 }
    | '=', 4 -> emit_tok TokLessEql (step b)
    | _, 4 -> add_char (emit_tok TokLess b) c
    (* Simple Symbols *)
    | _, 0 -> (
        match Hashtbl.find_opt symbols c with
        | Some tok -> emit_tok tok (step { b with start = b.loc })
        | None -> Error.fail_at_spot "Invalid token" b.src (Location.Spot b.loc)
        )
    (* Error*)
    | _ -> Error.fail_at_spot "Invalid token" b.src (Location.Spot b.loc)
  in

  b.src ^ " " |> String.fold_left add_char b |> (fun b -> b.toks) |> List.rev
