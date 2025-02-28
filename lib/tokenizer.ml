type token_kind = TokNumber | TokIdent | TokString | TokEnd
type token = { str : string; loc : int; kind : token_kind }

let tok_to_str tok =
  match tok.kind with
  | TokNumber -> "Number(" ^ tok.str ^ ")"
  | TokIdent -> "Ident(" ^ tok.str ^ ")"
  | TokString -> "String(" ^ tok.str ^ ")"
  | TokEnd -> "End"

let tokenize src =
  Printf.printf "%s" src;
  [ TokEnd ]
