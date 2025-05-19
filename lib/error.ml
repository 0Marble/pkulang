type error_code =
  | Unknown
  | DanglingElse
  | UnexpectedEnd
  | InvalidExpression
  | UnbalancedBrackets
  | Eval

exception Error of error_code

let fail_at_spot msg src loc (ec : exn) =
  let loc_str = Location.loc_to_str src loc in
  Printf.eprintf "ERROR: %s\n\n%s\n" msg loc_str;
  flush stderr;
  raise ec
