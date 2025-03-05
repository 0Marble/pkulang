type node =
  | BinOp of {
      lhs : node;
      rhs : node;
      op : Tokenizer.token;
      loc : Location.location;
    }
  | UnaryOp of { sub : node; op : Tokenizer.token; loc : Location.location }
  | Call of { fn : node; args : node list; loc : Location.location }
  | Index of { arr : node; coords : node list; loc : Location.location }
  | Variable of { name : string; loc : Location.location }
  | Number of { num : int; loc : Location.location }
  | LetStmt of { name : string; value : node; loc : Location.location }
  | Block of { stmts : node list; loc : Location.location }
  | Invalid

let rec node_to_str n =
  match n with
  | BinOp x ->
      Printf.sprintf "(bin %s %s %s)"
        (Tokenizer.tok_to_str x.op)
        (node_to_str x.lhs) (node_to_str x.rhs)
  | UnaryOp x ->
      Printf.sprintf "(unary %s %s)"
        (Tokenizer.tok_to_str x.op)
        (node_to_str x.sub)
  | Call x ->
      List.fold_left
        (fun s n -> Printf.sprintf "%s %s" s (node_to_str n))
        (Printf.sprintf "(call %s" (node_to_str x.fn))
        x.args
      |> Printf.sprintf "%s)"
  | Index x ->
      List.fold_left
        (fun s n -> Printf.sprintf "%s %s" s (node_to_str n))
        (Printf.sprintf "(idx %s" (node_to_str x.arr))
        x.coords
      |> Printf.sprintf "%s)"
  | Variable x -> Printf.sprintf "(var %s)" x.name
  | Number x -> Printf.sprintf "(num %d)" x.num
  | Invalid -> "?"
  | _ -> "Unimplemented"

let node_loc n =
  match n with
  | BinOp x -> x.loc
  | UnaryOp x -> x.loc
  | Call x -> x.loc
  | Variable x -> x.loc
  | Number x -> x.loc
  | LetStmt x -> x.loc
  | Block x -> x.loc
  | _ -> failwith "Unreachable"
