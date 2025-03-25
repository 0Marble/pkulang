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
  | DotExpr of { obj : node; field : string; loc : Location.location }
  | Variable of { name : string; loc : Location.location }
  | Number of { num : int; loc : Location.location }
  | String of { str : string; loc : Location.location }
  | ArrayLiteral of { elems : node list; loc : Location.location }
  | StructLiteral of { typ : node; fields : node list; loc : Location.location }
  | FieldLiteral of { name : string; value : node; loc : Location.location }
  (* Coroutines *)
  | Yield of { value : node option; loc : Location.location }
  | Resume of { coroutine : node; value : node option; loc : Location.location }
  | Coroutine of { fn : node; loc : Location.location }
  (* Statements *)
  | LabeledStmt of { label : string; stmt : node; loc : Location.location }
  | LetStmt of {
      name : string;
      typ : node;
      value : node;
      loc : Location.location;
    }
  | Block of { stmts : node list; loc : Location.location }
  | IfStmt of {
      cond : node;
      if_true : node;
      if_false : node option;
      loc : Location.location;
    }
  | ForLoop of {
      var_name : string;
      iter : node;
      body : node;
      finally : node option;
      loc : Location.location;
    }
  | WhileLoop of {
      cond : node;
      body : node;
      finally : node option;
      loc : Location.location;
    }
  | Return of { value : node option; loc : Location.location }
  | Break of {
      label : string option;
      value : node option;
      loc : Location.location;
    }
  | Continue of { label : string option; loc : Location.location }
  (* Types *)
  | NamedType of { name : string; loc : Location.location }
  | ArrayType of { elem : node; size : node; loc : Location.location }
  | SliceType of { elem : node; loc : Location.location }
  | DotType of { parent : node; child : string; loc : Location.location }
  | PtrType of { sub : node; loc : Location.location }
  (* Decls *)
  | Function of {
      name : string;
      args : node list;
      ret_type : node;
      body : node;
      loc : Location.location;
    }
  | FunctionArg of { name : string; typ : node; loc : Location.location }
  | PubDecl of { decl : node; loc : Location.location }
  | Field of {
      name : string;
      typ : node;
      value : node option;
      loc : Location.location;
    }
  | Struct of { name : string; decls : node list; loc : Location.location }
  | TypeAlias of { name : string; typ : node; loc : Location.location }
  (* used as a placeholder for when an error occurs *)
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
  | DotExpr x -> Printf.sprintf "(dot %s %s)" (node_to_str x.obj) x.field
  | Variable x -> Printf.sprintf "(var %s)" x.name
  | Number x -> Printf.sprintf "(num %d)" x.num
  | String x -> Printf.sprintf "(str \"%s\")" x.str
  | ArrayLiteral x ->
      let s = Printf.sprintf "(array_literal" in
      let s =
        List.fold_left
          (fun acc s -> Printf.sprintf "%s %s" acc (node_to_str s))
          s x.elems
      in
      Printf.sprintf "%s)" s
  | StructLiteral x ->
      let s = Printf.sprintf "(struct_literal %s (fields" (node_to_str x.typ) in
      let s =
        List.fold_left
          (fun acc s -> Printf.sprintf "%s %s" acc (node_to_str s))
          s x.fields
      in
      Printf.sprintf "%s))" s
  | FieldLiteral x ->
      Printf.sprintf "(field_literal %s %s)" x.name (node_to_str x.value)
  | Yield x ->
      Printf.sprintf "(yield %s)"
        (x.value |> Option.map node_to_str |> Option.value ~default:"_")
  | Resume x ->
      Printf.sprintf "(resume %s %s)" (node_to_str x.coroutine)
        (x.value |> Option.map node_to_str |> Option.value ~default:"_")
  | Coroutine x -> Printf.sprintf "(coroutine %s)" (node_to_str x.fn)
  | LabeledStmt x -> Printf.sprintf "(label %s %s)" x.label (node_to_str x.stmt)
  | LetStmt x ->
      Printf.sprintf "(let %s %s %s)" x.name (node_to_str x.typ)
        (node_to_str x.value)
  | Block x ->
      let s = Printf.sprintf "(block" in
      let s =
        List.fold_left
          (fun acc s -> Printf.sprintf "%s %s" acc (node_to_str s))
          s x.stmts
      in
      Printf.sprintf "%s)" s
  | IfStmt x ->
      Printf.sprintf "(if %s %s %s)" (node_to_str x.cond)
        (node_to_str x.if_true)
        (x.if_false |> Option.map node_to_str |> Option.value ~default:"_")
  | WhileLoop x ->
      Printf.sprintf "(while %s %s %s)" (node_to_str x.cond)
        (node_to_str x.body)
        (x.finally |> Option.map node_to_str |> Option.value ~default:"_")
  | ForLoop x ->
      Printf.sprintf "(for %s %s %s %s)" x.var_name (node_to_str x.iter)
        (node_to_str x.body)
        (x.finally |> Option.map node_to_str |> Option.value ~default:"_")
  | Return x ->
      Printf.sprintf "(return %s)"
        (x.value |> Option.map node_to_str |> Option.value ~default:"_")
  | Break x ->
      Printf.sprintf "(break %s %s)"
        (x.label |> Option.value ~default:"_")
        (x.value |> Option.map node_to_str |> Option.value ~default:"_")
  | Continue x ->
      Printf.sprintf "(continue %s)" (x.label |> Option.value ~default:"_")
  | NamedType x -> Printf.sprintf "(type %s)" x.name
  | ArrayType x ->
      Printf.sprintf "(array %s %s)" (node_to_str x.elem) (node_to_str x.size)
  | SliceType x -> Printf.sprintf "(slice %s)" (node_to_str x.elem)
  | DotType x ->
      Printf.sprintf "(dot_type %s %s)" (node_to_str x.parent) x.child
  | PtrType x -> Printf.sprintf "(ptr_type %s)" (node_to_str x.sub)
  | Function x ->
      let s = Printf.sprintf "(fn %s" x.name in
      let s =
        List.fold_left
          (fun acc s -> Printf.sprintf "%s %s" acc (node_to_str s))
          s x.args
      in
      Printf.sprintf "%s %s %s)" s (node_to_str x.ret_type) (node_to_str x.body)
  | FunctionArg x -> Printf.sprintf "(arg %s %s)" x.name (node_to_str x.typ)
  | PubDecl x -> Printf.sprintf "(pub %s)" (node_to_str x.decl)
  | Field x ->
      Printf.sprintf "(field %s %s %s)" x.name (node_to_str x.typ)
        (x.value
        |> Option.map (fun n -> node_to_str n)
        |> Option.value ~default:"_")
  | Struct x ->
      Printf.sprintf "%s)"
        (x.decls
        |> List.fold_left
             (fun acc n -> Printf.sprintf "%s %s" acc (node_to_str n))
             (Printf.sprintf "(struct %s" x.name))
  | TypeAlias x -> Printf.sprintf "(alias %s %s)" x.name (node_to_str x.typ)
  | Invalid -> "?"
(* | _ -> "Unimplemented" *)

let node_loc n =
  match n with
  | BinOp x -> x.loc
  | UnaryOp x -> x.loc
  | Call x -> x.loc
  | Index x -> x.loc
  | Variable x -> x.loc
  | Number x -> x.loc
  | StructLiteral x -> x.loc
  | FieldLiteral x -> x.loc
  | Yield x -> x.loc
  | Resume x -> x.loc
  | Coroutine x -> x.loc
  | LetStmt x -> x.loc
  | Block x -> x.loc
  | IfStmt x -> x.loc
  | ForLoop x -> x.loc
  | WhileLoop x -> x.loc
  | Return x -> x.loc
  | Break x -> x.loc
  | Continue x -> x.loc
  | NamedType x -> x.loc
  | ArrayType x -> x.loc
  | SliceType x -> x.loc
  | PtrType x -> x.loc
  | Function x -> x.loc
  | FunctionArg x -> x.loc
  | DotExpr x -> x.loc
  | String x -> x.loc
  | ArrayLiteral x -> x.loc
  | LabeledStmt x -> x.loc
  | DotType x -> x.loc
  | Field x -> x.loc
  | Struct x -> x.loc
  | PubDecl x -> x.loc
  | TypeAlias x -> x.loc
  | Invalid -> failwith "Unreachable: node_loc"
