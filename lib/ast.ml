type root = { stmts : top_stmt list; loc : Location.location; node_idx : int }

and top_stmt =
  | FnDecl of fn_decl
  | StructDecl of struct_decl
  | CoDecl of co_decl
  | LetStmt of let_stmt
  | AliasStmt of alias_stmt

and fn_decl = {
  parent : node ref;
  name : string;
  args : argument list;
  ret : typ;
  body : stmt;
  loc : Location.location;
  node_idx : int;
}

and struct_decl = {
  parent : node ref;
  name : string;
  decls : decl list;
  loc : Location.location;
  node_idx : int;
}

and co_decl = {
  parent : node ref;
  name : string;
  args : argument list;
  yield : typ;
  body : stmt;
  loc : Location.location;
  node_idx : int;
}

and let_stmt = {
  parent : node ref;
  name : string;
  typ : typ;
  value : expr;
  loc : Location.location;
  node_idx : int;
}

and alias_stmt = {
  parent : node ref;
  name : string;
  typ : typ;
  loc : Location.location;
  node_idx : int;
}

and argument = {
  parent : node ref;
  name : string;
  typ : typ;
  loc : Location.location;
  node_idx : int;
}

and typ =
  | NamedType of named_type
  | ArrayType of array_type
  | DotType of dot_type
  | FnType of fn_type
  | CoType of co_type
  | CoObjType of co_obj_type

and block = {
  parent : node ref;
  stmts : stmt list;
  loc : Location.location;
  node_idx : int;
}

and field = {
  parent : node ref;
  name : string;
  typ : typ;
  value : expr option;
  loc : Location.location;
  node_idx : int;
}

and decl =
  | FnDecl of fn_decl
  | CoDecl of co_decl
  | LetStmt of let_stmt
  | StructDecl of struct_decl
  | Field of field

and expr =
  | BinExpr of bin_expr
  | UnaryExpr of unary_expr
  | CallExpr of call_expr
  | IndexExpr of index_expr
  | DotExpr of dot_expr
  | VarExpr of var_expr
  | NumExpr of num_expr
  | StringExpr of string_expr
  | ArrayLiteral of array_literal
  | NullLiteral of null_literal
  | NewExpr of new_expr
  | CreateExpr of create_expr
  | ResumeExpr of resume_expr

and named_type = {
  parent : node ref;
  name : string;
  loc : Location.location;
  node_idx : int;
}

and array_type = {
  parent : node ref;
  elem : typ;
  loc : Location.location;
  node_idx : int;
}

and dot_type = {
  parent : node ref;
  namespace : typ;
  name : string;
  loc : Location.location;
  node_idx : int;
}

and fn_type = {
  parent : node ref;
  args : typ list;
  ret : typ;
  loc : Location.location;
  node_idx : int;
}

and co_type = {
  parent : node ref;
  args : typ list;
  yield : typ;
  loc : Location.location;
  node_idx : int;
}

and co_obj_type = {
  parent : node ref;
  yield : typ;
  loc : Location.location;
  node_idx : int;
}

and stmt =
  | Block of block
  | LetStmt of let_stmt
  | ForLoop of for_loop
  | WhileLoop of while_loop
  | ContinueStmt of continue_stmt
  | BreakStmt of break_stmt
  | IfStmt of if_stmt
  | ReturnStmt of return_stmt
  | Expr of expr
  | FnDecl of fn_decl
  | StructDecl of struct_decl
  | CoDecl of co_decl
  | AliasStmt of alias_stmt
  | IfResumeStmt of if_resume_stmt
  | YieldStmt of yield_stmt

and bin_expr = {
  parent : node ref;
  lhs : expr;
  rhs : expr;
  op : Tokenizer.token;
  loc : Location.location;
  node_idx : int;
}

and unary_expr = {
  parent : node ref;
  sub_expr : expr;
  op : Tokenizer.token;
  loc : Location.location;
  node_idx : int;
}

and call_expr = {
  parent : node ref;
  fn : expr;
  args : expr list;
  loc : Location.location;
  node_idx : int;
}

and index_expr = {
  parent : node ref;
  arr : expr;
  ids : expr list;
  loc : Location.location;
  node_idx : int;
}

and dot_expr = {
  parent : node ref;
  obj : expr;
  field : string;
  loc : Location.location;
  node_idx : int;
}

and var_expr = {
  parent : node ref;
  name : string;
  loc : Location.location;
  node_idx : int;
}

and num_expr = {
  parent : node ref;
  num : int;
  loc : Location.location;
  node_idx : int;
}

and string_expr = {
  parent : node ref;
  str : string;
  loc : Location.location;
  node_idx : int;
}

and array_literal = {
  parent : node ref;
  elems : expr list;
  loc : Location.location;
  node_idx : int;
}

and new_expr = {
  parent : node ref;
  typ : typ;
  fields : field_literal list;
  loc : Location.location;
  node_idx : int;
}

and null_literal = {
  parent : node ref;
  loc : Location.location;
  node_idx : int;
}

and yield_stmt = {
  parent : node ref;
  value : expr option;
  loc : Location.location;
  node_idx : int;
}

and resume_expr = {
  parent : node ref;
  coroutine : expr;
  loc : Location.location;
  node_idx : int;
}

and create_expr = {
  parent : node ref;
  coroutine : expr;
  args : expr list;
  loc : Location.location;
  node_idx : int;
}

and for_loop = {
  parent : node ref;
  var : string;
  iterator : expr;
  body : stmt;
  loc : Location.location;
  node_idx : int;
}

and while_loop = {
  parent : node ref;
  condition : expr;
  body : stmt;
  loc : Location.location;
  node_idx : int;
}

and continue_stmt = {
  parent : node ref;
  loc : Location.location;
  node_idx : int;
}

and break_stmt = { parent : node ref; loc : Location.location; node_idx : int }

and if_stmt = {
  parent : node ref;
  condition : expr;
  if_true : stmt;
  if_false : stmt option;
  loc : Location.location;
  node_idx : int;
}

and return_stmt = {
  parent : node ref;
  value : expr option;
  loc : Location.location;
  node_idx : int;
}

and if_resume_stmt = {
  parent : node ref;
  var : string option;
  coroutine : expr;
  if_ok : stmt;
  if_bad : stmt option;
  loc : Location.location;
  node_idx : int;
}

and field_literal = {
  parent : node ref;
  name : string;
  value : expr;
  loc : Location.location;
  node_idx : int;
}

and node =
  | Invalid
  | Root of root
  | FnDecl of fn_decl
  | StructDecl of struct_decl
  | CoDecl of co_decl
  | LetStmt of let_stmt
  | AliasStmt of alias_stmt
  | Argument of argument
  | NamedType of named_type
  | ArrayType of array_type
  | DotType of dot_type
  | FnType of fn_type
  | CoType of co_type
  | CoObjType of co_obj_type
  | Block of block
  | Field of field
  | BinExpr of bin_expr
  | UnaryExpr of unary_expr
  | CallExpr of call_expr
  | IndexExpr of index_expr
  | DotExpr of dot_expr
  | VarExpr of var_expr
  | NumExpr of num_expr
  | StringExpr of string_expr
  | ArrayLiteral of array_literal
  | NullLiteral of null_literal
  | NewExpr of new_expr
  | YieldStmt of yield_stmt
  | CreateExpr of create_expr
  | ResumeExpr of resume_expr
  | ForLoop of for_loop
  | WhileLoop of while_loop
  | ContinueStmt of continue_stmt
  | BreakStmt of break_stmt
  | IfStmt of if_stmt
  | IfResumeStmt of if_resume_stmt
  | ReturnStmt of return_stmt
  | FieldLiteral of field_literal

let type_to_node (t : typ) : node =
  match t with
  | NamedType x -> NamedType x
  | ArrayType x -> ArrayType x
  | DotType x -> DotType x
  | FnType x -> FnType x
  | CoType x -> CoType x
  | CoObjType x -> CoObjType x

let expr_to_node (e : expr) : node =
  match e with
  | BinExpr y -> BinExpr y
  | UnaryExpr y -> UnaryExpr y
  | CallExpr y -> CallExpr y
  | IndexExpr y -> IndexExpr y
  | DotExpr y -> DotExpr y
  | VarExpr y -> VarExpr y
  | NumExpr y -> NumExpr y
  | StringExpr y -> StringExpr y
  | ArrayLiteral y -> ArrayLiteral y
  | NullLiteral y -> NullLiteral y
  | NewExpr y -> NewExpr y
  | CreateExpr y -> CreateExpr y
  | ResumeExpr y -> ResumeExpr y

let stmt_to_node (s : stmt) : node =
  match s with
  | Block y -> Block y
  | LetStmt y -> LetStmt y
  | ForLoop y -> ForLoop y
  | WhileLoop y -> WhileLoop y
  | ContinueStmt y -> ContinueStmt y
  | BreakStmt y -> BreakStmt y
  | IfStmt y -> IfStmt y
  | Expr y -> expr_to_node y
  | ReturnStmt y -> ReturnStmt y
  | FnDecl y -> FnDecl y
  | StructDecl y -> StructDecl y
  | CoDecl y -> CoDecl y
  | AliasStmt y -> AliasStmt y
  | IfResumeStmt y -> IfResumeStmt y
  | YieldStmt y -> YieldStmt y

let decl_to_node (d : decl) : node =
  match d with
  | FnDecl y -> FnDecl y
  | CoDecl y -> CoDecl y
  | LetStmt y -> LetStmt y
  | StructDecl y -> StructDecl y
  | Field y -> Field y

let top_stmt_to_stmt (s : top_stmt) : stmt =
  match s with
  | FnDecl x -> FnDecl x
  | StructDecl y -> StructDecl y
  | CoDecl y -> CoDecl y
  | LetStmt y -> LetStmt y
  | AliasStmt y -> AliasStmt y

let node_loc n =
  match n with
  | Invalid -> failwith "unreachable"
  | Root x -> x.loc
  | FnDecl x -> x.loc
  | StructDecl x -> x.loc
  | CoDecl x -> x.loc
  | LetStmt x -> x.loc
  | AliasStmt x -> x.loc
  | Argument x -> x.loc
  | NamedType x -> x.loc
  | ArrayType x -> x.loc
  | DotType x -> x.loc
  | FnType x -> x.loc
  | CoType x -> x.loc
  | CoObjType x -> x.loc
  | Block x -> x.loc
  | Field x -> x.loc
  | BinExpr x -> x.loc
  | UnaryExpr x -> x.loc
  | CallExpr x -> x.loc
  | IndexExpr x -> x.loc
  | DotExpr x -> x.loc
  | VarExpr x -> x.loc
  | NumExpr x -> x.loc
  | StringExpr x -> x.loc
  | ArrayLiteral x -> x.loc
  | NullLiteral x -> x.loc
  | NewExpr x -> x.loc
  | YieldStmt x -> x.loc
  | CreateExpr x -> x.loc
  | ResumeExpr x -> x.loc
  | ForLoop x -> x.loc
  | WhileLoop x -> x.loc
  | ContinueStmt x -> x.loc
  | BreakStmt x -> x.loc
  | IfStmt x -> x.loc
  | ReturnStmt x -> x.loc
  | FieldLiteral x -> x.loc
  | IfResumeStmt x -> x.loc

let rec node_to_str (n : node) : string =
  match n with
  | Invalid -> "?"
  | Root x ->
      Printf.sprintf "(root (stmts%s))"
        (x.stmts
        |> List.fold_left
             (fun acc stmt ->
               Printf.sprintf "%s %s" acc
                 (stmt |> top_stmt_to_stmt |> stmt_to_node |> node_to_str))
             "")
  | FnDecl x ->
      let s = Printf.sprintf "(fn %s" x.name in
      let s =
        List.fold_left
          (fun acc s -> Printf.sprintf "%s %s" acc (node_to_str (Argument s)))
          s x.args
      in
      Printf.sprintf "%s %s %s)" s
        (x.ret |> type_to_node |> node_to_str)
        (x.body |> stmt_to_node |> node_to_str)
  | StructDecl x ->
      Printf.sprintf "%s)"
        (x.decls
        |> List.fold_left
             (fun acc n ->
               Printf.sprintf "%s %s" acc (n |> decl_to_node |> node_to_str))
             (Printf.sprintf "(struct %s" x.name))
  | CoDecl x ->
      let s = Printf.sprintf "(co %s" x.name in
      let s =
        List.fold_left
          (fun acc s -> Printf.sprintf "%s %s" acc (node_to_str (Argument s)))
          s x.args
      in
      Printf.sprintf "%s %s %s)" s
        (x.yield |> type_to_node |> node_to_str)
        (x.body |> stmt_to_node |> node_to_str)
  | LetStmt x ->
      Printf.sprintf "(let %s %s %s)" x.name
        (x.typ |> type_to_node |> node_to_str)
        (x.value |> expr_to_node |> node_to_str)
  | AliasStmt x ->
      Printf.sprintf "(alias %s %s)" x.name
        (x.typ |> type_to_node |> node_to_str)
  | Argument x ->
      Printf.sprintf "(arg %s %s)" x.name (x.typ |> type_to_node |> node_to_str)
  | NamedType x -> Printf.sprintf "(type %s)" x.name
  | ArrayType x ->
      Printf.sprintf "(array %s)" (x.elem |> type_to_node |> node_to_str)
  | DotType x ->
      Printf.sprintf "(dot_type %s %s)"
        (x.namespace |> type_to_node |> node_to_str)
        x.name
  | FnType x ->
      let s = "(fn_type " in
      let s =
        List.fold_left
          (fun acc s ->
            Printf.sprintf "%s %s" acc (node_to_str (type_to_node s)))
          s x.args
      in
      Printf.sprintf "%s %s)" s (x.ret |> type_to_node |> node_to_str)
  | CoType x ->
      let s = "(co_type " in
      let s =
        List.fold_left
          (fun acc s ->
            Printf.sprintf "%s %s" acc (node_to_str (type_to_node s)))
          s x.args
      in
      Printf.sprintf "%s %s)" s (x.yield |> type_to_node |> node_to_str)
  | CoObjType x ->
      Printf.sprintf "(co_obj %s)" (x.yield |> type_to_node |> node_to_str)
  | Block x ->
      let s = Printf.sprintf "(block" in
      let s =
        List.fold_left
          (fun acc s ->
            Printf.sprintf "%s %s" acc (s |> stmt_to_node |> node_to_str))
          s x.stmts
      in
      Printf.sprintf "%s)" s
  | Field x ->
      Printf.sprintf "(field %s %s %s)" x.name
        (x.typ |> type_to_node |> node_to_str)
        (x.value
        |> Option.map (fun n -> n |> expr_to_node |> node_to_str)
        |> Option.value ~default:"_")
  | BinExpr x ->
      Printf.sprintf "(bin %s %s %s)"
        (Tokenizer.tok_to_str x.op)
        (x.lhs |> expr_to_node |> node_to_str)
        (x.rhs |> expr_to_node |> node_to_str)
  | UnaryExpr x ->
      Printf.sprintf "(unary %s %s)"
        (Tokenizer.tok_to_str x.op)
        (x.sub_expr |> expr_to_node |> node_to_str)
  | CallExpr x ->
      List.fold_left
        (fun s n -> Printf.sprintf "%s %s" s (n |> expr_to_node |> node_to_str))
        (Printf.sprintf "(call %s" (x.fn |> expr_to_node |> node_to_str))
        x.args
      |> Printf.sprintf "%s)"
  | IndexExpr x ->
      List.fold_left
        (fun s n -> Printf.sprintf "%s %s" s (n |> expr_to_node |> node_to_str))
        (Printf.sprintf "(idx %s" (x.arr |> expr_to_node |> node_to_str))
        x.ids
      |> Printf.sprintf "%s)"
  | DotExpr x ->
      Printf.sprintf "(dot %s %s)"
        (x.obj |> expr_to_node |> node_to_str)
        x.field
  | VarExpr x -> Printf.sprintf "(var %s)" x.name
  | NumExpr x -> Printf.sprintf "(num %d)" x.num
  | StringExpr x -> Printf.sprintf "(str \"%s\")" x.str
  | ArrayLiteral x ->
      let s = Printf.sprintf "(array_literal" in
      let s =
        List.fold_left
          (fun acc s ->
            Printf.sprintf "%s %s" acc (s |> expr_to_node |> node_to_str))
          s x.elems
      in
      Printf.sprintf "%s)" s
  | NullLiteral _ -> "(null)"
  | NewExpr x ->
      let s =
        Printf.sprintf "(new %s (fields" (x.typ |> type_to_node |> node_to_str)
      in
      let s =
        List.fold_left
          (fun acc s ->
            Printf.sprintf "%s %s" acc (FieldLiteral s |> node_to_str))
          s x.fields
      in
      Printf.sprintf "%s))" s
  | YieldStmt x ->
      Printf.sprintf "(yield %s)"
        (x.value |> Option.map expr_to_node |> Option.map node_to_str
       |> Option.value ~default:"_")
  | CreateExpr x ->
      List.fold_left
        (fun acc y -> acc ^ " " ^ (y |> expr_to_node |> node_to_str))
        (Printf.sprintf "(create %s"
           (x.coroutine |> expr_to_node |> node_to_str))
        x.args
      ^ ")"
  | ResumeExpr x ->
      Printf.sprintf "(resume %s)" (x.coroutine |> expr_to_node |> node_to_str)
  | ForLoop x ->
      Printf.sprintf "(for %s %s %s)" x.var
        (x.iterator |> expr_to_node |> node_to_str)
        (x.body |> stmt_to_node |> node_to_str)
  | WhileLoop x ->
      Printf.sprintf "(while %s %s)"
        (x.condition |> expr_to_node |> node_to_str)
        (x.body |> stmt_to_node |> node_to_str)
  | ContinueStmt _ -> Printf.sprintf "(continue)"
  | BreakStmt _ -> Printf.sprintf "(break)"
  | IfStmt x ->
      Printf.sprintf "(if %s %s %s)"
        (x.condition |> expr_to_node |> node_to_str)
        (x.if_true |> stmt_to_node |> node_to_str)
        (x.if_false |> Option.map stmt_to_node |> Option.map node_to_str
       |> Option.value ~default:"_")
  | IfResumeStmt x ->
      Printf.sprintf "(if_resume %s %s %s %s)"
        (Option.value ~default:"_" x.var)
        (x.coroutine |> expr_to_node |> node_to_str)
        (x.if_ok |> stmt_to_node |> node_to_str)
        (x.if_bad |> Option.map stmt_to_node |> Option.map node_to_str
       |> Option.value ~default:"_")
  | ReturnStmt x ->
      Printf.sprintf "(return %s)"
        (x.value |> Option.map expr_to_node |> Option.map node_to_str
       |> Option.value ~default:"_")
  | FieldLiteral x ->
      Printf.sprintf "(field_literal %s %s)" x.name
        (x.value |> expr_to_node |> node_to_str)

let node_children n =
  match n with
  | Invalid -> failwith "unreachable"
  | Root x -> List.map (fun y -> top_stmt_to_stmt y |> stmt_to_node) x.stmts
  | FnDecl x ->
      type_to_node x.ret :: stmt_to_node x.body
      :: List.map (fun y -> Argument y) x.args
  | StructDecl x -> List.map decl_to_node x.decls
  | CoDecl x ->
      type_to_node x.yield :: stmt_to_node x.body
      :: List.map (fun y -> Argument y) x.args
  | LetStmt x -> [ expr_to_node x.value; type_to_node x.typ ]
  | AliasStmt x -> [ type_to_node x.typ ]
  | Argument x -> [ type_to_node x.typ ]
  | NamedType _ -> []
  | ArrayType x -> [ type_to_node x.elem ]
  | DotType x -> [ type_to_node x.namespace ]
  | FnType x -> type_to_node x.ret :: List.map type_to_node x.args
  | CoType x -> type_to_node x.yield :: List.map type_to_node x.args
  | CoObjType x -> [ type_to_node x.yield ]
  | Block x -> List.map stmt_to_node x.stmts
  | Field x -> [ type_to_node x.typ ]
  | BinExpr x -> [ expr_to_node x.lhs; expr_to_node x.rhs ]
  | UnaryExpr x -> [ expr_to_node x.sub_expr ]
  | CallExpr x -> expr_to_node x.fn :: List.map expr_to_node x.args
  | IndexExpr x -> expr_to_node x.arr :: List.map expr_to_node x.ids
  | DotExpr x -> [ expr_to_node x.obj ]
  | VarExpr _ -> []
  | NumExpr _ -> []
  | StringExpr _ -> []
  | ArrayLiteral x -> List.map expr_to_node x.elems
  | NullLiteral _ -> []
  | NewExpr x ->
      type_to_node x.typ :: List.map (fun y -> FieldLiteral y) x.fields
  | YieldStmt x -> x.value |> Option.map expr_to_node |> Option.to_list
  | CreateExpr x -> expr_to_node x.coroutine :: List.map expr_to_node x.args
  | ResumeExpr x -> [ expr_to_node x.coroutine ]
  | ForLoop x -> [ expr_to_node x.iterator; stmt_to_node x.body ]
  | WhileLoop x -> [ expr_to_node x.condition; stmt_to_node x.body ]
  | ContinueStmt _ -> []
  | BreakStmt _ -> []
  | IfStmt x ->
      expr_to_node x.condition :: stmt_to_node x.if_true
      :: (x.if_false |> Option.map stmt_to_node |> Option.to_list)
  | IfResumeStmt x ->
      expr_to_node x.coroutine :: stmt_to_node x.if_ok
      :: (x.if_bad |> Option.map stmt_to_node |> Option.to_list)
  | ReturnStmt x -> x.value |> Option.map expr_to_node |> Option.to_list
  | FieldLiteral x -> [ expr_to_node x.value ]

let node_parent n =
  match n with
  | Invalid -> failwith "unreachable"
  | Root _ -> None
  | FnDecl x -> Some !(x.parent)
  | StructDecl x -> Some !(x.parent)
  | CoDecl x -> Some !(x.parent)
  | LetStmt x -> Some !(x.parent)
  | AliasStmt x -> Some !(x.parent)
  | Argument x -> Some !(x.parent)
  | NamedType x -> Some !(x.parent)
  | ArrayType x -> Some !(x.parent)
  | DotType x -> Some !(x.parent)
  | FnType x -> Some !(x.parent)
  | CoType x -> Some !(x.parent)
  | CoObjType x -> Some !(x.parent)
  | Block x -> Some !(x.parent)
  | Field x -> Some !(x.parent)
  | BinExpr x -> Some !(x.parent)
  | UnaryExpr x -> Some !(x.parent)
  | CallExpr x -> Some !(x.parent)
  | IndexExpr x -> Some !(x.parent)
  | DotExpr x -> Some !(x.parent)
  | VarExpr x -> Some !(x.parent)
  | NumExpr x -> Some !(x.parent)
  | StringExpr x -> Some !(x.parent)
  | ArrayLiteral x -> Some !(x.parent)
  | NullLiteral x -> Some !(x.parent)
  | NewExpr x -> Some !(x.parent)
  | YieldStmt x -> Some !(x.parent)
  | CreateExpr x -> Some !(x.parent)
  | ResumeExpr x -> Some !(x.parent)
  | ForLoop x -> Some !(x.parent)
  | WhileLoop x -> Some !(x.parent)
  | ContinueStmt x -> Some !(x.parent)
  | BreakStmt x -> Some !(x.parent)
  | IfStmt x -> Some !(x.parent)
  | IfResumeStmt x -> Some !(x.parent)
  | ReturnStmt x -> Some !(x.parent)
  | FieldLiteral x -> Some !(x.parent)

let set_parent p n =
  match n with
  | Invalid -> failwith "unreachable"
  | Root _ -> failwith "no parent on root"
  | FnDecl x -> x.parent := p
  | StructDecl x -> x.parent := p
  | CoDecl x -> x.parent := p
  | LetStmt x -> x.parent := p
  | AliasStmt x -> x.parent := p
  | Argument x -> x.parent := p
  | NamedType x -> x.parent := p
  | ArrayType x -> x.parent := p
  | DotType x -> x.parent := p
  | FnType x -> x.parent := p
  | CoType x -> x.parent := p
  | CoObjType x -> x.parent := p
  | Block x -> x.parent := p
  | Field x -> x.parent := p
  | BinExpr x -> x.parent := p
  | UnaryExpr x -> x.parent := p
  | CallExpr x -> x.parent := p
  | IndexExpr x -> x.parent := p
  | DotExpr x -> x.parent := p
  | VarExpr x -> x.parent := p
  | NumExpr x -> x.parent := p
  | StringExpr x -> x.parent := p
  | ArrayLiteral x -> x.parent := p
  | NullLiteral x -> x.parent := p
  | NewExpr x -> x.parent := p
  | YieldStmt x -> x.parent := p
  | CreateExpr x -> x.parent := p
  | ResumeExpr x -> x.parent := p
  | ForLoop x -> x.parent := p
  | WhileLoop x -> x.parent := p
  | ContinueStmt x -> x.parent := p
  | BreakStmt x -> x.parent := p
  | IfStmt x -> x.parent := p
  | IfResumeStmt x -> x.parent := p
  | ReturnStmt x -> x.parent := p
  | FieldLiteral x -> x.parent := p

let visit_all_nodes (map : node -> 't) (root : root) : 't list =
  let rec visit_node n =
    map n :: (node_children n |> List.map visit_node |> List.concat)
  in
  visit_node (Root root)

let rec find_child (pred : node -> bool) (start : node) : node option =
  match node_children start |> List.find_opt pred with
  | Some n -> Some n
  | None -> node_children start |> List.find_map (find_child pred)

let node_eql a b =
  let node_idx n =
    match n with
    | Invalid -> failwith "unreachable"
    | Root x -> x.node_idx
    | FnDecl x -> x.node_idx
    | StructDecl x -> x.node_idx
    | CoDecl x -> x.node_idx
    | LetStmt x -> x.node_idx
    | AliasStmt x -> x.node_idx
    | Argument x -> x.node_idx
    | NamedType x -> x.node_idx
    | ArrayType x -> x.node_idx
    | DotType x -> x.node_idx
    | FnType x -> x.node_idx
    | CoType x -> x.node_idx
    | CoObjType x -> x.node_idx
    | Block x -> x.node_idx
    | Field x -> x.node_idx
    | BinExpr x -> x.node_idx
    | UnaryExpr x -> x.node_idx
    | CallExpr x -> x.node_idx
    | IndexExpr x -> x.node_idx
    | DotExpr x -> x.node_idx
    | VarExpr x -> x.node_idx
    | NumExpr x -> x.node_idx
    | StringExpr x -> x.node_idx
    | ArrayLiteral x -> x.node_idx
    | NullLiteral x -> x.node_idx
    | NewExpr x -> x.node_idx
    | YieldStmt x -> x.node_idx
    | CreateExpr x -> x.node_idx
    | ResumeExpr x -> x.node_idx
    | ForLoop x -> x.node_idx
    | WhileLoop x -> x.node_idx
    | ContinueStmt x -> x.node_idx
    | BreakStmt x -> x.node_idx
    | IfStmt x -> x.node_idx
    | IfResumeStmt x -> x.node_idx
    | ReturnStmt x -> x.node_idx
    | FieldLiteral x -> x.node_idx
  in
  node_idx a = node_idx b

type node_kind =
  | Root
  | FnDecl
  | StructDecl
  | CoDecl
  | LetStmt
  | AliasStmt
  | Argument
  | NamedType
  | ArrayType
  | DotType
  | FnType
  | CoType
  | CoObjType
  | Block
  | Field
  | BinExpr
  | UnaryExpr
  | CallExpr
  | IndexExpr
  | DotExpr
  | VarExpr
  | NumExpr
  | StringExpr
  | ArrayLiteral
  | NullLiteral
  | NewExpr
  | YieldStmt
  | CreateExpr
  | ResumeExpr
  | ForLoop
  | WhileLoop
  | ContinueStmt
  | BreakStmt
  | IfStmt
  | IfResumeStmt
  | ReturnStmt
  | FieldLiteral

let get_node_kind node =
  match node with
  | Invalid -> failwith "unreachable"
  | Root _ -> Root
  | FnDecl _ -> FnDecl
  | StructDecl _ -> StructDecl
  | CoDecl _ -> CoDecl
  | LetStmt _ -> LetStmt
  | AliasStmt _ -> AliasStmt
  | Argument _ -> Argument
  | NamedType _ -> NamedType
  | ArrayType _ -> ArrayType
  | DotType _ -> DotType
  | FnType _ -> FnType
  | CoType _ -> CoType
  | CoObjType _ -> CoObjType
  | Block _ -> Block
  | Field _ -> Field
  | BinExpr _ -> BinExpr
  | UnaryExpr _ -> UnaryExpr
  | CallExpr _ -> CallExpr
  | IndexExpr _ -> IndexExpr
  | DotExpr _ -> DotExpr
  | VarExpr _ -> VarExpr
  | NumExpr _ -> NumExpr
  | StringExpr _ -> StringExpr
  | ArrayLiteral _ -> ArrayLiteral
  | NullLiteral _ -> NullLiteral
  | NewExpr _ -> NewExpr
  | YieldStmt _ -> YieldStmt
  | CreateExpr _ -> CreateExpr
  | ResumeExpr _ -> ResumeExpr
  | ForLoop _ -> ForLoop
  | WhileLoop _ -> WhileLoop
  | ContinueStmt _ -> ContinueStmt
  | BreakStmt _ -> BreakStmt
  | IfStmt _ -> IfStmt
  | IfResumeStmt _ -> IfResumeStmt
  | ReturnStmt _ -> ReturnStmt
  | FieldLiteral _ -> FieldLiteral

let find_child_of_kind kind start =
  find_child (fun c -> get_node_kind c = kind) start
