type root = { stmts : top_stmt list; node_idx : int; loc : Location.location }

and top_stmt =
  | FnDecl of fn_decl
  | StructDecl of struct_decl
  | CoDecl of co_decl
  | LetStmt of let_stmt
  | AliasStmt of alias_stmt

and fn_decl = {
  name : string;
  args : argument list;
  ret_type : typ;
  body : stmt;
  node_idx : int;
  loc : Location.location;
}

and struct_decl = {
  name : string;
  decls : decl list;
  node_idx : int;
  loc : Location.location;
}

and co_decl = {
  name : string;
  args : argument list;
  yield_type : typ;
  body : stmt;
  node_idx : int;
  loc : Location.location;
}

and let_stmt = {
  var_name : string;
  var_type : typ;
  value : expr;
  node_idx : int;
  loc : Location.location;
}

and alias_stmt = {
  type_name : string;
  other_type : typ;
  node_idx : int;
  loc : Location.location;
}

and argument = {
  name : string;
  arg_type : typ;
  node_idx : int;
  loc : Location.location;
}

and typ =
  | NamedType of named_type
  | ArrayType of array_type
  | DotType of dot_type
  | FnType of fn_type
  | CoType of co_type
  | CoObjType of co_obj_type

and block = { stmts : stmt list; node_idx : int; loc : Location.location }

and field = {
  var_name : string;
  field_type : typ;
  value : expr option;
  node_idx : int;
  loc : Location.location;
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

and named_type = { name : string; node_idx : int; loc : Location.location }
and array_type = { elem : typ; node_idx : int; loc : Location.location }

and dot_type = {
  parent : typ;
  child : string;
  node_idx : int;
  loc : Location.location;
}

and fn_type = {
  args : typ list;
  ret : typ;
  node_idx : int;
  loc : Location.location;
}

and co_type = {
  args : typ list;
  yield : typ;
  node_idx : int;
  loc : Location.location;
}

and co_obj_type = { yield : typ; node_idx : int; loc : Location.location }

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
  lhs : expr;
  rhs : expr;
  op : Tokenizer.token;
  node_idx : int;
  loc : Location.location;
}

and unary_expr = {
  sub_expr : expr;
  op : Tokenizer.token;
  node_idx : int;
  loc : Location.location;
}

and call_expr = {
  fn : expr;
  params : expr list;
  node_idx : int;
  loc : Location.location;
}

and index_expr = {
  arr : expr;
  idx : expr list;
  node_idx : int;
  loc : Location.location;
}

and dot_expr = {
  obj : expr;
  field : string;
  node_idx : int;
  loc : Location.location;
}

and var_expr = { name : string; node_idx : int; loc : Location.location }
and num_expr = { num : int; node_idx : int; loc : Location.location }
and string_expr = { str : string; node_idx : int; loc : Location.location }

and array_literal = {
  elems : expr list;
  node_idx : int;
  loc : Location.location;
}

and new_expr = {
  typ : typ;
  fields : field_literal list;
  node_idx : int;
  loc : Location.location;
}

and null_literal = { node_idx : int; loc : Location.location }

and yield_stmt = {
  value : expr option;
  node_idx : int;
  loc : Location.location;
}

and resume_expr = { coroutine : expr; node_idx : int; loc : Location.location }

and create_expr = {
  coroutine : expr;
  params : expr list;
  node_idx : int;
  loc : Location.location;
}

and for_loop = {
  iter_var : string;
  iterator : expr;
  body : stmt;
  node_idx : int;
  loc : Location.location;
}

and while_loop = {
  condition : expr;
  body : stmt;
  node_idx : int;
  loc : Location.location;
}

and continue_stmt = { node_idx : int; loc : Location.location }
and break_stmt = { node_idx : int; loc : Location.location }

and if_stmt = {
  condition : expr;
  if_true : stmt;
  if_false : stmt option;
  node_idx : int;
  loc : Location.location;
}

and return_stmt = {
  value : expr option;
  node_idx : int;
  loc : Location.location;
}

and if_resume_stmt = {
  var : string option;
  coroutine : expr;
  if_ok : stmt;
  if_bad : stmt option;
  node_idx : int;
  loc : Location.location;
}

and field_literal = {
  name : string;
  value : expr;
  node_idx : int;
  loc : Location.location;
}

and node =
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
  | Invalid

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
  | Invalid -> failwith "unreachable"

let rec node_to_str (n : node) : string =
  match n with
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
        (x.ret_type |> type_to_node |> node_to_str)
        (x.body |> stmt_to_node |> node_to_str)
  | StructDecl x ->
      Printf.sprintf "%s)"
        (x.decls
        |> List.fold_left
             (fun acc n ->
               Printf.sprintf "%s %s" acc (n |> decl_to_node |> node_to_str))
             (Printf.sprintf "(struct %s" x.name))
  | CoDecl _ -> "?"
  | LetStmt x ->
      Printf.sprintf "(let %s %s %s)" x.var_name
        (x.var_type |> type_to_node |> node_to_str)
        (x.value |> expr_to_node |> node_to_str)
  | AliasStmt x ->
      Printf.sprintf "(alias %s %s)" x.type_name
        (x.other_type |> type_to_node |> node_to_str)
  | Argument x ->
      Printf.sprintf "(arg %s %s)" x.name
        (x.arg_type |> type_to_node |> node_to_str)
  | NamedType x -> Printf.sprintf "(type %s)" x.name
  | ArrayType x ->
      Printf.sprintf "(array %s)" (x.elem |> type_to_node |> node_to_str)
  | DotType x ->
      Printf.sprintf "(dot_type %s %s)"
        (x.parent |> type_to_node |> node_to_str)
        x.child
  | FnType _ -> "?"
  | CoType _ -> "?"
  | CoObjType _ -> "?"
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
      Printf.sprintf "(field %s %s %s)" x.var_name
        (x.field_type |> type_to_node |> node_to_str)
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
        x.params
      |> Printf.sprintf "%s)"
  | IndexExpr x ->
      List.fold_left
        (fun s n -> Printf.sprintf "%s %s" s (n |> expr_to_node |> node_to_str))
        (Printf.sprintf "(idx %s" (x.arr |> expr_to_node |> node_to_str))
        x.idx
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
        x.params
      ^ ")"
  | ResumeExpr x ->
      Printf.sprintf "(resume %s)" (x.coroutine |> expr_to_node |> node_to_str)
  | ForLoop x ->
      Printf.sprintf "(for %s %s %s)" x.iter_var
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
  | Invalid -> "?"
