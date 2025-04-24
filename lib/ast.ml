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
  param_type : typ option;
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
  | Invalid
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
  | YieldExpr of yield_expr
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
  param : typ option;
  node_idx : int;
  loc : Location.location;
}

and co_obj_type = {
  yield : typ;
  param : typ option;
  node_idx : int;
  loc : Location.location;
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

and yield_expr = {
  value : expr option;
  node_idx : int;
  loc : Location.location;
}

and resume_expr = {
  coroutine : expr;
  value : expr option;
  node_idx : int;
  loc : Location.location;
}

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
  | YieldExpr of yield_expr
  | CreateExpr of create_expr
  | ResumeExpr of resume_expr
  | ForLoop of for_loop
  | WhileLoop of while_loop
  | ContinueStmt of continue_stmt
  | BreakStmt of break_stmt
  | IfStmt of if_stmt
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
  | Invalid -> failwith "Unreachable"
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
  | YieldExpr y -> YieldExpr y
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
  | YieldExpr x -> x.loc
  | CreateExpr x -> x.loc
  | ResumeExpr x -> x.loc
  | ForLoop x -> x.loc
  | WhileLoop x -> x.loc
  | ContinueStmt x -> x.loc
  | BreakStmt x -> x.loc
  | IfStmt x -> x.loc
  | ReturnStmt x -> x.loc
  | FieldLiteral x -> x.loc
  | Invalid -> failwith "unreachable"
