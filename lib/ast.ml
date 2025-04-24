(*
   ROOT -> TOP_STMT ROOT | .
   TOP_STMT -> FN_DECL | STRUCT_DECL | CO_DECL | LET_STMT | ALIAS_STMT.
   FN_DECL -> fn ident ARGLIST TYPE STMT.
   ARGLIST -> lp ARGLIST1.
   ARGLIST1 -> ARGUMENT coma ARGLIST1 | rp.
   ARGUMENT -> ident colon TYPE.
   STRUCT_DECL -> struct ident lb DECLLIST rb.
   DECLLIST -> DECL DECLLIST | .
   DECL -> LET_STMT | FN_DECL | STRUCT_DECL | CO_DECL.
   CO_DECL -> CO_DECL_NO_PARAM | CO_DECL_WITH_PARAM.
   CO_DECL_NO_PARAM -> co ident ARGLIST TYPE STMT.
   CO_DECL_WITH_PARAM -> co ident ARGLIST colon lp TYPE rp TYPE STMT.
   LET_STMT -> let ident colon TYPE assign EXPR semi.
   ALIAS_STMT -> type ident assign TYPE semi.
   TYPE -> NAMED_TYPE | ARRAY_TYPE | FN_TYPE | CO_TYPE | CO_OBJ_TYPE | DOT_TYPE.
   PARENT_TYPE -> NAMED_TYPE | ARRAY_TYPE | DOT_TYPE.
   NAMED_TYPE -> ident.
   ARRAY_TYPE -> ls TYPE rs.
   FN_TYPE -> fn TYPE_LIST TYPE.
   TYPE_LIST -> lp TYPE_LIST1.
   TYPE_LIST1 -> TYPE coma TYPE_LIST1 | rp.
   CO_TYPE -> CO_TYPE_NO_PARAM | CO_TYPE_WITH_PARAM.
   CO_TYPE_NO_PARAM -> co TYPE_LIST TYPE.
   CO_TYPE_WITH_PARAM -> co TYPE_LIST colon lp TYPE rp TYPE.
   CO_OBJ_TYPE -> CO_OBJ_NO_PARAM | CO_OBJ_WITH_PARAM.
   CO_OBJ_NO_PARAM -> co TYPE.
   CO_OBJ_WITH_PARAM -> co colon lp TYPE rp TYPE.
   DOT_TYPE -> PARENT_TYPE dot ident.
   STMT -> FOR_LOOP | WHILE_LOOP | IF_STMT | FN_DECL | STRUCT_DECL | CO_DECL | BODY_STMT.
   BODY_STMT -> BLOCK | LET_STMT | CONTINUE_STMT | BREAK_STMT | RETURN_STMT | EXPR_STMT | ALIAS_STMT.
   BLOCK -> lb STMT_LIST rb.
   STMT_LIST -> STMT STMT_LIST | .
   FOR_LOOP -> for lp ident colon EXPR rp BODY_STMT.
   WHILE_LOOP -> while lp EXPR rp BODY_STMT.
   CONTINUE_STMT -> continue semi.
   BREAK_STMT -> break semi.
   IF_STMT -> if lp EXPR rp BODY_STMT.
   RETURN_STMT -> RETURN_VALUE | RETURN_VOID.
   RETURN_VOID -> return semi.
   RETURN_VALUE -> return EXPR semi.
   EXPR_STMT -> EXPR semi.
   EXPR -> CALLABLE_EXPR assign EXPR | EXPR1.
   EXPR1 -> EXPR2 eql EXPR1 | EXPR2.
   EXPR2 -> EXPR3 lt EXPR2 | EXPR3.
   EXPR3 -> EXPR4 add EXPR3 | EXPR4.
   EXPR4 -> EXPR5 mul EXPR4 | EXPR5.
   EXPR5 -> UNARY_EXPR | NUM_EXPR | STRING_EXPR | ARRAY_LITERAL | NULL_LITERAL | NEW_EXPR | YIELD_EXPR | CREATE_EXPR | RESUME_EXPR | CALLABLE_EXPR.
   UNARY_EXPR -> unop EXPR5.
   CALLABLE_EXPR -> CALL_EXPR | INDEX_EXPR | DOT_EXPR | VAR_EXPR | lp EXPR rp.
   CALL_EXPR -> CALLABLE_EXPR EXPR_LIST.
   EXPR_LIST -> lp EXPR_LIST1.
   EXPR_LIST1 -> EXPR coma EXPR_LIST | rp.
   INDEX_EXPR -> CALLABLE_EXPR ls EXPR rs.
   DOT_EXPR -> CALLABLE_EXPR dot ident.
   VAR_EXPR -> ident.
   NUM_EXPR -> number.
   STRING_EXPR -> string.
   ARRAY_LITERAL -> ls ARRAY_LITERAL1.
   ARRAY_LITERAL1 -> EXPR coma ARRAY_LITERAL1 | rs.
   NULL_LITERAL -> null.
   NEW_EXPR -> new STRUCT_LITERAL.
   STRUCT_LITERAL -> lb STRUCT_LITERAL1.
   STRUCT_LITERAL1 -> FIELD_LITERAL coma STRUCT_LITERAL1 | rs.
   FIELD_LITERAL -> ident colon EXPR.
   YIELD_EXPR -> YIELD_VALUE | YIELD_VOID.
   YIELD_VALUE -> yield lp EXPR rp.
   YIELD_VOID -> yield.
   CREATE_EXPR -> create lp EXPR CREATE_ARGS.
   CREATE_ARGS -> coma EXPR CREATE_ARGS | rp.
   RESUME_EXPR -> RESUME_NO_PARAM | RESUME_WITH_PARAM.
   RESUME_NO_PARAM -> resume lp EXPR rp.
   RESUME_WITH_PARAM -> resume lp EXPR coma EXPR rp.
 *)

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
