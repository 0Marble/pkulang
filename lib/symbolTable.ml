exception No_scope

(* type scope = {
  scope_typ : scope_typ;
  node_idx : int; (*node id of scope object in AST*)
  parent: scope; (*Global doesnt hv a parent so how?*)
  children: scope list option;
  items: item list option;
  (*How do I store the object of the scope itself? e.g. function has params, struct has fields*)
} *)

type item =
  | Field of field
  | Struct of struct_type
  | FnDecl of func_decl
  | LetStmt of let_stmt
  | IfStmt of if_stmt
  | ReturnStmt of return_stmt
  | Block of block
  | Argument of argument
  | CallExpr of call_expr
  | IntType
  | StrType
  | VoidType
  | NullType
  | Root of root

and struct_type = {
  name : string;
  node_idx : int; (*node index in AST*)
  parent : int;
  children : item list; (*Store both scopes and variables*)
}

and field = {
  name : string;
  node_idx : int;
  parent : int;
  typ : item;
  value : Ast.expr option; (*Default value of field*)
}

and argument = { name : string; node_idx : int; parent : int; typ : item }

and let_stmt = {
  name : string;
  node_idx : int;
  parent : int;
  typ : item;
  value : Ast.expr; (*Default value of field*)
}

and if_stmt = {
  node_idx : int;
  parent : int;
  if_true : item;
  if_false : item option;
}

and return_stmt = { node_idx : int; parent : int }
and call_expr = { node_idx : int; parent : int }

and func_decl = {
  name : string;
  node_idx : int;
  parent : int;
  args : item list;
  return_type : item;
  body : item; (*kiv*)
}

and block = {
  (*Any generic block of code, e.g. inside if, for, while, function etc*)
  node_idx : int;
  parent : int; (*node index of parent*)
  body : item list;
}

and root = { node_idx : int; children : item list }

type symbolTable = { root : item; items : (int, item) Hashtbl.t }

let get_children (scope : item) : item list =
  match scope with
  | Struct x -> x.children
  | FnDecl x -> x.args
  | Block x -> x.body
  | Root x -> x.children
  | IfStmt x -> x.if_true :: Option.to_list x.if_false
  | LetStmt _ -> []
  | ReturnStmt _ -> []
  | CallExpr _ -> []
  | Field _ -> []
  | Argument _ -> []
  | IntType -> []
  | StrType -> []
  | VoidType -> []
  | NullType -> []

let get_parent (scope : item) (st : symbolTable) : item option =
  match scope with
  | Field x -> Hashtbl.find_opt st.items x.parent
  | Struct x -> Hashtbl.find_opt st.items x.parent
  | FnDecl x -> Hashtbl.find_opt st.items x.parent
  | LetStmt x -> Hashtbl.find_opt st.items x.parent
  | IfStmt x -> Hashtbl.find_opt st.items x.parent
  | ReturnStmt x -> Hashtbl.find_opt st.items x.parent
  | CallExpr x -> Hashtbl.find_opt st.items x.parent
  | Block x -> Hashtbl.find_opt st.items x.parent
  | Argument x -> Hashtbl.find_opt st.items x.parent
  | IntType -> None
  | StrType -> None
  | VoidType -> None
  | NullType -> None
  | Root _ -> None

let get_name (it : item) : string option =
  match it with
  | Field x -> Some x.name
  | Struct x -> Some x.name
  | FnDecl x -> Some x.name
  | LetStmt x -> Some x.name
  | Argument x -> Some x.name
  | IntType -> Some "int"
  | StrType -> Some "string"
  | VoidType -> Some "void"
  | NullType -> Some "null"
  | IfStmt _ -> None
  | ReturnStmt _ -> None
  | CallExpr _ -> None
  | Block _ -> None
  | Root _ -> None

let rec find_by_name (name : string) (scope : item) (st : symbolTable) :
    item option =
  let children = get_children scope in
  match
    List.find_opt (fun (child : item) -> get_name child = Some name) children
  with
  | Some child -> Some child
  | None -> (
      match get_parent scope st with
      | Some parent -> find_by_name name parent st
      | None -> None)

let build_symbol_table (root : Ast.root) : symbolTable =
  let st = Hashtbl.create 64 in
  (*create variable for symbol table*)
  let rec scan_args (arg : Ast.argument) (current_scope : int) =
    let a =
      {
        name = arg.name;
        node_idx = arg.node_idx;
        parent = current_scope;
        typ = scan_type arg.arg_type current_scope;
      }
    in
    Hashtbl.add st arg.node_idx (Argument a);
    Argument a
  and scan_type (t : Ast.typ) (current_scope : int) =
    match t with
    | NamedType x -> (
        match x.name with
        | "int" -> IntType
        | "void" -> NullType
        | _ -> failwith "TODO")
    | ArrayType x -> failwith "TODO"
    | DotType x -> failwith "TODO"
    | FnType x -> failwith "TODO"
    | CoType x -> failwith "TODO"
    | CoObjType x -> failwith "TODO"
  and scan_expr (e : Ast.expr) (current_scope : int) =
    match e with
    | BinExpr x -> failwith "TODO"
    | UnaryExpr x -> failwith "TODO"
    | CallExpr x ->
        let (e : call_expr) =
          { node_idx = x.node_idx; parent = current_scope }
        in
        Hashtbl.add st x.node_idx (CallExpr e);
        CallExpr e
    | IndexExpr x -> failwith "TODO"
    | DotExpr x -> failwith "TODO"
    | VarExpr x -> failwith "TODO"
    | NumExpr x -> failwith "TODO"
    | StringExpr x -> failwith "TODO"
    | ArrayLiteral x -> failwith "TODO"
    | NullLiteral x -> failwith "TODO"
    | NewExpr x -> failwith "TODO"
    | CreateExpr x -> failwith "TODO"
    | ResumeExpr x -> failwith "TODO"
  and scan_stmt (stmt : Ast.stmt) (current_scope : int) =
    match stmt with
    | Block x ->
        let children =
          List.map (fun stmt -> scan_stmt stmt x.node_idx) x.stmts
        in
        let (b : block) =
          { node_idx = x.node_idx; parent = current_scope; body = children }
        in
        Hashtbl.add st x.node_idx (Block b);
        Block b
    | LetStmt x ->
        let (ls : let_stmt) =
          {
            name = x.var_name;
            node_idx = x.node_idx;
            parent = current_scope;
            typ = scan_type x.var_type current_scope;
            value = x.value;
          }
        in
        Hashtbl.add st x.node_idx (LetStmt ls);
        LetStmt ls
    | ForLoop x -> failwith "TODO"
    | WhileLoop x -> failwith "TODO"
    | ContinueStmt x -> failwith "TODO"
    | BreakStmt x -> failwith "TODO"
    | IfStmt x ->
        let (is : if_stmt) =
          {
            node_idx = x.node_idx;
            parent = current_scope;
            if_true = scan_stmt x.if_true x.node_idx;
            if_false =
              Option.map
                (fun if_false -> scan_stmt if_false x.node_idx)
                x.if_false;
          }
        in
        Hashtbl.add st x.node_idx (IfStmt is);
        IfStmt is
    | ReturnStmt x ->
        let (rs : return_stmt) =
          { node_idx = x.node_idx; parent = current_scope }
        in
        Hashtbl.add st x.node_idx (ReturnStmt rs);
        ReturnStmt rs
    | Expr x -> scan_expr x current_scope
    | FnDecl x ->
        let args = List.map (fun arg -> scan_args arg x.node_idx) x.args in
        let body = scan_stmt x.body x.node_idx in
        let (fn : func_decl) =
          {
            name = x.name;
            node_idx = x.node_idx;
            parent = current_scope;
            args;
            return_type = scan_type x.ret_type current_scope;
            body;
          }
        in
        Hashtbl.add st x.node_idx (FnDecl fn);
        FnDecl fn
    | StructDecl x -> failwith "TODO"
    | CoDecl x -> failwith "TODO"
    | AliasStmt x -> failwith "TODO"
    | IfResumeStmt x -> failwith "TODO"
    | YieldStmt x -> failwith "TODO"
  in
  failwith "TODO"
