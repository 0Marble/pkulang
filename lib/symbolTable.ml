[@@@ocaml.warning "-26-27"] (*Skips warning of unused variable*)

exception No_scope

type item =
  | Field of field
  | FnDecl of func_decl
  | StructDecl of struct_decl
  | CoDecl of co_decl
  | LetStmt of let_stmt
  | IfStmt of if_stmt
  | ReturnStmt of return_stmt
  | ContinueStmt of continue_stmt
  | BreakStmt of break_stmt
  | IfResumeStmt of if_resume_stmt
  | YieldStmt of yield_stmt
  | AliasStmt of alias_stmt
  | Block of block
  | Argument of argument
  | CallExpr of call_expr
  | DotExpr of dot_expr
  | VarExpr of var_expr
  | NumExpr of num_expr
  | StrExpr of str_expr
  | BinExpr of bin_expr
  | UnaryExpr of unary_expr
  | IndexExpr of index_expr
  | ArrayLiteral of array_literal
  | NullLiteral of null_literal
  | NewExpr of new_expr
  | CreateExpr of create_expr
  | ResumeExpr of resume_expr
  | ForLoop of for_loop
  | WhileLoop of while_loop
  | NamedType of named_type
  | ArrayType of array_type
  | FnType of fn_type
  | CoType of co_type
  | CoObjType of co_obj_type
  | DotType of dot_type
  | IntType
  | StrType
  | VoidType
  | NullType
  | Root of root

and struct_decl = {
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
  value : item option;
}

and argument = { name : string; node_idx : int; parent : int; typ : item }

and let_stmt = {
  name : string;
  node_idx : int;
  parent : int;
  typ : item;
  value : item;
}

and if_stmt = {
  node_idx : int;
  parent : int;
  if_true : item;
  if_false : item option;
}

and if_resume_stmt = {
  node_idx : int;
  parent : int;
  var : string option;
  if_ok : item;
  if_bad : item option;
  co : item;
}

and alias_stmt = {
  type_name : string;
  other_type : item;
  node_idx : int;
  parent : int;
}

and yield_stmt = { value : item option; node_idx : int; parent : int }
and continue_stmt = { node_idx : int; parent : int }
and break_stmt = { node_idx : int; parent : int }
and return_stmt = { node_idx : int; parent : int; value : item option }
and call_expr = { node_idx : int; parent : int; fn: item; params : item list }
and var_expr = { parent : int; node_idx : int; name : string }
and num_expr = { parent : int; node_idx : int; num : int }
and str_expr = { parent : int; node_idx : int; str : string }
and dot_expr = { parent : int; node_idx : int; obj : item; field : string }

and bin_expr = {
  parent : int;
  node_idx : int;
  left : item;
  right : item;
  op : Tokenizer.token;
}

and unary_expr = { parent : int; node_idx : int; op : Tokenizer.token; 
                   sub_expr : item }
and index_expr = { parent : int; node_idx : int; arr: item; idx : item list }
and array_literal = { parent : int; node_idx : int; children : item list }
and null_literal = { parent : int; node_idx : int }
and new_expr = { parent : int; node_idx : int; typ: item; 
                 fields : item list } 
and create_expr = { parent : int; node_idx : int; args : item list; co: item }
and resume_expr = { parent : int; node_idx : int; co: item }

and func_decl = {
  name : string;
  node_idx : int;
  parent : int;
  args : item list;
  return_type : item;
  body : item; (*kiv*)
}

and co_decl = {
  name : string;
  node_idx : int;
  parent : int;
  args : item list;
  yield_type : item;
  body : item;
}

and block = {
  (*Any generic block of code, e.g. inside if, for, while, function etc*)
  node_idx : int;
  parent : int;
  body : item list;
}

and for_loop = {
  node_idx : int;
  parent : int;
  iter_var : string;
  iterator : item;
  body : item;
}

and while_loop = { node_idx : int; parent : int; condition : item; body : item }
and root = { node_idx : int; children : item list }
and named_type = { node_idx : int; name : string; parent : int }
and array_type = { elem : item; node_idx : int }

and fn_type = {
  node_idx : int;
  args : item list;
  return_type : item;
  parent : int;
}

and co_type = { node_idx : int; args : item list; yield : item; parent : int }
and co_obj_type = { node_idx : int; yield : item }
and dot_type = { parent : int; child : string; node_idx : int; dt_parent: item }

type symbolTable = { root : item; items : (int, item) Hashtbl.t }

let get_children (scope : item) : item list =
  match scope with
  | StructDecl x -> x.children
  | FnDecl x -> x.args
  | CoDecl x -> x.args
  | Root x -> x.children
  | Block x -> x.body
  | WhileLoop x -> [ x.body ]
  | ForLoop x -> [ x.body ]
  | ArrayLiteral x -> x.children
  | CreateExpr x -> x.args
  | ResumeExpr x -> [ x.co ]
  | IfStmt x -> x.if_true :: Option.to_list x.if_false
  | IfResumeStmt x -> x.if_ok :: Option.to_list x.if_bad
  | ContinueStmt _ -> []
  | BreakStmt _ -> []
  | LetStmt _ -> []
  | ReturnStmt _ -> []
  | AliasStmt _ -> []
  | YieldStmt _ -> []
  | CallExpr x -> x.params
  | DotExpr _ -> []
  | VarExpr _ -> []
  | NumExpr _ -> []
  | StrExpr _ -> []
  | BinExpr _ -> []
  | UnaryExpr _ -> []
  | IndexExpr _ -> []
  | NullLiteral _ -> []
  | NewExpr _ -> []
  | Field _ -> []
  | Argument _ -> []
  | NamedType _ -> []
  | ArrayType _ -> []
  | FnType _ -> []
  | CoType _ -> []
  | CoObjType _ -> []
  | DotType _ -> []
  | IntType -> []
  | StrType -> []
  | VoidType -> []
  | NullType -> []

let get_parent (scope : item) (st : symbolTable) : item option =
  match scope with
  | Field x -> Hashtbl.find_opt st.items x.parent
  | StructDecl x -> Hashtbl.find_opt st.items x.parent
  | FnDecl x -> Hashtbl.find_opt st.items x.parent
  | CoDecl x -> Hashtbl.find_opt st.items x.parent
  | LetStmt x -> Hashtbl.find_opt st.items x.parent
  | IfStmt x -> Hashtbl.find_opt st.items x.parent
  | ReturnStmt x -> Hashtbl.find_opt st.items x.parent
  | ContinueStmt x -> Hashtbl.find_opt st.items x.parent
  | BreakStmt x -> Hashtbl.find_opt st.items x.parent
  | IfResumeStmt x -> Hashtbl.find_opt st.items x.parent
  | YieldStmt x -> Hashtbl.find_opt st.items x.parent
  | AliasStmt x -> Hashtbl.find_opt st.items x.parent
  | CallExpr x -> Hashtbl.find_opt st.items x.parent
  | DotExpr x -> Hashtbl.find_opt st.items x.parent
  | VarExpr x -> Hashtbl.find_opt st.items x.parent
  | NumExpr x -> Hashtbl.find_opt st.items x.parent
  | StrExpr x -> Hashtbl.find_opt st.items x.parent
  | BinExpr x -> Hashtbl.find_opt st.items x.parent
  | UnaryExpr x -> Hashtbl.find_opt st.items x.parent
  | IndexExpr x -> Hashtbl.find_opt st.items x.parent
  | ArrayLiteral x -> Hashtbl.find_opt st.items x.parent
  | NullLiteral x -> Hashtbl.find_opt st.items x.parent
  | NewExpr x -> Hashtbl.find_opt st.items x.parent
  | CreateExpr x -> Hashtbl.find_opt st.items x.parent
  | ResumeExpr x -> Hashtbl.find_opt st.items x.parent
  | Block x -> Hashtbl.find_opt st.items x.parent
  | ForLoop x -> Hashtbl.find_opt st.items x.parent
  | WhileLoop x -> Hashtbl.find_opt st.items x.parent
  | Argument x -> Hashtbl.find_opt st.items x.parent
  | NamedType x -> Hashtbl.find_opt st.items x.parent
  | ArrayType x -> Hashtbl.find_opt st.items x.node_idx
  | FnType x -> Hashtbl.find_opt st.items x.node_idx
  | CoType x -> Hashtbl.find_opt st.items x.node_idx
  | CoObjType _ -> None
  | DotType _ -> None
  | IntType -> None
  | StrType -> None
  | VoidType -> None
  | NullType -> None
  | Root _ -> None

let get_name (it : item) : string option =
  match it with
  | Field x -> Some x.name
  | StructDecl x -> Some x.name
  | FnDecl x -> Some x.name
  | CoDecl x -> Some x.name
  | LetStmt x -> Some x.name
  | Argument x -> Some x.name
  | NamedType x -> Some x.name
  | ArrayType x -> Some "array"
  | FnType x -> Some "function"
  | CoType x -> Some "coroutine"
  | CoObjType x -> Some "coroutine object"
  | DotType x -> failwith "TODO" (*parent.child???*)
  | IntType -> Some "int"
  | StrType -> Some "string"
  | VoidType -> Some "void"
  | NullType -> Some "null"
  | IfStmt _ -> None
  | ReturnStmt _ -> None
  | ContinueStmt _ -> None
  | BreakStmt _ -> None
  | AliasStmt _ -> None
  | YieldStmt _ -> None
  | IfResumeStmt _ -> None
  | VarExpr x -> Some x.name
  | CallExpr _ -> None
  | DotExpr _ -> None
  | NumExpr _ -> None
  | StrExpr _ -> None
  | BinExpr _ -> None
  | UnaryExpr _ -> None
  | IndexExpr _ -> None
  | ArrayLiteral _ -> None
  | NullLiteral _ -> None
  | NewExpr _ -> None
  | CreateExpr _ -> None
  | ResumeExpr _ -> None
  | Block _ -> None
  | ForLoop _ -> None
  | WhileLoop _ -> None
  | Root _ -> None

let get_node_idx (it : item) : int =
  match it with
  | Field x -> x.node_idx
  | StructDecl x -> x.node_idx
  | FnDecl x -> x.node_idx
  | CoDecl x -> x.node_idx
  | LetStmt x -> x.node_idx
  | Argument x -> x.node_idx
  | NamedType x -> x.node_idx
  | ArrayType x -> x.node_idx
  | FnType x -> x.node_idx
  | CoType x -> x.node_idx
  | CoObjType x -> x.node_idx
  | DotType x -> x.node_idx
  | IntType -> -1
  | StrType -> -1
  | VoidType -> -1
  | NullType -> -1
  | IfStmt x -> x.node_idx
  | ReturnStmt x -> x.node_idx
  | ContinueStmt x -> x.node_idx
  | BreakStmt x -> x.node_idx
  | AliasStmt x -> x.node_idx
  | YieldStmt x -> x.node_idx
  | IfResumeStmt x -> x.node_idx
  | VarExpr x -> x.node_idx
  | CallExpr e -> e.node_idx
  | DotExpr e -> e.node_idx
  | NumExpr e -> e.node_idx
  | StrExpr e -> e.node_idx
  | BinExpr e -> e.node_idx
  | UnaryExpr e -> e.node_idx
  | IndexExpr e -> e.node_idx
  | ArrayLiteral e -> e.node_idx
  | NullLiteral e -> e.node_idx
  | NewExpr e -> e.node_idx
  | CreateExpr e -> e.node_idx
  | ResumeExpr e -> e.node_idx
  | Block b -> b.node_idx
  | ForLoop f -> f.node_idx
  | WhileLoop w -> w.node_idx
  | Root r -> r.node_idx

let rec find_by_name (name : string) (scope : item) (st : symbolTable) :
    item option =
  let children = get_children scope in
  match
    List.find_opt
      (fun (child : item) -> get_name child = Some name)
      children (*match name with child.name*)
  with
  | Some child -> Some child (*found -> return child*)
  | None -> (
      match get_parent scope st with
      | Some parent ->
          find_by_name name parent st (*recursively search in parent scope*)
      | None -> None)

(*
  input: item & symbolTable
  output: item
  usage: get the item object that defines the current item variable
  e.g.:
  Struct Foo {};
  let foo = Foo {};
  ...
  x = foo+ 10;
  get_def(foo) -> let foo = Foo {};
   *)

let rec get_definition (it : item) (st : symbolTable) : item option =
  match it with
  | Field _ | FnDecl _ | StructDecl _ | CoDecl _ | LetStmt _ | AliasStmt _
  | Argument _ | Block _ | ForLoop _ | WhileLoop _ | IfStmt _ | IfResumeStmt _
  | ContinueStmt _ | BreakStmt _ | ReturnStmt _ | YieldStmt _ | NumExpr _
  | StrExpr _ | ArrayType _ | FnType _ | CoType _ | CoObjType _ | Root _ ->
      Some it (*these items define smth so they are their own def?*)
  | VarExpr x -> (
      match Hashtbl.find_opt st.items x.parent with
      | Some parent_scope -> (
          match find_by_name x.name parent_scope st with
          | Some def_item -> Some def_item
          | None -> failwith "Definition not found")
      | None -> failwith "VarExpr has no parent scope in symbol table.")
  | NamedType x -> (
      match x.name with
      | "int" -> Some IntType
      | "str" -> Some StrType
      | "void" -> Some VoidType
      | "null" -> Some NullType
      | _ -> (
          match find_by_name x.name st.root st with
          | Some def_item -> (
              match def_item with
              | StructDecl _ -> Some def_item
              | _ -> failwith "Error: Not supported named type definition")
          | _ -> failwith "Error: Definition not found"))
  | DotType x -> (
      let parent =
        match get_parent it st with
        | Some parent -> parent
        | _ -> failwith "Error: DotType parent not found in symbol table"
      in
      let parent_type_definition = get_definition parent st in
      match parent_type_definition with
      | Some (StructDecl s_decl) -> (
          match find_by_name x.child (StructDecl s_decl) st with
          | Some def_item -> Some def_item
          | None -> failwith "Error: child not found in struct")
      | _ -> failwith "Parent does not resolve to a struct or alias type.")
  | DotExpr x -> (
      let obj_def = get_definition x.obj st in
      match obj_def with
      | Some (NamedType nt) -> (
          match find_by_name x.field (NamedType nt) st with
          | Some field_item -> Some field_item
          | None -> failwith "Error: child field not found within object")
      | _ -> failwith "object not a named type")
  | _ -> failwith "TODO"

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
        | "string" -> StrType
        | "void" -> VoidType
        | "null" -> NullType
        | _ ->
            let (nt : named_type) =
              { name = x.name; node_idx = x.node_idx; parent = current_scope }
            in
            Hashtbl.add st x.node_idx (NamedType nt);
            NamedType nt)
    | ArrayType x ->
        let at =
          { elem = scan_type x.elem current_scope; node_idx = x.node_idx }
        in
        Hashtbl.add st x.node_idx (ArrayType at);
        ArrayType at
    | DotType x ->
        let dt =
          { parent = current_scope; dt_parent = scan_type x.parent current_scope; child = x.child; node_idx = x.node_idx }
        in
        Hashtbl.add st x.node_idx (DotType dt);
        DotType dt
    | FnType x ->
        let ft =
          {
            parent = current_scope;
            node_idx = x.node_idx;
            args = List.map (fun a -> scan_type a current_scope) x.args;
            return_type = scan_type x.ret current_scope;
          }
        in
        Hashtbl.add st x.node_idx (FnType ft);
        FnType ft
    | CoType x ->
        let ct =
          {
            parent = current_scope;
            node_idx = x.node_idx;
            args = List.map (fun a -> scan_type a current_scope) x.args;
            yield = scan_type x.yield current_scope;
          }
        in
        Hashtbl.add st x.node_idx (CoType ct);
        CoType ct
    | CoObjType x ->
        let cot =
          { node_idx = x.node_idx; yield = scan_type x.yield current_scope }
        in
        Hashtbl.add st x.node_idx (CoObjType cot);
        CoObjType cot
  and scan_expr (e : Ast.expr) (current_scope : int) =
    match e with
    | CallExpr x ->
        let (e : call_expr) =
          { parent = current_scope; node_idx = x.node_idx; 
            params = List.map (fun p -> scan_expr p current_scope) x.params;
            fn = scan_expr x.fn current_scope;
          }
        in
        Hashtbl.add st x.node_idx (CallExpr e);
        CallExpr e
    | DotExpr x ->
        let (e : dot_expr) =
          {
            parent = current_scope;
            node_idx = x.node_idx;
            obj = scan_expr x.obj current_scope;
            field = x.field;
          }
        in
        Hashtbl.add st x.node_idx (DotExpr e);
        DotExpr e
    | VarExpr x ->
        let (e : var_expr) =
          { parent = current_scope; node_idx = x.node_idx; name = x.name }
        in
        Hashtbl.add st x.node_idx (VarExpr e);
        VarExpr e
    | NumExpr x ->
        let (e : num_expr) =
          { parent = current_scope; node_idx = x.node_idx; num = x.num }
        in
        Hashtbl.add st x.node_idx (NumExpr e);
        NumExpr e
    | StringExpr x ->
        let (e : str_expr) =
          { parent = current_scope; node_idx = x.node_idx; str = x.str }
        in
        Hashtbl.add st x.node_idx (StrExpr e);
        StrExpr e
    | BinExpr x ->
        let (e : bin_expr) =
          {
            parent = current_scope;
            node_idx = x.node_idx;
            left = scan_expr x.lhs current_scope;
            right = scan_expr x.rhs current_scope;
            op = x.op;
          }
        in
        Hashtbl.add st x.node_idx (BinExpr e);
        BinExpr e
    | UnaryExpr x -> 
        let (e : unary_expr) =
          {
            parent = current_scope;
            node_idx = x.node_idx;
            op = x.op;
            sub_expr = scan_expr x.sub_expr current_scope;
          }
        in
        Hashtbl.add st x.node_idx (UnaryExpr e);
        UnaryExpr e
    | IndexExpr x -> 
        let (e : index_expr) =
          { parent = current_scope; node_idx = x.node_idx; arr = scan_expr x.arr current_scope; idx = List.map (fun i -> scan_expr i current_scope) x.idx }
          
        in
        Hashtbl.add st x.node_idx (IndexExpr e);
        IndexExpr e
    | ArrayLiteral x -> 
        let children = List.map (fun e -> scan_expr e current_scope) x.elems in
        let (e : array_literal) =
          { parent = current_scope; node_idx = x.node_idx; children }
        in
        Hashtbl.add st x.node_idx (ArrayLiteral e);
        ArrayLiteral e
    | NullLiteral x -> 
        let (e : null_literal) =
          { parent = current_scope; node_idx = x.node_idx }
        in
        Hashtbl.add st x.node_idx (NullLiteral e);
        NullLiteral e
    | NewExpr x -> 
        let (e : new_expr) =
          { parent = current_scope; node_idx = x.node_idx; 
            typ = scan_type x.typ current_scope;
            fields = List.map (fun (f : Ast.field_literal) -> scan_expr f.value current_scope) x.fields;
          }
        in
        Hashtbl.add st x.node_idx (NewExpr e);
        NewExpr e
    | CreateExpr x -> 
        let args = List.map (fun e -> scan_expr e current_scope) x.params in
        let (e : create_expr) =
          { parent = current_scope; node_idx = x.node_idx; args = args; co = scan_expr x.coroutine current_scope }
        in
        Hashtbl.add st x.node_idx (CreateExpr e);
        CreateExpr e
    | ResumeExpr x -> 
        let (e : resume_expr) =
          {
            parent = current_scope;
            node_idx = x.node_idx;
            co = scan_expr x.coroutine current_scope;
          }
        in
        Hashtbl.add st x.node_idx (ResumeExpr e);
        ResumeExpr e
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
            value = scan_expr x.value current_scope;
          }
        in
        Hashtbl.add st x.node_idx (LetStmt ls);
        LetStmt ls
    | ContinueStmt x ->
        let (cs : continue_stmt) =
          { node_idx = x.node_idx; parent = current_scope }
        in
        Hashtbl.add st x.node_idx (ContinueStmt cs);
        ContinueStmt cs
    | BreakStmt x ->
        let (bs : break_stmt) =
          { node_idx = x.node_idx; parent = current_scope }
        in
        Hashtbl.add st x.node_idx (BreakStmt bs);
        BreakStmt bs
    | ReturnStmt x ->
        let (rs : return_stmt) =
          { node_idx = x.node_idx; parent = current_scope; 
            value = Option.map (fun v -> scan_expr v current_scope) x.value }
        in
        Hashtbl.add st x.node_idx (ReturnStmt rs);
        ReturnStmt rs
    | IfStmt x ->
        let (s : if_stmt) =
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
        Hashtbl.add st x.node_idx (IfStmt s);
        IfStmt s
    | AliasStmt x ->
        let (s : alias_stmt) =
          {
            type_name = x.type_name;
            other_type = scan_type x.other_type current_scope;
            node_idx = x.node_idx;
            parent = current_scope;
          }
        in
        Hashtbl.add st x.node_idx (AliasStmt s);
        AliasStmt s
    | IfResumeStmt x ->
        let (s : if_resume_stmt) =
          {
            var = x.var;
            if_ok = scan_stmt x.if_ok x.node_idx;
            if_bad =
              Option.map (fun if_bad -> scan_stmt if_bad x.node_idx) x.if_bad;
            co = scan_expr x.coroutine current_scope;
            node_idx = x.node_idx;
            parent = current_scope;
          }
        in
        Hashtbl.add st x.node_idx (IfResumeStmt s);
        IfResumeStmt s
    | YieldStmt x ->
        let (s : yield_stmt) =
          {
            value = Option.map (fun v -> scan_expr v current_scope) x.value;
            node_idx = x.node_idx;
            parent = current_scope;
          }
        in
        Hashtbl.add st x.node_idx (YieldStmt s);
        YieldStmt s
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
    | CoDecl x ->
        let args = List.map (fun arg -> scan_args arg x.node_idx) x.args in
        let body = scan_stmt x.body x.node_idx in
        let (c : co_decl) =
          {
            name = x.name;
            node_idx = x.node_idx;
            parent = current_scope;
            args;
            yield_type = scan_type x.yield_type current_scope;
            body;
          }
        in
        Hashtbl.add st x.node_idx (CoDecl c);
        CoDecl c
    | StructDecl x ->
        let children = List.map (fun d -> scan_decl d x.node_idx) x.decls in
        let (sd : struct_decl) =
          {
            name = x.name;
            children;
            parent = current_scope;
            node_idx = x.node_idx;
          }
        in
        Hashtbl.add st x.node_idx (StructDecl sd);
        StructDecl sd
    | ForLoop x ->
        let body = scan_stmt x.body x.node_idx in
        let (fl : for_loop) =
          {
            node_idx = x.node_idx;
            parent = current_scope;
            iter_var = x.iter_var;
            iterator = scan_expr x.iterator current_scope;
            body;
          }
        in
        Hashtbl.add st x.node_idx (ForLoop fl);
        ForLoop fl
    | WhileLoop x ->
        let body = scan_stmt x.body x.node_idx in
        let (wl : while_loop) =
          {
            node_idx = x.node_idx;
            parent = current_scope;
            condition = scan_expr x.condition current_scope;
            body;
          }
        in
        Hashtbl.add st x.node_idx (WhileLoop wl);
        WhileLoop wl
    | Expr x -> scan_expr x current_scope
  and scan_decl (decl : Ast.decl) (current_scope : int) = 
    match decl with
      | CoDecl d -> scan_stmt (CoDecl d) current_scope
      | LetStmt d -> scan_stmt (LetStmt d) current_scope
      | StructDecl d -> scan_stmt (StructDecl d) current_scope
      | FnDecl d -> scan_stmt (FnDecl d) current_scope
      | Field f -> 
          let (f : field) =
            {
              name = f.var_name;
              node_idx = f.node_idx;
              parent = current_scope;
              typ = scan_type f.field_type current_scope;
              value = Option.map (fun v -> scan_expr v current_scope) f.value;
            }
          in
          Hashtbl.add st f.node_idx (Field f);
          Field f
        in
  let r =
    Root
      {
        node_idx = root.node_idx;
        children =
          List.map
            (fun c -> scan_stmt (Ast.top_stmt_to_stmt c) root.node_idx)
            root.stmts;
      }
  in
  Hashtbl.add st root.node_idx r;
  { root = r; items = st }
