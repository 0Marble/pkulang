type array_type = { elem : typ }
and struct_type = { name : string; decls : decl list }
and decl = { name : string; typ : typ; kind : decl_kind }
and decl_kind = Field | Method | LetStmt | InnerStruct
and fn_type = { args : typ list; ret : typ }
and co_type = { args : typ list; yield : typ }
and co_obj_type = { yield : typ }
and builtin_fn = { name : string; typecheck : typ list -> typ }

and typ =
  | IntType
  | VoidType
  | NullType
  | EmptyArrayType
  | StringType
  | StructType of struct_type
  | ArrayType of array_type
  | FnType of fn_type
  | CoType of co_type
  | CoObjType of co_obj_type
  | BuiltinFn of builtin_fn

type definition = Node of Ast.node | Builtin of string

let builtins = Hashtbl.create 64;;

Hashtbl.add builtins "int" IntType;;
Hashtbl.add builtins "void" VoidType;;
Hashtbl.add builtins "str" StringType;;

Hashtbl.add builtins "len"
  (BuiltinFn
     {
       name = "len";
       typecheck =
         (fun args ->
           match args with
           | [ ArrayType _ ] -> IntType
           | [ EmptyArrayType ] -> IntType
           | _ -> failwith "Error: len() function accepts only arrays");
     })
;;

Hashtbl.add builtins "print"
  (BuiltinFn { name = "print"; typecheck = (fun _ -> VoidType) })
;;

Hashtbl.add builtins "println"
  (BuiltinFn { name = "println"; typecheck = (fun _ -> VoidType) })
;;

()

let rec get_definition (node : Ast.node) : definition =
  let node_name (node : Ast.node) : string option =
    match node with
    | Invalid -> failwith "unreachable: Invalid"
    | FnDecl x -> Some x.name
    | StructDecl x -> Some x.name
    | CoDecl x -> Some x.name
    | LetStmt x -> Some x.name
    | AliasStmt x -> Some x.name
    | Argument x -> Some x.name
    | Field x -> Some x.name
    | ForLoop x -> Some x.var
    | IfResumeStmt x -> x.var
    | _ -> None
  in
  let find_node_with_name name =
    List.find_opt (fun node ->
        match node_name node with Some n when n = name -> true | _ -> false)
  in
  let rec find_name_in_parents name node =
    match Ast.node_children node |> find_node_with_name name with
    | Some n -> Some n
    | None ->
        Option.map (find_name_in_parents name) (Ast.get_parent node)
        |> Option.join
  in

  match node with
  | NamedType x -> (
      match Hashtbl.find_opt builtins x.name with
      | Some _ -> Builtin x.name
      | None -> (
          match find_name_in_parents x.name (NamedType x) with
          | Some n -> Node n
          | None -> failwith "Error: undefined symbol"))
  | VarExpr x -> (
      match Hashtbl.find_opt builtins x.name with
      | Some _ -> Builtin x.name
      | None -> (
          match find_name_in_parents x.name (VarExpr x) with
          | Some n -> Node n
          | None -> failwith "Error: undefined symbol"))
  | DotType x -> (
      let parent = get_definition (Ast.type_to_node x.namespace) in
      match parent with
      | Node (StructDecl s) -> (
          match
            s.decls |> List.map Ast.decl_to_node |> find_node_with_name x.name
          with
          | Some (StructDecl d) -> Node (StructDecl d)
          | Some _ -> failwith "Error: child is not a type"
          | None -> failwith "Error: child type not found")
      | _ -> failwith "Error: Invalid parent, expected a struct type")
  | DotExpr x -> (
      let obj = get_definition (Ast.expr_to_node x.obj) in
      match obj with
      | Node (StructDecl s) -> (
          match
            s.decls |> List.map Ast.decl_to_node |> find_node_with_name x.field
          with
          | Some y -> Node y
          | None -> failwith "Error: no such declaration")
      | _ -> failwith "Error: parent is not an object")
  | YieldStmt _ | WhileLoop _ | ContinueStmt _ | BreakStmt _ | IfStmt _
  | ReturnStmt _ | Invalid | Root _ | Block _ ->
      failwith
        (Printf.sprintf "unreachable: no definition associated with `%s'"
           (Ast.node_to_str node))
  | _ -> Node node

and get_type (node : Ast.node) : typ =
  match node with
  | FnDecl x ->
      let ret = get_type (Ast.type_to_node x.ret) in
      let args =
        x.args
        |> List.map (fun (x : Ast.argument) ->
               x.typ |> Ast.type_to_node |> get_type)
      in
      FnType { ret; args }
  | CoDecl x ->
      let yield = get_type (Ast.type_to_node x.yield) in
      let args =
        x.args
        |> List.map (fun (x : Ast.argument) ->
               x.typ |> Ast.type_to_node |> get_type)
      in
      CoType { yield; args }
  | StructDecl x ->
      let decls =
        List.map
          (fun (d : Ast.decl) ->
            let typ = get_type (Ast.decl_to_node d) in
            match d with
            | FnDecl x -> { name = x.name; typ; kind = Method }
            | CoDecl x -> { name = x.name; typ; kind = Method }
            | LetStmt x -> { name = x.name; typ; kind = LetStmt }
            | StructDecl x -> { name = x.name; typ; kind = InnerStruct }
            | Field x -> { name = x.name; typ; kind = Field })
          x.decls
      in
      StructType { name = x.name; decls }
  | LetStmt x -> get_type (Ast.type_to_node x.typ)
  | AliasStmt x -> get_type (Ast.type_to_node x.typ)
  | Argument x -> get_type (Ast.type_to_node x.typ)
  | ArrayType x ->
      let elem = get_type (Ast.type_to_node x.elem) in
      ArrayType { elem }
  | FnType x ->
      let ret = get_type (Ast.type_to_node x.ret) in
      let args = List.map Ast.type_to_node x.args |> List.map get_type in
      FnType { ret; args }
  | CoType x ->
      let yield = get_type (Ast.type_to_node x.yield) in
      let args = List.map Ast.type_to_node x.args |> List.map get_type in
      CoType { yield; args }
  | CoObjType x ->
      let yield = get_type (Ast.type_to_node x.yield) in
      CoObjType { yield }
  | Field x -> get_type (Ast.type_to_node x.typ)
  | BinExpr x ->
      let lhs = get_type (Ast.expr_to_node x.lhs) in
      let rhs = get_type (Ast.expr_to_node x.rhs) in
      if lhs = rhs then lhs else failwith "Error: adding different types"
  | UnaryExpr x -> (
      let sub = get_type (Ast.expr_to_node x.sub_expr) in
      match sub with
      | IntType -> IntType
      | _ -> failwith "Error: expected an int")
  | CallExpr x -> (
      let fn = get_type (Ast.expr_to_node x.fn) in
      match fn with
      | FnType fn -> fn.ret
      | CoType co -> co.yield
      | BuiltinFn fn ->
          fn.typecheck
            (List.map (fun a -> Ast.expr_to_node a |> get_type) x.args)
      | _ -> failwith "Error: not a callable type")
  | IndexExpr x -> (
      let arr = get_type (Ast.expr_to_node x.arr) in
      match arr with
      | ArrayType arr -> arr.elem
      | _ -> failwith "Error: not an array type")
  | DotExpr x -> (
      let obj = get_type (Ast.expr_to_node x.obj) in
      match obj with
      | StructType s -> (
          match List.find_opt (fun (y : decl) -> y.name = x.field) s.decls with
          | Some y -> y.typ
          | None -> failwith "Error: no such field")
      | _ -> failwith "Error: not an object")
  | NumExpr _ -> IntType
  | StringExpr _ -> StringType
  | ArrayLiteral x -> (
      match x.elems with
      | y :: _ ->
          let elem = get_type (Ast.expr_to_node y) in
          ArrayType { elem }
      | _ -> EmptyArrayType)
  | NullLiteral _ -> NullType
  | NewExpr x -> get_type (Ast.type_to_node x.typ)
  | CreateExpr x -> (
      let coro = get_type (Ast.expr_to_node x.coroutine) in
      match coro with
      | CoType coro -> CoObjType { yield = coro.yield }
      | _ -> failwith "Error: creates takes a co fn as a first parameter")
  | ResumeExpr x -> (
      let coro = get_type (Ast.expr_to_node x.coroutine) in
      match coro with
      | CoObjType coro -> coro.yield
      | _ -> failwith "Error: resume takes a coroutine")
  | ForLoop x -> (
      let coro = get_type (Ast.expr_to_node x.iterator) in
      match coro with
      | CoObjType coro -> coro.yield
      | _ -> failwith "Error: for loop takes a coroutine object as an iterator")
  | IfResumeStmt x -> (
      let coro = get_type (Ast.expr_to_node x.coroutine) in
      match coro with
      | CoObjType coro -> coro.yield
      | _ -> failwith "Error: if resume should take a coroutine object")
  | DotType x -> (
      let ns = get_type (Ast.type_to_node x.namespace) in
      match ns with
      | StructType s -> (
          match List.find_opt (fun (y : decl) -> y.name = x.name) s.decls with
          | Some y when y.kind = InnerStruct -> y.typ
          | _ -> failwith "Error: no such inner type")
      | _ -> failwith "Error: not an object")
  | NamedType _ | VarExpr _ -> (
      match get_definition node with
      | Node n -> get_type n
      | Builtin b -> Hashtbl.find builtins b)
  | n ->
      failwith
        (Printf.sprintf "unreachable: no type associated with `%s'"
           (Ast.node_to_str n))
