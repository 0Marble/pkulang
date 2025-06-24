type array_type = { elem : typ }
and struct_type = Ast.struct_decl
and fn_type = { args : typ list; ret : typ }
and co_type = { args : typ list; yield : typ }
and co_obj_type = { yield : typ }
and builtin_fn = { name : string; typecheck : symbol_store -> typ list -> typ }

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

and symbol_store = {
  defs : (Ast.node, definition) Hashtbl.t;
  types : (Ast.node, typ) Hashtbl.t;
  src : string;
}

and definition = Node of Ast.node | Builtin of string

let rec string_of_type t =
  match t with
  | IntType -> "(int)"
  | VoidType -> "(void)"
  | NullType -> "(null)"
  | EmptyArrayType -> "(empty_array)"
  | StringType -> "(str)"
  | StructType x -> Ast.node_to_str (StructDecl x)
  | ArrayType x -> Printf.sprintf "(array %s)" (string_of_type x.elem)
  | FnType x ->
      Printf.sprintf "(fn (%s) %s)"
        (List.map string_of_type x.args |> String.concat " ")
        (string_of_type x.ret)
  | CoType x ->
      Printf.sprintf "(co (%s) %s)"
        (List.map string_of_type x.args |> String.concat " ")
        (string_of_type x.yield)
  | CoObjType x -> Printf.sprintf "(co_obj %s)" (string_of_type x.yield)
  | BuiltinFn x -> Printf.sprintf "(builtin %s)" x.name

let string_of_definition d =
  match d with
  | Node n -> Printf.sprintf "(node %s)" (Ast.node_to_str n)
  | Builtin s -> Printf.sprintf "(builtin %s)" s

let builtins = Hashtbl.create 64;;

Hashtbl.add builtins "int" IntType;;
Hashtbl.add builtins "void" VoidType;;
Hashtbl.add builtins "str" StringType;;

Hashtbl.add builtins "len"
  (BuiltinFn
     {
       name = "len";
       typecheck =
         (fun _ args ->
           match args with
           | [ ArrayType _ ] -> IntType
           | [ EmptyArrayType ] -> IntType
           | _ -> failwith "Error: len() function accepts only arrays");
     })
;;

Hashtbl.add builtins "print"
  (BuiltinFn { name = "print"; typecheck = (fun _ _ -> VoidType) })
;;

Hashtbl.add builtins "println"
  (BuiltinFn { name = "println"; typecheck = (fun _ _ -> VoidType) })
;;

Hashtbl.add builtins "read_line"
  (BuiltinFn
     {
       name = "read_line";
       typecheck =
         (fun _ args ->
           if List.length args <> 0 then
             failwith "Error: read_line() does not take arguments"
           else StringType);
     })
;;

Hashtbl.add builtins "push"
  (BuiltinFn
     {
       name = "push";
       typecheck =
         (fun _ args ->
           match args with
           | [ ArrayType arr; elem ] when arr.elem = elem -> VoidType
           | _ ->
               failwith
                 "Error: push() takes an array of type [T] and elem of type T");
     })
;;

()

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

let find_node_with_name name =
  List.find_opt (fun node ->
      match node_name node with Some n when n = name -> true | _ -> false)

let rec find_name_in_parents name node =
  match Ast.node_children node |> find_node_with_name name with
  | Some n -> Some n
  | None ->
      Option.map (find_name_in_parents name) (Ast.node_parent node)
      |> Option.join

let create src : symbol_store =
  { defs = Hashtbl.create 64; types = Hashtbl.create 64; src }

let rec get_definition ss (node : Ast.node) : definition =
  match Hashtbl.find_opt ss.defs node with
  | Some old -> old
  | None -> (
      let get_definition = get_definition ss in
      let get_type = get_type ss in
      try
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
                  s.decls |> List.map Ast.decl_to_node
                  |> find_node_with_name x.name
                with
                | Some (StructDecl d) -> Node (StructDecl d)
                | Some _ -> failwith "Error: child is not a type"
                | None -> failwith "Error: child type not found")
            | _ -> failwith "Error: Invalid parent, expected a struct type")
        | DotExpr x -> (
            let obj = get_type (Ast.expr_to_node x.obj) in
            match obj with
            | StructType s -> (
                match
                  s.decls |> List.map Ast.decl_to_node
                  |> find_node_with_name x.field
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
      with err ->
        Error.fail_at_spot "symbol_store error" ss.src (Ast.node_loc node) err)

and get_type (ss : symbol_store) (node : Ast.node) : typ =
  match Hashtbl.find_opt ss.types node with
  | Some old -> old
  | None -> (
      let get_definition = get_definition ss in
      let get_type = get_type ss in
      try
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
        | StructDecl x -> StructType x
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
                fn.typecheck ss
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
                match
                  s.decls |> List.map Ast.decl_to_node
                  |> find_node_with_name x.field
                with
                | Some y -> get_type y
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
            | _ ->
                failwith
                  "Error: for loop takes a coroutine object as an iterator")
        | IfResumeStmt x -> (
            let coro = get_type (Ast.expr_to_node x.coroutine) in
            match coro with
            | CoObjType coro -> coro.yield
            | _ -> failwith "Error: if resume should take a coroutine object")
        | DotType x -> (
            let ns = get_type (Ast.type_to_node x.namespace) in
            match ns with
            | StructType s -> (
                match
                  s.decls |> List.map Ast.decl_to_node
                  |> find_node_with_name x.name
                with
                | Some (StructDecl y) -> StructType y
                | _ -> failwith "Error: no such inner type")
            | _ -> failwith "Error: not an object")
        | NamedType _ | VarExpr _ -> (
            match get_definition node with
            | Node n -> get_type n
            | Builtin b -> (
                match Hashtbl.find_opt builtins b with
                | Some b -> b
                | None ->
                    failwith
                      (Printf.sprintf
                         "unreachable: somehow got an invalid builtin `%s'" b)))
        | n ->
            failwith
              (Printf.sprintf "unreachable: no type associated with `%s'"
                 (Ast.node_to_str n))
      with err ->
        Error.fail_at_spot "symbol_store error" ss.src (Ast.node_loc node) err)

let def_eql (a : definition) (b : definition) =
  match (a, b) with
  | Node a, Node b -> Ast.node_eql a b
  | Builtin a, Builtin b -> a = b
  | _ -> false

let rec typ_eql a b =
  match (a, b) with
  | IntType, IntType -> true
  | VoidType, VoidType -> true
  | NullType, NullType -> true
  | EmptyArrayType, EmptyArrayType -> true
  | StringType, StringType -> true
  | StructType a, StructType b -> Ast.node_eql (StructDecl a) (StructDecl b)
  | ArrayType a, ArrayType b -> typ_eql a.elem b.elem
  | FnType a, FnType b ->
      typ_eql a.ret b.ret && List.for_all2 typ_eql a.args b.args
  | CoType a, CoType b ->
      typ_eql a.yield b.yield && List.for_all2 typ_eql a.args b.args
  | CoObjType a, CoObjType b -> typ_eql a.yield b.yield
  | BuiltinFn a, BuiltinFn b -> a == b
  | _ -> false

let compatible_types expected given =
  match (expected, given) with
  | IntType, NullType -> false
  | _, NullType -> true
  | ArrayType _, EmptyArrayType -> true
  | a, b -> typ_eql a b

let can_be_coro typ =
  match typ with CoObjType _ -> true | NullType -> true | _ -> false
