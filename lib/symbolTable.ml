exception No_scope

type scope_kind =
  | GlobalScope
  | BlockScope
  | FunctionScope of string
  | StructScope of string

type symbol_kind = Variable | Function | Struct

type symbol_info = {
  name : string;
  kind : symbol_kind;
  node_idx : int;
  ty : Ast.typ option;
  declared_in : scope_kind;
  loc : Location.location option;
}

type scope = {
  kind : scope_kind;
  symbols : (string, symbol_info) Hashtbl.t;
  parent : scope option;
  children : scope list ref;
}

type t = {
  root : scope;
  mutable current : scope;
  by_node_idx : (int, symbol_info) Hashtbl.t;
}

let create_scope ?parent kind : scope =
  { kind; symbols = Hashtbl.create 16; parent; children = ref [] }

let root_scope (t : t) : scope = t.root

let create () =
  let global_scope =
    {
      kind = GlobalScope;
      symbols = Hashtbl.create 32;
      parent = None;
      children = ref [];
    }
  in
  {
    root = global_scope;
    current = global_scope;
    by_node_idx = Hashtbl.create 64;
  }

let enter_scope symtab kind =
  let new_scope =
    {
      kind;
      symbols = Hashtbl.create 16;
      parent = Some symtab.current;
      children = ref [];
    }
  in
  symtab.current.children := new_scope :: !(symtab.current.children);
  symtab.current <- new_scope;
  symtab (* <-- return the updated symbol table *)

let exit_scope symtab =
  match symtab.current.parent with
  | Some parent ->
      symtab.current <- parent;
      symtab (* <-- return the updated symbol table *)
  | None -> raise (Invalid_argument "symbolTable: Cannot exit global scope")

let add_symbol symtab (sym : symbol_info) =
  if Hashtbl.mem symtab.current.symbols sym.name then
    failwith ("Symbol " ^ sym.name ^ " already declared in current scope")
  else (
    Hashtbl.add symtab.current.symbols sym.name sym;
    Hashtbl.add symtab.by_node_idx sym.node_idx sym)

let add_symbol_to_scope scope sym symtab =
  if Hashtbl.mem scope.symbols sym.name then
    failwith ("Symbol " ^ sym.name ^ " already declared in scope")
  else (
    Hashtbl.add scope.symbols sym.name sym;
    Hashtbl.add symtab.by_node_idx sym.node_idx sym)

let rec find_symbol_in_scope scope name =
  match Hashtbl.find_opt scope.symbols name with
  | Some sym -> Some sym
  | None -> (
      match scope.parent with
      | Some parent -> find_symbol_in_scope parent name
      | None -> None)

let find_symbol symtab name = find_symbol_in_scope symtab.current name

let find_by_node_idx (symtab : t) (idx : int) : symbol_info option =
  Hashtbl.find_opt symtab.by_node_idx idx

let find_symbol_exn t name =
  match find_symbol t name with
  | Some s -> s
  | None -> invalid_arg ("symbolTable: " ^ name ^ " not bound")

let find_by_node_idx_exn t idx : symbol_info =
  match find_by_node_idx t idx with
  | Some s -> s
  | None ->
      invalid_arg ("symbolTable: node_idx " ^ string_of_int idx ^ " not bound")

let find_in_scope (s : scope) (name : string) : symbol_info option =
  find_symbol_in_scope s name

let current_scope_kind t : scope_kind = t.current.kind

let rec fold_scope scope ~init ~f =
  let acc = Hashtbl.fold (fun _ sym acc -> f sym acc) scope.symbols init in
  List.fold_left
    (fun acc child -> fold_scope child ~init:acc ~f)
    acc !(scope.children)

let fold_symbols t ~init ~f = fold_scope t.root ~init ~f

let all_symbols t : symbol_info list =
  Hashtbl.fold (fun _ sym acc -> sym :: acc) t.by_node_idx []

let rec collect_scopes_by_kind scope kind_filter acc =
  let acc' = if scope.kind = kind_filter then scope :: acc else acc in
  List.fold_left
    (fun acc child -> collect_scopes_by_kind child kind_filter acc)
    acc' !(scope.children)

let scopes_by_kind t kind_filter = collect_scopes_by_kind t.root kind_filter []

let string_of_scope_kind (kind : scope_kind) : string =
  match kind with
  | GlobalScope -> "global"
  | BlockScope -> "block"
  | FunctionScope name -> "function(" ^ name ^ ")"
  | StructScope name -> "struct(" ^ name ^ ")"

let string_of_symbol_kind (kind : symbol_kind) : string =
  match kind with Variable -> "var" | Function -> "fn" | Struct -> "struct"

let rec dump_scope buf indent scope =
  let indent_str = String.make indent ' ' in
  let scope_header =
    match scope.kind with
    | GlobalScope -> "GlobalScope"
    | BlockScope -> "BlockScope"
    | FunctionScope name -> "FunctionScope(" ^ name ^ ")"
    | StructScope name -> "StructScope(" ^ name ^ ")"
  in
  Buffer.add_string buf (indent_str ^ "Scope: " ^ scope_header ^ "\n");
  Hashtbl.iter
    (fun name (info : symbol_info) ->
      let kind_str = string_of_symbol_kind info.kind in
      Buffer.add_string buf
        (indent_str ^ "  " ^ name ^ " [" ^ kind_str ^ "] node_idx="
        ^ string_of_int info.node_idx
        ^ "\n"))
    scope.symbols;
  List.iter (dump_scope buf (indent + 2)) !(scope.children)

let dump t : string =
  let buf = Buffer.create 128 in
  dump_scope buf 0 t.root;
  Buffer.contents buf

let rec find_struct_scope scope name =
  match scope.kind with
  | StructScope sname when sname = name -> Some scope
  | _ ->
      List.find_map
        (fun child -> find_struct_scope child name)
        !(scope.children)

let rec find_child_scope_by_kind (parent_scope : scope)
    (kind_filter : scope_kind) : scope option =
  List.find_map
    (fun child ->
      if child.kind = kind_filter then Some child
      else
        find_child_scope_by_kind child
          kind_filter (* Recursively search in children of children *))
    !(parent_scope.children)
