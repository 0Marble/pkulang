exception No_scope

type scope_kind =
  | Global
  | Function of string
  | Block of int  (* node_idx *)

type symbol_info = {
  name : string;
  kind : [ `Variable | `Function | `Struct ];
  node_idx : int;
  ty : string option;  (* optional type annotation *)
  declared_in : scope_kind;
}

type t = {
  mutable scopes : (scope_kind * (string, symbol_info) Hashtbl.t) list;
  mutable by_node_idx : (int, symbol_info) Hashtbl.t;  (* NEW *)
}

let create () : t =
  {
    scopes = [ (Global, Hashtbl.create 16) ];
    by_node_idx = Hashtbl.create 128;
  }

let enter_scope t kind =
  let new_scope = (kind, Hashtbl.create 16) in
  t.scopes <- new_scope :: t.scopes

let exit_scope t =
  match t.scopes with
  | _ :: rest when List.length rest >= 1 -> t.scopes <- rest
  | _ -> raise No_scope

let add_symbol t (info : symbol_info) : unit =
  match t.scopes with
  | (_, table) :: _ ->
      Hashtbl.replace table info.name info;
      Hashtbl.replace t.by_node_idx info.node_idx info
  | [] -> raise No_scope

let find_symbol t name : symbol_info option =
  let rec aux = function
    | [] -> None
    | (_, table) :: rest ->
        (match Hashtbl.find_opt table name with
         | Some sym -> Some sym
         | None -> aux rest)
  in
  aux t.scopes

let find_symbol_exn t name =
  match find_symbol t name with
  | Some s -> s
  | None -> invalid_arg ("symbolTable: " ^ name ^ " not bound")

let find_by_node_idx t idx : symbol_info option =
  Hashtbl.find_opt t.by_node_idx idx

let find_by_node_idx_exn t idx : symbol_info =
  match find_by_node_idx t idx with
  | Some s -> s
  | None -> invalid_arg ("symbolTable: node_idx " ^ string_of_int idx ^ " not bound")

let current_scope_kind t =
  match t.scopes with
  | (k, _) :: _ -> k
  | [] -> raise No_scope

let fold_symbols t ~init ~f =
  List.fold_left
    (fun acc (_, scope) ->
      Hashtbl.fold (fun _ sym acc -> f sym acc) scope acc
    ) init t.scopes

let all_symbols t : symbol_info list =
  Hashtbl.fold (fun _ sym acc -> sym :: acc) t.by_node_idx []

let symbols_by_kind t kind_filter =
  all_symbols t
  |> List.filter (fun sym -> sym.kind = kind_filter)

let dump t : string =
  let buf = Buffer.create 128 in
  List.iter (fun (kind, scope) ->
    Buffer.add_string buf
      (match kind with
       | Global -> "Scope: Global\n"
       | Function name -> "Scope: Function " ^ name ^ "\n"
       | Block idx -> "Scope: Block node_idx=" ^ string_of_int idx ^ "\n");
    Hashtbl.iter (fun name info ->
      let ty_str = match info.ty with Some t -> ": " ^ t | None -> "" in
      Buffer.add_string buf ("  " ^ name ^ ty_str ^ " [" ^
                              (match info.kind with
                               | `Variable -> "var"
                               | `Function -> "fun"
                               | `Struct -> "struct") ^
                              "] node_idx=" ^ string_of_int info.node_idx ^ "\n")
    ) scope;
  ) (List.rev t.scopes);  (* print from outermost to innermost *)
  Buffer.contents buf
