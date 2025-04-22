exception No_scope

type 'a t = (string, 'a) Hashtbl.t list

let create ?(initial_size = 16) () : 'a t =
  [Hashtbl.create initial_size]

let enter_scope env = Hashtbl.create (Hashtbl.length (List.hd env)) :: env

let exit_scope (env : 'a t) : 'a t =
  match env with
  | _ :: (_ :: _ as rest) -> rest
  | _ -> raise No_scope

let add (env : 'a t) (name : string) (value : 'a) : unit =
  match env with
  | scope :: _ -> Hashtbl.replace scope name value
  | [] -> raise No_scope

let find (env : 'a t) (name : string) : 'a option =
  let rec search = function
    | [] -> None
    | scope :: rest ->
      match Hashtbl.find_opt scope name with
      | Some v -> Some v
      | None -> search rest
  in
  search env

let mem env name =
  match find env name with Some _ -> true | None -> false

let find_exn env name =
  match find env name with
  | Some v -> v
  | None   -> invalid_arg ("Symbol_table.find_exn: “" ^ name ^ "” not bound")

let fold env ~init ~f =
  List.fold_left
    (fun acc scope ->
      Hashtbl.fold (fun key v acc -> f key v acc) scope acc)
    init
    env
