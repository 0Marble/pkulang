type 'a t = { next : 'a t ref option array; mutable value : 'a option }

let init () = ref { next = Array.make 256 None; value = None }

let rec get_word (root : 'a t ref) s =
  let len = String.length s in
  if len = 0 then !root.value
  else
    let idx = int_of_char s.[0] in
    let rest = String.sub s 1 (len - 1) in
    match !root.next.(idx) with Some leaf -> get_word leaf rest | None -> None

let rec add_word (root : 'a t ref) s value =
  let len = String.length s in
  if len = 0 then !root.value <- Some value
  else
    let c = s.[0] in
    let idx = int_of_char c in
    let rest = String.sub s 1 (len - 1) in
    match !root.next.(idx) with
    | Some leaf -> add_word leaf rest value
    | None ->
        let leaf = init () in
        assert (Option.is_none !root.next.(idx));
        !root.next.(idx) <- Some leaf;
        add_word leaf rest value

type 'a finder_type = { root : 'a t }

let finder root = { root = !root }

let push f c =
  let idx = int_of_char c in
  match f.root.next.(idx) with
  | Some leaf -> Some { root = !leaf }
  | None -> None

let get f = f.root.value
