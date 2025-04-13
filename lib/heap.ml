module StringMap = Map.Make (String)

type heap_ptr = { idx : int }

type obj =
  | HeapNumber of int
  | HeapArray of heap_ptr array
  | HeapObject of { fields : heap_ptr StringMap.t }

type heap = { memory : obj array; free : int }

exception InvalidPtr of heap_ptr

let get_obj h ptr =
  if ptr.idx < Array.length h.memory then h.memory.(ptr.idx)
  else raise @@ InvalidPtr ptr

let rec string_of_obj h o =
  match o with
  | HeapNumber x -> string_of_int x
  | HeapArray a ->
      let s, _ =
        Array.fold_left
          (fun (acc, i) ptr ->
            let s = string_of_obj h @@ get_obj h ptr in
            if i + 1 = Array.length a then (acc ^ s, i + 1)
            else (acc ^ s ^ ",", i + 1))
          ("[", 0) a
      in
      s ^ "]"
  | HeapObject obj ->
      let s, _ =
        StringMap.fold
          (fun f ptr (acc, i) ->
            let s = string_of_obj h @@ get_obj h ptr in
            let s =
              if i + 1 = StringMap.cardinal obj.fields then
                Printf.sprintf "%s%s: %s" acc f s
              else Printf.sprintf "%s%s: %s," acc f s
            in
            (s, i + 1))
          obj.fields ("{", 0)
      in
      s ^ "}"
