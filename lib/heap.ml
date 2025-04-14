module StringMap = Map.Make (String)

type heap_ptr = { idx : int }

type obj =
  | HeapInvalid
  | HeapNumber of int
  | HeapArray of heap_ptr array
  | HeapObject of { fields : heap_ptr StringMap.t }

type heap = { memory : obj array; free : int }

exception InvalidPtr of heap_ptr

let ptr_valid h ptr = ptr.idx >= 1024 && ptr.idx < h.free

let create () =
  let size = Int.shift_left 1 16 in
  { memory = Array.make size HeapInvalid; free = 1024 }

let alloc h =
  if h.free >= Array.length h.memory then failwith "Out of memory"
  else ({ h with free = h.free + 1 }, { idx = h.free })

let resize h ptr new_len =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapInvalid ->
        h.memory.(ptr.idx) <- HeapArray (Array.make new_len { idx = 0 });
        h
    | HeapArray arr ->
        let old_len = Array.length arr in
        if old_len == new_len then h
        else if old_len < new_len then (
          let added = Array.make new_len { idx = 0 } in
          h.memory.(ptr.idx) <- HeapArray (Array.append arr added);
          h)
        else (
          h.memory.(ptr.idx) <- HeapArray (Array.sub arr 0 new_len);
          h)
    | _ -> failwith "Not an array"

let length h ptr =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapArray arr -> Array.length arr
    | _ -> failwith "Not an array"

let store h ptr x =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapInvalid ->
        h.memory.(ptr.idx) <- HeapNumber x;
        h
    | HeapNumber _ ->
        h.memory.(ptr.idx) <- HeapNumber x;
        h
    | _ -> failwith "Not a number"

let load h ptr =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapNumber x -> x
    | _ -> failwith "Not a number"

let index_set h ptr idx x =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapArray arr ->
        arr.(idx) <- x;
        h
    | _ -> failwith "Not an array"

let index_get h ptr idx =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapArray arr -> arr.(idx)
    | _ -> failwith "Not an array"

let add_field h ptr fname =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapObject obj ->
        let h, new_fields =
          if StringMap.mem fname obj.fields then failwith "Field already exists"
          else
            let h, ptr = alloc h in
            (h, StringMap.add fname ptr obj.fields)
        in
        h.memory.(ptr.idx) <- HeapObject { fields = new_fields };
        h
    | _ -> failwith "Not an object"

let field_get h ptr fname =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapObject obj -> StringMap.find fname obj.fields
    | _ -> failwith "Not an object"

let field_set h ptr fname x =
  if not (ptr_valid h ptr) then raise (InvalidPtr ptr)
  else
    match h.memory.(ptr.idx) with
    | HeapObject obj ->
        let fields = StringMap.add fname x obj.fields in
        h.memory.(ptr.idx) <- HeapObject { fields };
        h
    | _ -> failwith "Not an object"

let rec string_of_obj h ptr =
  let o = h.memory.(ptr.idx) in
  match o with
  | HeapInvalid -> "?"
  | HeapNumber x -> string_of_int x
  | HeapArray a ->
      let s, _ =
        Array.fold_left
          (fun (acc, i) ptr ->
            let s = string_of_obj h ptr in
            if i + 1 = Array.length a then (acc ^ s, i + 1)
            else (acc ^ s ^ ",", i + 1))
          ("[", 0) a
      in
      s ^ "]"
  | HeapObject obj ->
      let s, _ =
        StringMap.fold
          (fun f ptr (acc, i) ->
            let s = string_of_obj h ptr in
            let s =
              if i + 1 = StringMap.cardinal obj.fields then
                Printf.sprintf "%s%s: %s" acc f s
              else Printf.sprintf "%s%s: %s," acc f s
            in
            (s, i + 1))
          obj.fields ("{", 0)
      in
      s ^ "}"
