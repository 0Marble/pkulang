module StringMap = Map.Make (String)
module IntSet = Set.Make (Int)

type obj =
  | HeapInvalid
  | FreeList of int
  | HeapNumber of int
  | HeapArray of Value.value array
  | HeapObject of { fields : Value.value StringMap.t }
  | Coroutine of Stack.frame

type heap = {
  memory : obj array;
  min_address : int;
  used_size : int;
  max_address : int;
  free : int;
  gc_threshold : int;
  prev_gc : int;
}

exception NotAPointer of Value.value
exception InvalidPtr of Value.heap_ptr
exception UseAfterFree of Value.heap_ptr
exception OOM

let ptr_valid h ptr =
  match ptr with
  | Value.Number _ -> raise (NotAPointer ptr)
  | Value.Pointer ptr -> (
      if not (ptr.idx >= 1024 && ptr.idx < h.max_address) then
        raise (InvalidPtr ptr)
      else
        match h.memory.(ptr.idx) with
        | FreeList _ -> raise (UseAfterFree ptr)
        | _ -> ptr)

let create () =
  let size = Int.shift_left 1 16 in
  {
    memory = Array.make size HeapInvalid;
    free = 1024;
    min_address = 1024;
    used_size = 0;
    max_address = 1024;
    gc_threshold = 4096;
    prev_gc = 0;
  }

let alloc h =
  let slot = h.free in
  if slot >= Array.length h.memory then raise OOM
  else
    let next_free, max_address =
      match h.memory.(slot) with
      | FreeList next_free -> (next_free, h.max_address)
      | _ -> (slot + 1, h.max_address + 1)
    in
    h.memory.(slot) <- HeapInvalid;
    ( { h with free = next_free; used_size = h.used_size + 1; max_address },
      Value.Pointer { idx = slot } )

let dealloc h ptr =
  let ptr = ptr_valid h ptr in
  h.memory.(ptr.idx) <- FreeList h.free;
  { h with free = ptr.idx; used_size = h.used_size - 1 }

let store_coroutine h ptr f =
  let ptr = ptr_valid h ptr in
  h.memory.(ptr.idx) <- Coroutine f

let get_coroutine h ptr =
  let ptr' = ptr_valid h ptr in
  match h.memory.(ptr'.idx) with
  | Coroutine f -> (h, { f with ptr })
  | _ -> failwith "not a coroutine"

let resize h ptr new_len =
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapInvalid ->
      h.memory.(ptr.idx) <- HeapArray (Array.make new_len Value.null);
      h
  | HeapArray arr ->
      let old_len = Array.length arr in
      if old_len == new_len then h
      else if old_len < new_len then (
        let added = Array.make new_len Value.null in
        h.memory.(ptr.idx) <- HeapArray (Array.append arr added);
        h)
      else (
        h.memory.(ptr.idx) <- HeapArray (Array.sub arr 0 new_len);
        h)
  | _ -> failwith "Not an array"

let length h ptr =
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapArray arr -> Array.length arr
  | _ -> failwith "Not an array"

let store h ptr x =
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapInvalid ->
      h.memory.(ptr.idx) <- HeapNumber x;
      h
  | HeapNumber _ ->
      h.memory.(ptr.idx) <- HeapNumber x;
      h
  | _ -> failwith "Not a number"

let load h ptr =
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapNumber x -> x
  | _ -> failwith "Not a number"

let index_set h ptr idx x =
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapArray arr ->
      arr.(idx) <- x;
      h
  | _ -> failwith "Not an array"

let index_get h ptr idx =
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapArray arr -> arr.(idx)
  | _ -> failwith "Not an array"

let add_field h ptr fname =
  let ptr = ptr_valid h ptr in
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
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapObject obj -> StringMap.find fname obj.fields
  | _ -> failwith "Not an object"

let field_set h ptr fname x =
  let ptr = ptr_valid h ptr in
  match h.memory.(ptr.idx) with
  | HeapObject obj ->
      let fields = StringMap.add fname x obj.fields in
      h.memory.(ptr.idx) <- HeapObject { fields };
      h
  | _ -> failwith "Not an object"

let rec string_of_obj ?(include_ptr = false) h ptr =
  match ptr with
  | Value.Number x -> string_of_int x
  | Value.Pointer ptr ->
      let o = h.memory.(ptr.idx) in
      let s =
        match o with
        | HeapInvalid -> "?"
        | FreeList x -> "!->*" ^ string_of_int x
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
        | Coroutine f -> "crt-" ^ string_of_int f.start
      in
      if include_ptr then Printf.sprintf "(*%d)%s" ptr.idx s else s

let force_gc h active =
  let rec mark h (ptr : Value.heap_ptr) visited =
    match h.memory.(ptr.idx) with
    | FreeList _ -> failwith "Shouldnt be accessable"
    | HeapNumber _ | HeapInvalid -> IntSet.add ptr.idx visited
    | HeapArray arr ->
        let visited = IntSet.add ptr.idx visited in
        Array.fold_left
          (fun visited elem ->
            match elem with
            | Value.Number _ -> visited
            | Value.Pointer elem ->
                if IntSet.mem elem.idx visited then visited
                else mark h elem visited)
          visited arr
    | HeapObject o ->
        let visited = IntSet.add ptr.idx visited in
        StringMap.fold
          (fun _ ptr visited ->
            match ptr with
            | Value.Number _ -> visited
            | Value.Pointer ptr ->
                if IntSet.mem ptr.idx visited then visited
                else mark h ptr visited)
          o.fields visited
    | Coroutine f ->
        let active = Stack.get_active f in
        List.fold_left
          (fun visited (elem : Value.heap_ptr) ->
            if IntSet.mem elem.idx visited then visited else mark h elem visited)
          visited active
  in
  let rec sweep h i accessible =
    if i >= h.max_address then h
    else
      match h.memory.(i) with
      | FreeList _ -> sweep h (i + 1) accessible
      | _ ->
          if IntSet.mem i accessible then sweep h (i + 1) accessible
          else
            let h = dealloc h (Pointer { idx = i }) in
            sweep h (i + 1) accessible
  in
  let accessible =
    List.fold_left (fun visited root -> mark h root visited) IntSet.empty active
  in
  let h' = sweep h h.min_address accessible in
  { h' with prev_gc = h.used_size }

let maybe_gc h get_active =
  if h.used_size / h.gc_threshold = h.prev_gc / h.gc_threshold then h
  else force_gc h (get_active ())
