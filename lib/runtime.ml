module IntSet = Set.Make (Int)

type operand = Number of int | Null | Ip | Location of Stack.location
type jump_target = Static of int | Relative of int | Dynamic of Stack.location

type command_kind =
  | Halt
  | Nop
  | Trap
  | Trace of int
  | Add of (Stack.location * operand * operand)
  | Sub of (Stack.location * operand * operand)
  | Lt of (Stack.location * operand * operand)
  | Eq of (Stack.location * operand * operand)
  | Assign of (Stack.location * operand)
  | Goto of jump_target
  | GotoIfZero of (operand * jump_target)
  | GotoIfNeg of (operand * jump_target)
  | GotoIfNull of (operand * jump_target)
  | Call of (Stack.location * operand array * jump_target)
  | Ret of operand
  | Create of (Stack.location * operand array * jump_target)
    (* val=resume(crt) or goto tgt *)
  | Resume of (Stack.location * operand * jump_target)
  | Yield of operand
  | Alloca of int
  | New of Stack.location
  | Free of operand
  | ForceGc
  | DisableGc
  | EnableGc
  | Resize of (Stack.location * operand)
  | Size of (Stack.location * operand)
  | AddField of (Stack.location * string)
  | FieldSet of (Stack.location * string * operand)
  | FieldGet of (Stack.location * operand * string)
  | IndexSet of (Stack.location * operand * operand)
  | IndexGet of (Stack.location * operand * operand)
  | Store of (Stack.location * operand)
  | Load of (Stack.location * operand)
  | Builtin of (operand array * string)

type command = { cmd : command_kind; loc : Location.location }

and runtime = {
  stack : Stack.stack;
  heap : Heap.heap;
  code : command array;
  stdin : string;
  stdout : string;
  source : string;
  gc_on : bool;
  (* tracing *)
  last_instrs : int Queue.t;
  keep_instrs : int;
}

let string_of_operand_dyn r (f : Stack.frame) op =
  match op with
  | Number x -> string_of_int x
  | Location x ->
      let v =
        match x with
        | Register i -> f.locals.(i)
        | Argument i -> f.args.(i)
        | Void -> raise Stack.WriteToVoid
      in
      let loc = Stack.string_of_location x in
      let obj = Heap.string_of_obj ~include_ptr:true r.heap v in
      Printf.sprintf "%s->%s" loc obj
  | Null -> "Null"
  | Ip -> "Ip"

let string_of_cmd ?(ctx : (runtime * Stack.frame) option = None) ?(mark = false)
    idx c =
  let sov =
    match ctx with
    | None -> fun s _ -> s
    | Some (r, f) -> fun _ x -> string_of_operand_dyn r f x
  in

  let string_of_tgt a =
    match a with
    | Static x -> string_of_int x
    | Dynamic x -> sov (Stack.string_of_location x) (Location x)
    | Relative x -> (
        let s = if x >= 0 then "+" ^ string_of_int x else string_of_int x in
        match ctx with
        | Some (_, f) -> Printf.sprintf "%s=%d" s (f.ip + x)
        | None -> s)
  in
  let string_of_operand o =
    match o with
    | Ip -> "Ip"
    | Null -> "Null"
    | Number x -> string_of_int x
    | Location x -> sov (Stack.string_of_location x) (Location x)
  in
  let string_of_dest a = Stack.string_of_location a in
  let s =
    match c with
    | Halt -> "Halt"
    | Nop -> "Nop"
    | Trace x -> "Trace " ^ string_of_int x
    | Trap -> "Trap"
    | Add (dest, lhs, rhs) ->
        Printf.sprintf "Add %s %s %s" (string_of_dest dest)
          (string_of_operand lhs) (string_of_operand rhs)
    | Sub (dest, lhs, rhs) ->
        Printf.sprintf "Sub %s %s %s" (string_of_dest dest)
          (string_of_operand lhs) (string_of_operand rhs)
    | Lt (dest, lhs, rhs) ->
        Printf.sprintf "Lt %s %s %s" (string_of_dest dest)
          (string_of_operand lhs) (string_of_operand rhs)
    | Eq (dest, lhs, rhs) ->
        Printf.sprintf "Eq %s %s %s" (string_of_dest dest)
          (string_of_operand lhs) (string_of_operand rhs)
    | Assign (dest, v) ->
        Printf.sprintf "Assign %s %s" (string_of_dest dest)
          (string_of_operand v)
    | Goto tgt -> "Goto " ^ string_of_tgt tgt
    | GotoIfZero (op, tgt) ->
        Printf.sprintf "GotoIfZero %s %s" (string_of_operand op)
          (string_of_tgt tgt)
    | GotoIfNeg (op, tgt) ->
        Printf.sprintf "GotoIfNeg %s %s" (string_of_operand op)
          (string_of_tgt tgt)
    | GotoIfNull (op, tgt) ->
        Printf.sprintf "GotoIfNull %s %s" (string_of_operand op)
          (string_of_tgt tgt)
    | Call (dest, args, fn) ->
        Printf.sprintf "Call %s %s %s" (string_of_dest dest)
          (Array.fold_left
             (fun acc op -> acc ^ string_of_operand op ^ ",")
             "[" args
          ^ "]")
          (string_of_tgt fn)
    | Ret op -> "Ret " ^ string_of_operand op
    | Create (dest, args, fn) ->
        Printf.sprintf "Create %s %s %s" (string_of_dest dest)
          (Array.fold_left
             (fun acc op -> acc ^ string_of_operand op ^ ",")
             "[" args
          ^ "]")
          (string_of_tgt fn)
    | Resume (dest, arg, tgt) ->
        Printf.sprintf "Resume %s %s %s" (string_of_dest dest)
          (string_of_operand arg) (string_of_tgt tgt)
    | Yield op -> Printf.sprintf "Yield %s" (string_of_operand op)
    | Alloca amt -> "Alloca " ^ string_of_int amt
    | New dst -> "New " ^ string_of_dest dst
    | Free op -> "Free " ^ string_of_operand op
    | ForceGc -> "ForceGc"
    | DisableGc -> "DisableGc"
    | EnableGc -> "EnableGc"
    | Resize (arr, size) ->
        Printf.sprintf "Resize %s %s" (string_of_dest arr)
          (string_of_operand size)
    | Size (dst, arr) ->
        Printf.sprintf "Size %s %s" (string_of_dest dst) (string_of_operand arr)
    | AddField (obj, fname) ->
        Printf.sprintf "AddField %s %s" (string_of_dest obj) fname
    | FieldSet (obj, fname, v) ->
        Printf.sprintf "FieldSet %s %s %s" (string_of_dest obj) fname
          (string_of_operand v)
    | FieldGet (dest, obj, fname) ->
        Printf.sprintf "FieldGet %s %s %s" (string_of_dest dest)
          (string_of_operand obj) fname
    | IndexSet (dest, arr, idx) ->
        Printf.sprintf "IndexSet %s %s %s" (string_of_dest dest)
          (string_of_operand arr) (string_of_operand idx)
    | IndexGet (dest, arr, idx) ->
        Printf.sprintf "IndexGet %s %s %s" (string_of_dest dest)
          (string_of_operand arr) (string_of_operand idx)
    | Load (dest, ptr) ->
        Printf.sprintf "Load %s %s" (string_of_dest dest)
          (string_of_operand ptr)
    | Store (dest, v) ->
        Printf.sprintf "Store %s %s" (string_of_dest dest) (string_of_operand v)
    | Builtin (args, fn) ->
        Printf.sprintf "Builtin.%s %s" fn
          (Array.fold_left
             (fun acc op -> acc ^ string_of_operand op ^ ",")
             "[" args
          ^ "]")
  in
  if not mark then Printf.sprintf "[%5d] %s" idx s
  else Printf.sprintf "[>%4d] %s" idx s

let create source code main =
  let rec (frame : Stack.frame) =
    {
      start = main;
      args = [||];
      locals = [||];
      caller = None;
      result = Stack.Void;
      ip = main;
      stack;
    }
  and (stack : Stack.stack) =
    { top = frame; bot = frame; yielder = None; ptr = Value.null }
  in
  {
    heap = Heap.create ();
    gc_on = true;
    stdout = "";
    stdin = "";
    stack;
    code;
    source;
    last_instrs = Queue.create ();
    keep_instrs = 20;
  }

let trace ?(flags = 15) r =
  prerr_string "================TRACE START================\n";

  if Int.logand flags 1 <> 0 then (
    prerr_string "====== Section 1: Last Instructions =======\n";
    let s =
      Queue.fold
        (fun acc idx -> acc ^ string_of_cmd idx r.code.(idx).cmd ^ "\n")
        "" r.last_instrs
    in
    prerr_string s)
  else ();

  if Int.logand flags 2 <> 0 then (
    prerr_string "========= Section 2: Stack Trace ==========\n";
    let rec stack_trace r (f : Stack.frame) =
      let acc =
        match f.caller with Some caller -> stack_trace r caller | None -> "\n"
      in
      Printf.sprintf "%s\n%s"
        (string_of_cmd ~ctx:(Some (r, f)) f.ip r.code.(f.ip).cmd)
        acc
    in
    prerr_string @@ stack_trace r r.stack.top)
  else ();

  if Int.logand flags 4 <> 0 then (
    prerr_string "===== Section 3: Current Function Code ====\n";
    let rec function_code r ip visited =
      try
        let cmd = r.code.(ip) in
        match cmd.cmd with
        | Ret _ | Halt | Trap -> (IntSet.add ip visited, [ ip ])
        | Goto tgt | GotoIfNeg (_, tgt) | GotoIfZero (_, tgt) ->
            let v = IntSet.add ip visited in
            let v, block =
              match tgt with
              | Static next_ip ->
                  if IntSet.mem next_ip v then (v, [])
                  else function_code r next_ip v
              | Dynamic _ -> (v, [])
              | Relative x ->
                  let next_ip = ip + x in
                  if IntSet.mem next_ip v then (v, [])
                  else function_code r next_ip v
            in
            let v, l = function_code r (ip + 1) (IntSet.add ip v) in
            (v, (ip :: l) @ block)
        | _ ->
            let v, l = function_code r (ip + 1) (IntSet.add ip visited) in
            (v, ip :: l)
      with Invalid_argument _ -> (visited, [])
    in
    let _, cmds = function_code r r.stack.top.start IntSet.empty in
    List.iter (fun ip ->
        prerr_endline
        @@ string_of_cmd ~mark:(ip = r.stack.top.ip) ip r.code.(ip).cmd)
    @@ cmds)
  else ();

  if Int.logand flags 8 <> 0 then (
    prerr_string "====== Section 4: Current Registers =======\n";
    Array.iteri
      (fun i _ ->
        prerr_endline
        @@ string_of_operand_dyn r r.stack.top (Location (Argument i)))
      r.stack.top.args;
    Array.iteri
      (fun i _ ->
        prerr_endline
        @@ string_of_operand_dyn r r.stack.top (Location (Register i)))
      r.stack.top.locals)
  else ();

  prerr_endline "=================TRACE END=================\n"

let finished r =
  let isp = r.stack.top.ip in
  let cmd = r.code.(isp) in
  if cmd.cmd = Halt then true else false

let step r =
  try
    let get_active r () = Stack.get_active r.stack in
    let next r = { r with stack = Stack.inc_ip r.stack } in
    let op_to_val r op =
      match op with
      | Ip -> Value.Number r.stack.top.ip
      | Null -> Value.null
      | Number x -> Value.Number x
      | Location x -> Stack.load r.stack x
    in
    let val_to_int v =
      match v with Value.Number x -> x | Pointer _ -> failwith "Not a number"
    in
    let val_to_ptr v =
      match v with Value.Pointer x -> x | Number _ -> failwith "Not a ptr"
    in
    let get_ip r addr =
      match addr with
      | Static x -> x
      | Dynamic x -> (
          match Stack.load r.stack x with
          | Value.Number y -> y
          | _ -> failwith "Todo")
      | Relative x -> r.stack.top.ip + x
    in

    let r =
      if r.gc_on then { r with heap = Heap.maybe_gc r.heap (get_active r) }
      else r
    in
    let cmd = r.code.(r.stack.top.ip) in

    if Queue.length r.last_instrs >= r.keep_instrs then
      let _ = Queue.pop r.last_instrs in
      ()
    else ();
    Queue.push r.stack.top.ip r.last_instrs;

    match cmd.cmd with
    | Halt -> r
    | Nop -> next r
    | Trap -> failwith "Trap"
    | Trace x ->
        trace ~flags:x r;
        next r
    | Alloca amt -> next { r with stack = Stack.alloca r.stack amt }
    | Add (dest, lhs, rhs) ->
        Stack.store r.stack dest
          (Value.Number
             ((op_to_val r lhs |> val_to_int) + (op_to_val r rhs |> val_to_int)));
        next r
    | Sub (dest, lhs, rhs) ->
        Stack.store r.stack dest
          (Value.Number
             ((op_to_val r lhs |> val_to_int) - (op_to_val r rhs |> val_to_int)));
        next r
    | Lt (dest, lhs, rhs) ->
        let res =
          if op_to_val r lhs |> val_to_int < (op_to_val r rhs |> val_to_int)
          then 1
          else 0
        in
        Stack.store r.stack dest (Value.Number res);
        next r
    | Eq (dest, lhs, rhs) ->
        let res =
          if op_to_val r lhs |> val_to_int = (op_to_val r rhs |> val_to_int)
          then 1
          else 0
        in
        Stack.store r.stack dest (Value.Number res);
        next r
    | Assign (dest, src) ->
        Stack.store r.stack dest (op_to_val r src);
        next r
    | Call (dest, args, fn) ->
        let args = Array.map (fun x -> op_to_val r x) args in
        let fn = get_ip r fn in
        { r with stack = Stack.call r.stack dest args fn }
    | Ret v ->
        let v = op_to_val r v in
        let return_stack = Stack.ret r.stack v in
        { r with stack = return_stack }
    | Builtin (args, "print") ->
        let s, _ =
          Array.fold_left
            (fun (acc, i) op ->
              let s = Heap.string_of_obj r.heap @@ op_to_val r op in
              if i + 1 = Array.length args then (acc ^ s, i + 1)
              else (acc ^ s ^ ", ", i + 1))
            ("", 0) args
        in
        print_endline s;
        next { r with stdout = r.stdout ^ s ^ "\n" }
    | Builtin (_, _) -> failwith "Unknown builtin"
    | Goto tgt ->
        let ip = get_ip r tgt in
        { r with stack = Stack.goto r.stack ip }
    | GotoIfZero (v, addr) ->
        if op_to_val r v |> val_to_int = 0 then
          let ip = get_ip r addr in
          { r with stack = Stack.goto r.stack ip }
        else next r
    | GotoIfNeg (v, addr) ->
        if op_to_val r v |> val_to_int < 0 then
          let ip = get_ip r addr in
          { r with stack = Stack.goto r.stack ip }
        else next r
    | GotoIfNull (v, addr) ->
        if (op_to_val r v |> val_to_ptr).idx = 0 then
          let ip = get_ip r addr in
          { r with stack = Stack.goto r.stack ip }
        else next r
    | New dest ->
        let h, ptr = Heap.alloc r.heap in
        Stack.store r.stack dest ptr;
        next { r with heap = h }
    | Free op ->
        let ptr = op_to_val r op in
        let h = Heap.dealloc r.heap ptr in
        next { r with heap = h }
    | ForceGc ->
        let active = get_active r () in
        let h = Heap.force_gc r.heap active in
        next { r with heap = h }
    | DisableGc -> next { r with gc_on = false }
    | EnableGc -> next { r with gc_on = true }
    | Resize (arr, len) ->
        let arr = Stack.load r.stack arr in
        let len = op_to_val r len |> val_to_int in
        let h = Heap.resize r.heap arr len in
        next { r with heap = h }
    | Size (dest, arr) ->
        let arr = op_to_val r arr in
        Stack.store r.stack dest @@ Value.Number (Heap.length r.heap arr);
        next r
    | IndexSet (dest, idx, x) ->
        let arr = Stack.load r.stack dest in
        let idx = op_to_val r idx |> val_to_int in
        let x = op_to_val r x in
        let h = Heap.index_set r.heap arr idx x in
        next { r with heap = h }
    | IndexGet (dest, arr, idx) ->
        let arr = op_to_val r arr in
        let idx = op_to_val r idx |> val_to_int in
        Stack.store r.stack dest (Heap.index_get r.heap arr idx);
        next r
    | Load (dest, op) ->
        let ptr = op_to_val r op in
        let x = Heap.load r.heap ptr in
        Stack.store r.stack dest (Value.Number x);
        next r
    | Store (dest, op) ->
        let x = op_to_val r op |> val_to_int in
        let ptr = Stack.load r.stack dest in
        let h = Heap.store r.heap ptr x in
        next { r with heap = h }
    | AddField (obj, fname) ->
        let obj = Stack.load r.stack obj in
        let h = Heap.add_field r.heap obj fname in
        next { r with heap = h }
    | FieldSet (obj, fname, v) ->
        let obj = Stack.load r.stack obj in
        let v = op_to_val r v in
        let h = Heap.field_set r.heap obj fname v in
        next { r with heap = h }
    | FieldGet (dest, obj, fname) ->
        let obj = op_to_val r obj in
        let v = Heap.field_get r.heap obj fname in
        Stack.store r.stack dest v;
        next r
    | Create (dest, args, fn) ->
        let args = Array.map (fun x -> op_to_val r x) args in
        let fn = get_ip r fn in
        let rec (frame : Stack.frame) =
          {
            start = fn;
            args;
            locals = [||];
            caller = None;
            result = Void;
            stack;
            ip = fn;
          }
        and stack =
          { top = frame; bot = frame; yielder = None; ptr = Value.null }
        in
        let h, ptr = Heap.alloc r.heap in
        Stack.store r.stack dest ptr;
        Heap.store_coroutine r.heap ptr stack;
        let r = { r with heap = h } in
        next r
    | Yield v ->
        let v = op_to_val r v in
        let coroutine = Stack.inc_ip r.stack in
        let yielder =
          match coroutine.yielder with
          | Some (y, _) -> y
          | None -> failwith "Not a coroutine!"
        in
        Stack.store yielder yielder.top.result v;
        Heap.store_coroutine r.heap coroutine.ptr coroutine;
        { r with stack = yielder }
    | Resume (dest, crt, if_end) ->
        let yielder = Stack.inc_ip r.stack in
        let if_end = get_ip r if_end in
        let (yielder : Stack.stack) =
          { yielder with top = { yielder.top with result = dest } }
        in
        let h, coroutine = Heap.get_coroutine r.heap (op_to_val r crt) in
        let coroutine = { coroutine with yielder = Some (yielder, if_end) } in
        { r with stack = coroutine; heap = h }
  with e ->
    trace r;
    raise e
