module IntSet = Set.Make (Int)

type value = DynNumber of int | DynPtr of Heap.heap_ptr
type operand = Number of int | Null | Ip | Register of int | Argument of int
type destination = Register of int
type jump_target = Static of int | Relative of int | Dynamic of int

type command_kind =
  | Halt
  | Nop
  | Trap
  | Trace of int
  | Add of (destination * operand * operand)
  | Sub of (destination * operand * operand)
  | Assign of (destination * operand)
  | Goto of jump_target
  | GotoIfZero of (operand * jump_target)
  | GotoIfNeg of (operand * jump_target)
  | GotoIfNull of (operand * jump_target)
  | Call of (destination * operand array * jump_target)
  | Ret of operand
  | Alloca of int
  | New of destination
  | Resize of (destination * operand)
  | AddField of (destination * string)
  | IndexSet of (destination * operand * operand)
  | IndexGet of (destination * operand * operand)
  | Store of (destination * operand)
  | Load of (destination * operand)
  | Builtin of (operand array * string)

type command = { cmd : command_kind; loc : Location.location }

type stack_frame = {
  start : int;
  args : value array;
  locals : value array;
  caller : stack_frame option;
  ip : int;
  return : destination;
}

and runtime = {
  stack : stack_frame;
  heap : Heap.heap;
  code : command array;
  stdin : string;
  stdout : string;
  source : string;
  (* tracing *)
  last_instrs : int Queue.t;
  keep_instrs : int;
}

let string_of_operand_dyn r f op =
  match op with
  | Number x -> string_of_int x
  | Register x -> (
      match f.locals.(x) with
      | DynNumber y -> Printf.sprintf "r%d=%d" x y
      | DynPtr y ->
          Printf.sprintf "r%d->(*%d)%s" x y.idx (Heap.string_of_obj r.heap y))
  | Argument x -> (
      match f.args.(x) with
      | DynNumber y -> Printf.sprintf "a%d=%d" x y
      | DynPtr y ->
          Printf.sprintf "a%d->(*%d)%s" x y.idx (Heap.string_of_obj r.heap y))
  | Null -> "Null"
  | Ip -> "Ip"

let string_of_cmd ?(ctx = None) ?(mark = false) idx c =
  let sov =
    match ctx with
    | None -> fun s _ -> s
    | Some (r, f) -> fun _ x -> string_of_operand_dyn r f x
  in

  let string_of_tgt a =
    match a with
    | Static x -> string_of_int x
    | Dynamic x -> sov ("r" ^ string_of_int x) (Register x)
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
    | Register x -> sov ("r" ^ string_of_int x) (Register x)
    | Argument x -> sov ("a" ^ string_of_int x) (Argument x)
  in
  let string_of_dest a = match a with Register x -> "r" ^ string_of_int x in
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
    | Alloca amt -> "Alloca " ^ string_of_int amt
    | New dst -> "New " ^ string_of_dest dst
    | Resize (arr, size) ->
        Printf.sprintf "Resize %s %s" (string_of_dest arr)
          (string_of_operand size)
    | AddField (obj, fname) ->
        Printf.sprintf "AddField %s %s" (string_of_dest obj) fname
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
  {
    heap = Heap.create ();
    stdout = "";
    stdin = "";
    stack =
      {
        start = main;
        caller = None;
        ip = main;
        args = [||];
        locals = [||];
        return = Register 0;
      };
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
    let rec stack_trace r f =
      let acc =
        match f.caller with Some caller -> stack_trace r caller | None -> "\n"
      in
      Printf.sprintf "%s\n%s"
        (string_of_cmd ~ctx:(Some (r, f)) f.ip r.code.(f.ip).cmd)
        acc
    in
    prerr_string @@ stack_trace r r.stack)
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
    let _, cmds = function_code r r.stack.start IntSet.empty in
    List.iter (fun ip ->
        prerr_endline
        @@ string_of_cmd ~mark:(ip = r.stack.ip) ip r.code.(ip).cmd)
    @@ cmds)
  else ();

  if Int.logand flags 8 <> 0 then (
    prerr_string "====== Section 4: Current Registers =======\n";
    Array.iteri
      (fun i _ -> prerr_endline @@ string_of_operand_dyn r r.stack (Argument i))
      r.stack.args;
    Array.iteri
      (fun i _ -> prerr_endline @@ string_of_operand_dyn r r.stack (Register i))
      r.stack.locals)
  else ();

  prerr_endline "=================TRACE END=================\n"

let finished r =
  let isp = r.stack.ip in
  let cmd = r.code.(isp) in
  if cmd.cmd = Halt then true else false

let step r =
  try
    let next r = { r with stack = { r.stack with ip = r.stack.ip + 1 } } in
    let store r addr x =
      match addr with Register a -> r.stack.locals.(a) <- x
    in
    let op_to_val r op =
      match op with
      | Ip -> DynNumber r.stack.ip
      | Null -> DynPtr { idx = 0 }
      | Number x -> DynNumber x
      | Register x -> r.stack.locals.(x)
      | Argument x -> r.stack.args.(x)
    in
    let val_to_int v =
      match v with DynNumber x -> x | DynPtr _ -> failwith "Not a number"
    in
    let val_to_ptr v =
      match v with DynPtr x -> x | DynNumber _ -> failwith "Not a ptr"
    in
    let dest_to_val r d =
      match d with Register x -> op_to_val r (Register x)
    in
    let get_ip r addr =
      match addr with
      | Static x -> x
      | Dynamic x -> (
          match r.stack.locals.(x) with
          | DynNumber y -> y
          | _ -> failwith "Todo")
      | Relative x -> r.stack.ip + x
    in

    let cmd = r.code.(r.stack.ip) in

    if Queue.length r.last_instrs >= r.keep_instrs then
      let _ = Queue.pop r.last_instrs in
      ()
    else ();
    Queue.push r.stack.ip r.last_instrs;

    match cmd.cmd with
    | Halt -> r
    | Nop -> next r
    | Trap -> failwith "Trap"
    | Trace x ->
        trace ~flags:x r;
        next r
    | Alloca amt ->
        next
          {
            r with
            stack =
              {
                r.stack with
                locals =
                  Array.append r.stack.locals (Array.make amt (DynNumber 0));
              };
          }
    | Add (dest, lhs, rhs) ->
        store r dest
          (DynNumber
             ((op_to_val r lhs |> val_to_int) + (op_to_val r rhs |> val_to_int)));
        next r
    | Sub (dest, lhs, rhs) ->
        store r dest
          (DynNumber
             ((op_to_val r lhs |> val_to_int) - (op_to_val r rhs |> val_to_int)));
        next r
    | Assign (dest, src) ->
        store r dest (op_to_val r src);
        next r
    | Call (dest, args, fn) ->
        let callee =
          {
            start = get_ip r fn;
            locals = [||];
            args = Array.map (fun x -> op_to_val r x) args;
            caller = Some r.stack;
            return = dest;
            ip = get_ip r fn;
          }
        in
        { r with stack = callee }
    | Ret v ->
        let callee = r.stack in
        let caller =
          match callee.caller with
          | Some f -> f
          | None ->
              Error.fail_at_spot r.source "Stack underflow" cmd.loc Error.Eval
        in
        let r' = { r with stack = caller } in
        store r' callee.return (op_to_val r v);
        next r'
    | Builtin (args, "print") ->
        let s, _ =
          Array.fold_left
            (fun (acc, i) op ->
              let s =
                match op_to_val r op with
                | DynNumber x -> string_of_int x
                | DynPtr x -> Heap.string_of_obj r.heap x
              in
              if i + 1 = Array.length args then (acc ^ s, i + 1)
              else (acc ^ s ^ ", ", i + 1))
            ("", 0) args
        in
        print_endline s;
        next { r with stdout = r.stdout ^ s ^ "\n" }
    | Builtin (_, _) -> failwith "Unknown builtin"
    | Goto tgt -> { r with stack = { r.stack with ip = get_ip r tgt } }
    | GotoIfZero (v, addr) ->
        if op_to_val r v |> val_to_int = 0 then
          { r with stack = { r.stack with ip = get_ip r addr } }
        else next r
    | GotoIfNeg (v, addr) ->
        if op_to_val r v |> val_to_int < 0 then
          { r with stack = { r.stack with ip = get_ip r addr } }
        else next r
    | GotoIfNull (v, addr) ->
        if (op_to_val r v |> val_to_ptr).idx = 0 then
          { r with stack = { r.stack with ip = get_ip r addr } }
        else next r
    | New dest ->
        let h, ptr = Heap.alloc r.heap in
        (match dest with Register x -> r.stack.locals.(x) <- DynPtr ptr);
        next { r with heap = h }
    | Resize (arr, len) ->
        let arr = dest_to_val r arr |> val_to_ptr in
        let len = op_to_val r len |> val_to_int in
        let h = Heap.resize r.heap arr len in
        next { r with heap = h }
    | IndexSet (dest, idx, x) ->
        let arr = dest_to_val r dest |> val_to_ptr in
        let idx = op_to_val r idx |> val_to_int in
        let x = op_to_val r x |> val_to_ptr in
        let h = Heap.index_set r.heap arr idx x in
        next { r with heap = h }
    | IndexGet (dest, arr, idx) ->
        let arr = op_to_val r arr |> val_to_ptr in
        let idx = op_to_val r idx |> val_to_int in
        store r dest @@ DynPtr (Heap.index_get r.heap arr idx);
        next r
    | Load (dest, op) ->
        let ptr = op_to_val r op |> val_to_ptr in
        let x = Heap.load r.heap ptr in
        store r dest (DynNumber x);
        next r
    | Store (dest, op) ->
        let x = op_to_val r op |> val_to_int in
        let ptr = dest_to_val r dest |> val_to_ptr in
        let h = Heap.store r.heap ptr x in
        next { r with heap = h }
    | _ -> failwith "Todo"
  with e ->
    trace r;
    raise e
