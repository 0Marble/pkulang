module IntSet = Set.Make (Int)

type operand =
  | Number of int
  | Register of int
  | AtRegister of int
  | Argument of int
  | AtArgument of int

type destination = Register of int | AtRegister of int
type jump_target = Static of int | Dynamic of int

type command_kind =
  | Halt
  | Nop
  | Trap
  | Add of (destination * operand * operand)
  | Sub of (destination * operand * operand)
  | Assign of (destination * operand)
  | Goto of jump_target
  | GotoIfZero of (operand * jump_target)
  | GotoIfNeg of (operand * jump_target)
  | Call of (destination * operand array * jump_target)
  | Ret of operand
  | Alloca of int
  | Builtin of (destination * operand array * string)

type command = { cmd : command_kind; loc : Location.location }

type stack_frame = {
  start : int;
  args : int array;
  locals : int array;
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
  let heap_val r x =
    try Heap.string_of_obj r.heap (Heap.get_obj r.heap { idx = x })
    with Heap.InvalidPtr _ -> "?"
  in
  match op with
  | Number x -> string_of_int x
  | Register x ->
      let local = try string_of_int @@ f.locals.(x) with _ -> "?" in
      let global = try heap_val r f.locals.(x) with _ -> "?" in
      Printf.sprintf "r%d=%s(%s)" x local global
  | AtRegister x ->
      let local = try string_of_int @@ f.locals.(x) with _ -> "?" in
      let global = try heap_val r f.locals.(x) with _ -> "?" in
      Printf.sprintf "@r%d=%s(%s)" x local global
  | Argument x ->
      let local = try string_of_int @@ f.args.(x) with _ -> "?" in
      let global = try heap_val r f.args.(x) with _ -> "?" in
      Printf.sprintf "a%d=%s(%s)" x local global
  | AtArgument x ->
      let local = try string_of_int @@ f.args.(x) with _ -> "?" in
      let global = try heap_val r f.args.(x) with _ -> "?" in
      Printf.sprintf "@a%d=%s(%s)" x local global

let string_of_cmd ?(ctx = None) idx c =
  let sov =
    match ctx with
    | None -> fun s _ -> s
    | Some (r, f) -> fun _ x -> string_of_operand_dyn r f x
  in

  let string_of_tgt a =
    match a with
    | Static x -> string_of_int x
    | Dynamic x -> sov ("r" ^ string_of_int x) (Register x)
  in
  let string_of_operand o =
    match o with
    | Number x -> string_of_int x
    | Register x -> sov ("r" ^ string_of_int x) (Register x)
    | AtRegister x -> sov ("@r" ^ string_of_int x) (AtRegister x)
    | Argument x -> sov ("a" ^ string_of_int x) (Argument x)
    | AtArgument x -> sov ("@a" ^ string_of_int x) (AtArgument x)
  in
  let string_of_dest a =
    match a with
    | AtRegister x -> "@r" ^ string_of_int x
    | Register x -> "r" ^ string_of_int x
  in
  let s =
    match c with
    | Halt -> "Halt"
    | Nop -> "Nop"
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
    | Call (dest, args, fn) ->
        Printf.sprintf "Call %s %s %s" (string_of_dest dest)
          (Array.fold_left
             (fun acc op -> acc ^ string_of_operand op ^ ",")
             "[" args
          ^ "]")
          (string_of_tgt fn)
    | Ret op -> "Ret " ^ string_of_operand op
    | Alloca amt -> "Alloca " ^ string_of_int amt
    | Builtin (dest, args, fn) ->
        Printf.sprintf "Builtin.%s %s %s" fn (string_of_dest dest)
          (Array.fold_left
             (fun acc op -> acc ^ string_of_operand op ^ ",")
             "[" args
          ^ "]")
  in
  Printf.sprintf "[%5d] %s" idx s

let create source code main =
  let size = Int.shift_left 1 16 in
  {
    heap = { memory = Array.make size (Heap.HeapNumber 0); free = 0 };
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

let trace r =
  prerr_string "================TRACE START================\n";

  prerr_string "====== Section 1: Last Instructions =======\n";
  let s =
    Queue.fold
      (fun acc idx -> acc ^ string_of_cmd idx r.code.(idx).cmd ^ "\n")
      "" r.last_instrs
  in
  prerr_string s;

  prerr_string "========= Section 2: Stack Trace ==========\n";
  let rec stack_trace r f =
    let acc =
      match f.caller with Some caller -> stack_trace r caller | None -> "\n"
    in
    Printf.sprintf "%s\n%s"
      (string_of_cmd ~ctx:(Some (r, f)) f.ip r.code.(f.ip).cmd)
      acc
  in
  prerr_string @@ stack_trace r r.stack;

  prerr_string "====== Section 3: Current Stack Frame =====\n";
  prerr_string "==== Section 3.1: Current Function Code ===\n";
  let rec function_code r ip visited =
    try
      let cmd = r.code.(ip) in
      match cmd.cmd with
      | Ret _ | Halt -> (IntSet.add ip visited, [ ip ])
      | Goto tgt | GotoIfNeg (_, tgt) | GotoIfZero (_, tgt) ->
          let v = IntSet.add ip visited in
          let v, block =
            match tgt with
            | Static next_ip ->
                if IntSet.mem next_ip v then (v, [])
                else function_code r next_ip v
            | Dynamic _ -> (v, [])
          in
          let v, l = function_code r (ip + 1) (IntSet.add ip v) in
          (v, (ip :: l) @ block)
      | _ ->
          let v, l = function_code r (ip + 1) (IntSet.add ip visited) in
          (v, ip :: l)
    with Invalid_argument _ -> (visited, [])
  in
  let _, cmds = function_code r r.stack.start IntSet.empty in
  List.iter (fun ip -> prerr_endline @@ string_of_cmd ip r.code.(ip).cmd)
  @@ cmds;

  prerr_string "===== Section 3.2: Current Registers ======\n";
  Array.iteri
    (fun i _ -> prerr_endline @@ string_of_operand_dyn r r.stack (Argument i))
    r.stack.args;
  Array.iteri
    (fun i _ -> prerr_endline @@ string_of_operand_dyn r r.stack (Register i))
    r.stack.locals;

  prerr_endline "=================TRACE END=================\n"

let finished r =
  let isp = r.stack.ip in
  let cmd = r.code.(isp) in
  if cmd.cmd = Halt then true else false

let step r =
  let next r = { r with stack = { r.stack with ip = r.stack.ip + 1 } } in
  let get_int r v =
    match v with
    | Number x -> x
    | Register x -> r.stack.locals.(x)
    | AtRegister x -> (
        let x = r.stack.locals.(x) in
        match r.heap.memory.(x) with HeapNumber x -> x | _ -> x)
    | Argument x -> r.stack.args.(x)
    | AtArgument x -> (
        let x = r.stack.args.(x) in
        match r.heap.memory.(x) with HeapNumber x -> x | _ -> x)
  in
  let store_int r addr x =
    match addr with
    | AtRegister a ->
        let a = r.stack.locals.(a) in
        r.heap.memory.(a) <- HeapNumber x;
        r
    | Register a ->
        r.stack.locals.(a) <- x;
        r
  in
  let get_ip addr =
    match addr with Static x -> x | Dynamic x -> r.stack.locals.(x)
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
  | Trap ->
      prerr_string "Trapped!\n";
      trace r;
      failwith "Trap"
  | Alloca amt ->
      next
        {
          r with
          stack =
            {
              r.stack with
              locals = Array.append r.stack.locals (Array.make amt 0);
            };
        }
  | Add (dest, lhs, rhs) ->
      next @@ store_int r dest (get_int r lhs + get_int r rhs)
  | Sub (dest, lhs, rhs) ->
      next @@ store_int r dest (get_int r lhs - get_int r rhs)
  | Assign (dest, src) -> next @@ store_int r dest @@ get_int r src
  | Call (dest, args, fn) ->
      let callee =
        {
          start = get_ip fn;
          locals = [||];
          args = Array.map (fun x -> get_int r x) args;
          caller = Some r.stack;
          return = dest;
          ip = get_ip fn;
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
      next @@ store_int r' callee.return (get_int r v)
  | Builtin (_, args, "print") ->
      let s, _ =
        Array.fold_left
          (fun (acc, i) op ->
            let s =
              match op with
              | Number x -> string_of_int x
              | Register x -> string_of_int r.stack.locals.(x)
              | AtRegister x ->
                  Heap.string_of_obj r.heap r.heap.memory.(r.stack.locals.(x))
              | Argument x -> string_of_int r.stack.args.(x)
              | AtArgument x ->
                  Heap.string_of_obj r.heap r.heap.memory.(r.stack.args.(x))
            in
            if i + 1 = Array.length args then (acc ^ s, i + 1)
            else (acc ^ s ^ ", ", i + 1))
          ("", 0) args
      in
      print_endline s;
      next { r with stdout = r.stdout ^ s ^ "\n" }
  | Builtin (_, _, _) -> failwith "Unknown builtin"
  | GotoIfZero (v, addr) ->
      if get_int r v = 0 then
        { r with stack = { r.stack with ip = get_ip addr } }
      else next r
  | GotoIfNeg (v, addr) ->
      if get_int r v < 0 then
        { r with stack = { r.stack with ip = get_ip addr } }
      else next r
  | _ -> failwith "Todo"
