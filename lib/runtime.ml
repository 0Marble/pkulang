module StringMap = Map.Make (String)

type operand = Number of int | Local of int | Global of int
type address = Local of int | Global of int
type target = Static of int | Dynamic of int

type obj =
  | HeapNumber of int
  | HeapArray of int array
  | HeapObject of { fields : int StringMap.t }

type heap = { memory : obj array; free : int }

let rec string_of_obj h o =
  match o with
  | HeapNumber x -> string_of_int x
  | HeapArray a ->
      Array.fold_left
        (fun acc o -> acc ^ string_of_obj h h.memory.(o) ^ ",")
        "[" a
      ^ "]"
  | HeapObject o ->
      StringMap.fold
        (fun f o acc ->
          Printf.sprintf "%s%s: %s," acc f (string_of_obj h h.memory.(o)))
        o.fields "{"
      ^ "}"

type command_kind =
  | Halt
  | Nop
  | Trap
  | Add of (address * operand * operand)
  | Sub of (address * operand * operand)
  | Assign of (address * operand)
  | Goto of target
  | GotoIfZero of (operand * target)
  | Call of (address * operand array * target)
  | Ret of operand
  | Alloca of int
  | Builtin of (address * operand array * string)

let string_of_cmd idx c =
  let string_of_tgt a =
    match a with
    | Static x -> string_of_int x
    | Dynamic x -> "r" ^ string_of_int x
  in
  let string_of_operand o =
    match o with
    | Number x -> string_of_int x
    | Local x -> "r" ^ string_of_int x
    | Global x -> "@r" ^ string_of_int x
  in
  let string_of_addr a =
    match a with
    | Local x -> "r" ^ string_of_int x
    | Global x -> "@r" ^ string_of_int x
  in
  let s =
    match c with
    | Halt -> "Halt"
    | Nop -> "Nop"
    | Trap -> "Trap"
    | Add (dest, lhs, rhs) ->
        Printf.sprintf "Add %s %s %s" (string_of_addr dest)
          (string_of_operand lhs) (string_of_operand rhs)
    | Sub (dest, lhs, rhs) ->
        Printf.sprintf "Sub %s %s %s" (string_of_addr dest)
          (string_of_operand lhs) (string_of_operand rhs)
    | Assign (dest, v) ->
        Printf.sprintf "Assign %s %s" (string_of_addr dest)
          (string_of_operand v)
    | Goto tgt -> "Goto " ^ string_of_tgt tgt
    | GotoIfZero (op, tgt) ->
        Printf.sprintf "GotoIfZero %s %s" (string_of_operand op)
          (string_of_tgt tgt)
    | Call (dest, args, fn) ->
        Printf.sprintf "Call %s %s %s" (string_of_addr dest)
          (Array.fold_left
             (fun acc op -> acc ^ string_of_operand op ^ ",")
             "[" args
          ^ "]")
          (string_of_tgt fn)
    | Ret op -> "Ret " ^ string_of_operand op
    | Alloca amt -> "Alloca " ^ string_of_int amt
    | Builtin (dest, args, fn) ->
        Printf.sprintf "Builtin.%s %s %s" fn (string_of_addr dest)
          (Array.fold_left
             (fun acc op -> acc ^ string_of_operand op ^ ",")
             "[" args
          ^ "]")
  in
  Printf.sprintf "[%5d] %s" idx s

type command = { cmd : command_kind; loc : Location.location }

type stack_frame = {
  locals : int array;
  caller : stack_frame option;
  ip : int;
  return : address;
}

and runtime = {
  stack : stack_frame;
  heap : heap;
  code : command array;
  stdin : string;
  stdout : string;
  source : string;
}

let create source code main =
  let size = Int.shift_left 1 16 in
  {
    heap = { memory = Array.make size (HeapNumber 0); free = 0 };
    stdout = "";
    stdin = "";
    stack = { caller = None; ip = main; locals = [||]; return = Local 0 };
    code;
    source;
  }

let finished r =
  let isp = r.stack.ip in
  let cmd = r.code.(isp) in
  if cmd.cmd = Halt then true else false

let step r =
  let next r = { r with stack = { r.stack with ip = r.stack.ip + 1 } } in
  let get_int v =
    match v with
    | Number x -> x
    | Local x -> r.stack.locals.(x)
    | Global x -> (
        let x = r.stack.locals.(x) in
        match r.heap.memory.(x) with HeapNumber x -> x | _ -> x)
  in
  let store_int r addr x =
    match addr with
    | Local a ->
        r.stack.locals.(a) <- x;
        r
    | Global a ->
        let a = r.stack.locals.(a) in
        r.heap.memory.(a) <- HeapNumber x;
        r
  in
  let get_ip addr =
    match addr with Static x -> x | Dynamic x -> r.stack.locals.(x)
  in

  let cmd = r.code.(r.stack.ip) in
  prerr_string @@ string_of_cmd r.stack.ip cmd.cmd;
  prerr_newline ();
  match cmd.cmd with
  | Halt -> r
  | Nop -> next r
  | Trap -> failwith "Trapped!"
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
  | Add (dest, lhs, rhs) -> next @@ store_int r dest (get_int lhs + get_int rhs)
  | Sub (dest, lhs, rhs) -> next @@ store_int r dest (get_int lhs - get_int rhs)
  | Assign (dest, src) -> next @@ store_int r dest @@ get_int src
  | Call (dest, args, fn) ->
      let f = { r.stack with return = dest } in
      let f' =
        {
          locals = Array.map (fun x -> get_int x) args;
          caller = Some f;
          return = Local 0;
          ip = get_ip fn;
        }
      in
      { r with stack = f' }
  | Ret v ->
      let f =
        match r.stack.caller with
        | Some f -> f
        | None ->
            Error.fail_at_spot r.source "Stack underflow" cmd.loc Error.Eval
      in
      next @@ store_int { r with stack = f } f.return (get_int v)
  | Builtin (_, args, "print_int") ->
      let s =
        if Array.length args <> 1 then
          Error.fail_at_spot r.source "Expected 1 argument" cmd.loc Error.Eval
        else string_of_int (get_int args.(0))
      in
      print_endline s;
      next { r with stdout = r.stdout ^ s ^ "\n" }
  | GotoIfZero (v, addr) ->
      if get_int v = 0 then { r with stack = { r.stack with ip = get_ip addr } }
      else next r
  | _ -> failwith "Todo"
