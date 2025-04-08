type address = { idx : int }
type value = Number of int | Address of address | Null

module StringMap = Map.Make (String)

type obj =
  | Value of value
  | Object of { addr : address; fields : value StringMap.t }

type command_kind =
  | Nop
  | Add
  | Sub
  | Push of value
  | Pop of address
  | Goto of value
  | IfZeroGoto of value
  | SetField of string
  | GetField of string
  | Alloc
  | BuiltinCall of string

type command = { kind : command_kind; loc : Location.location }

type runtime = {
  isp : int;
  regs : value array;
  code : command array;
  stack : value list;
  memory : obj array;
  source : string;
}

let create s =
  let size = Int.shift_left 1 16 in
  {
    isp = 0;
    regs = Array.make 16 (Number 0);
    code = Array.make size { kind = Nop; loc = Location.Spot 0 };
    stack = [];
    memory = Array.make size (Value (Number 0));
    source = s;
  }

let rec string_of_val r v loc =
  match v with
  | Number x -> string_of_int x
  | Null -> "null"
  | Address a ->
      (let o = r.memory.(a.idx) in
       match o with
       | Value v -> string_of_val r v loc
       | Object o ->
           StringMap.fold
             (fun fname v acc ->
               Printf.sprintf "%s%s: %s," acc fname (string_of_val r v loc))
             o.fields "{")
      ^ "}"

let step r =
  let cmd = r.code.(r.isp) in

  let next r = { r with isp = r.isp + 1 } in
  let push r v = { r with stack = v :: r.stack } in
  let pop r =
    match r.stack with
    | v :: vs -> ({ r with stack = vs }, v)
    | _ -> Error.fail_at_spot r.source "Stack underflow" cmd.loc Error.Eval
  in
  let pop_int r =
    let r, a = pop r in
    match a with
    | Number x -> (r, x)
    | _ -> Error.fail_at_spot r.source "Expected a number" cmd.loc Error.Eval
  in
  let goto r v =
    match v with
    | Number x -> { r with isp = x }
    | Address a -> (
        let o = r.memory.(a.idx) in
        match o with
        | Value (Number x) -> { r with isp = x }
        | _ -> Error.fail_at_spot r.source "Invalid address" cmd.loc Error.Eval)
    | _ -> Error.fail_at_spot r.source "Invalid address" cmd.loc Error.Eval
  in

  match cmd.kind with
  | Nop -> next r
  | Add ->
      let r, a = pop_int r in
      let r, b = pop_int r in
      push r (Number (a + b)) |> next
  | Sub ->
      let r, a = pop_int r in
      let r, b = pop_int r in
      push r (Number (a - b)) |> next
  | Push v -> push r v
  | Pop addr ->
      let r, v = pop r in
      r.memory.(addr.idx) <- Value v;
      next r
  | Goto addr -> goto r addr
  | IfZeroGoto addr ->
      let r, x = pop_int r in
      if x = 0 then goto r addr else next r
  | SetField _ -> failwith "Todo"
  | GetField _ -> failwith "Todo"
  | Alloc -> failwith "Todo"
  | BuiltinCall "print" ->
      let r, v = pop r in
      string_of_val r v cmd.loc |> print_string;
      r
  | BuiltinCall _ ->
      Error.fail_at_spot r.source "Unknown builtin function" cmd.loc Error.Eval
