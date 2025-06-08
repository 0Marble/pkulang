type location = Register of int | Argument of int | Void

exception InvalidLocation of location
exception WriteToVoid
exception StackUnderflow

type frame = {
  start : int;
  args : Value.value array;
  locals : Value.value array;
  caller : frame option;
  result : location;
  stack : stack;
  ip : int;
}

and stack = {
  top : frame;
  bot : frame;
  yielder : (stack * int) option;
  ptr : Value.value;
}

let string_of_location loc =
  match loc with
  | Register x -> "r" ^ string_of_int x
  | Argument x -> "a" ^ string_of_int x
  | Void -> "void"

let location_valid (f : stack) loc =
  let f = f.top in
  let valid =
    match loc with
    | Register x -> x >= 0 && x < Array.length f.locals
    | Argument x -> x >= 0 && x < Array.length f.args
    | Void -> true
  in
  if not valid then raise (InvalidLocation loc) else ()

let store (f : stack) loc v =
  location_valid f loc;
  let f = f.top in
  match loc with
  | Register x -> f.locals.(x) <- v
  | Argument x -> f.args.(x) <- v
  | Void -> ()

let load (f : stack) loc =
  location_valid f loc;
  let f = f.top in
  match loc with
  | Register x -> f.locals.(x)
  | Argument x -> f.args.(x)
  | Void -> raise WriteToVoid

let get_active (f : stack) =
  let active_in_array arr =
    Array.fold_left
      (fun active v ->
        match v with Value.Number _ -> active | Pointer ptr -> ptr :: active)
      [] arr
  in
  let rec active_in_frame (f : frame) =
    let locals = active_in_array f.locals in
    let args = active_in_array f.args in
    match f.caller with
    | Some f -> List.concat [ locals; args; active_in_frame f ]
    | None -> List.append locals args
  in
  active_in_frame f.top

let get_ip s = s.top.ip
let inc_ip s = { s with top = { s.top with ip = s.top.ip + 1 } }

let alloca s amt =
  {
    s with
    top =
      {
        s.top with
        locals = Array.append s.top.locals (Array.make amt (Value.Number 0));
      };
  }

let call s dest args fn =
  let caller = { s.top with result = dest } in
  let callee =
    {
      start = fn;
      locals = [||];
      args;
      caller = Some caller;
      result = Void;
      ip = fn;
      stack = s;
    }
  in
  { s with top = callee }

let ret (s : stack) v =
  let callee = s.top in
  match callee.caller with
  | Some caller ->
      let s = { s with top = { caller with ip = caller.ip + 1 } } in
      store s caller.result v;
      s
  | None -> (
      match s.yielder with
      | Some (y, ip) -> { y with top = { y.top with ip } }
      | None -> raise StackUnderflow)

let goto s ip = { s with top = { s.top with ip } }
