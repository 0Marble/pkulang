type location = Register of int | Argument of int | Void

exception InvalidLocation of location
exception WriteToVoid

type frame = {
  ptr : Value.value;
  start : int;
  args : Value.value array;
  locals : Value.value array;
  caller : frame option;
  ip : int;
  result : location;
}

let string_of_location loc =
  match loc with
  | Register x -> "r" ^ string_of_int x
  | Argument x -> "a" ^ string_of_int x
  | Void -> "void"

let location_valid f loc =
  let valid =
    match loc with
    | Register x -> x >= 0 && x < Array.length f.locals
    | Argument x -> x >= 0 && x < Array.length f.args
    | Void -> true
  in
  if not valid then raise (InvalidLocation loc) else ()

let store f loc v =
  location_valid f loc;
  match loc with
  | Register x -> f.locals.(x) <- v
  | Argument x -> f.args.(x) <- v
  | Void -> ()

let load f loc =
  location_valid f loc;
  match loc with
  | Register x -> f.locals.(x)
  | Argument x -> f.args.(x)
  | Void -> raise WriteToVoid

let get_active f =
  let active_in_array arr =
    Array.fold_left
      (fun active v ->
        match v with Value.Number _ -> active | Pointer ptr -> ptr :: active)
      [] arr
  in
  let locals = active_in_array f.locals in
  let args = active_in_array f.args in
  List.append locals args
