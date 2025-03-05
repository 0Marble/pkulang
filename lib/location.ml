type location = Spot of int | Range of (int * int)

let union l1 l2 =
  let a, b = match l1 with Spot x -> (x, x + 1) | Range (a, b) -> (a, b) in
  let a', b' = match l2 with Spot x -> (x, x + 1) | Range (a, b) -> (a, b) in
  Range (min a a', max b b')

let loc_to_str src loc =
  let in_location idx =
    match loc with
    | Spot x -> idx = x
    | Range (a, b) -> idx >= min a b && idx < max a b
  in

  let acc = ref "" in
  let line_num = ref 0 in
  let had_mark = ref false in

  let build (idx, line, underline) c : int * string * string =
    if c = '\n' then (
      line_num := !line_num + 1;
      let () =
        if !had_mark then
          acc :=
            Printf.sprintf "%s%5d|%s\n     |%s\n" !acc !line_num line underline
        else ()
      in
      had_mark := false;
      (idx + 1, "", ""))
    else
      let l = Printf.sprintf "%s%c" line c in
      let u =
        if in_location idx then (
          had_mark := true;
          Printf.sprintf "%s^" underline)
        else Printf.sprintf "%s " underline
      in
      (idx + 1, l, u)
  in
  let _, _, _ = String.fold_left build (0, "", "") (src ^ "\n") in
  !acc
