type error_with_location = { msg : string; src : string; loc : int }

exception ErrorAtLocation of error_with_location

let fail_at_spot msg src loc = raise (ErrorAtLocation { msg; src; loc })

let print_error err =
  let get_line src loc =
    let len = String.length src in
    if len <= loc then failwith "Invalid location"
    else
      let rec get_line_impl src loc idx cnt start =
        if idx >= len then (String.sub src start (len - start), loc - start, cnt)
        else
          let c = src.[idx] in
          match c with
          | '\n' ->
              if loc <= idx then
                (String.sub src start (idx - start), loc - start, cnt)
              else get_line_impl src loc (idx + 1) (cnt + 1) idx
          | _ -> get_line_impl src loc (idx + 1) cnt start
      in
      get_line_impl src loc 0 0 0
  in
  let line, offt, line_num = get_line err.src err.loc in
  Printf.eprintf "[line %d] %s\n  %s\n%s^\n" (line_num + 1) err.msg line
    (String.make (offt + 2) ' ');
  flush Out_channel.stderr
