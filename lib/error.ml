type error_with_location = {
  msg : string;
  src : string;
  loc : Location.location;
}

exception ErrorAtLocation of error_with_location

let fail_at_spot msg src loc =
  let err = { msg; src; loc } in
  let loc_str = Location.loc_to_str src loc in
  Printf.eprintf "ERROR: %s\n\n%s\n" msg loc_str;
  flush stderr;
  raise (ErrorAtLocation err)
