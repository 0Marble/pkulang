open Alcotest
open Pkulang

let scan_root (root : Ast.root) =
  let fn_list = ref [] in
  let map =
    Hashtbl.fold
      (fun _ (v : Ast.node) acc ->
        let name =
          match v with
          | FnDecl x ->
              fn_list := v :: !fn_list;
              Some x.name
          | CoDecl x ->
              fn_list := v :: !fn_list;
              Some x.name
          | LetStmt x -> Some x.var_name
          | Argument x -> Some x.name
          | Field x -> Some x.var_name
          | ForLoop x -> Some x.iter_var
          | IfResumeStmt x -> x.var
          | _ -> None
        in
        (match name with
        | Some name ->
            if Hashtbl.find_opt acc name |> Option.is_some then
              failwith "Error: we only support unique names for testing"
            else Hashtbl.add acc name v
        | None -> ());
        acc)
      root.all_nodes (Hashtbl.create 64)
  in
  (map, !fn_list)

let dummy_get_definition map (node : Ast.node) :
    [ `Node of Ast.node | `Builtin of string ] =
  match node with
  | DotExpr x -> `Node (Hashtbl.find map x.field)
  | VarExpr x ->
      Hashtbl.find_opt map x.name
      |> Option.map (fun n : [ `Node of Ast.node | `Builtin of string ] ->
             `Node n)
      |> Option.value ~default:(`Builtin x.name)
  | _ -> failwith "Error: not a node with definition"

let compile src =
  let root = Parser.parse_root src in
  let map, fns = scan_root root in
  let r = Codegen.codegen src fns (dummy_get_definition map) root in
  Printf.eprintf "\n";
  Array.iteri
    (fun i (cmd : Runtime.command) ->
      Printf.eprintf "%s\n"
        (Runtime.string_of_cmd ~mark:(i = r.stack.top.ip) i cmd.cmd))
    r.code;
  r

let interpret r n =
  let rec complete r n =
    if Runtime.finished r then r
    else if n = 0 then failwith "Too many steps!"
    else complete (Runtime.step r) (n - 1)
  in
  complete r n

let hello_world () =
  let src =
    {|
      fn main() void {
        println("Hello World!");
      }
      |}
  in
  let r = compile src in
  let r = interpret r 100 in
  check string "hello world" "Hello World!\n" r.stdout;
  ()

let () =
  run "Codegen" [ ("programs", [ ("hello_world", `Quick, hello_world) ]) ]
