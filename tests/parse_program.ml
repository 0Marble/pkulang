open Alcotest
open Pkulang

let run_parser s = Ast.Root (Parser.parse_root s) |> Ast.node_to_str

let range () =
  let src =
    {|
    fn range(a: int, b: int) void {
      let i: int = a;
      while(i < b) {
        yield(i);
        i += 1;
      }
    }

    fn main() void {
      for (i : range(0, 10)) print_int(i);
    }
  |}
  in
  check string "range"
    "(root (stmts (fn range (arg a (type int)) (arg b (type int)) (type void) \
     (block (let i (type int) (var a)) (while (bin Lt (var i) (var b)) (block \
     (yield (var i)) (bin Unimplemented (var i) (num 1)))))) (fn main (type \
     void) (block (for i (call (var range) (num 0) (num 10)) (call (var \
     print_int) (var i)))))))"
    (run_parser src)

let linked_list () =
  let src =
    {|
  struct LinkedList {
    val: int,
    next: LinkedList,

    fn iterator() void {
      yield(self.val);
      if (self.next) self.next.iterator();
    }
  }

  fn main() void {
    let l: LinkedList = null;
    for (i : range(0, 10)) {
      l = new LinkedList{ val: i, next: l };
    } 
    let it: CoroutineType = create(l.iterator);
    assert(resume(it) == 9);
    assert(resume(it) == 8);
    for (x : it) {
      print_int(x);
    }
  }
  |}
  in
  check string "linked_list"
    "(root (stmts (struct LinkedList (field val (type int) _) (field next \
     (type LinkedList) _) (fn iterator (type void) (block (yield (dot (var \
     self) val)) (if (dot (var self) next) (call (dot (dot (var self) next) \
     iterator)) _)))) (fn main (type void) (block (let l (type LinkedList) \
     (null)) (for i (call (var range) (num 0) (num 10)) (block (bin Assign \
     (var l) (new (type LinkedList) (fields (field_literal val (var i)) \
     (field_literal next (var l))))))) (let it (type CoroutineType) (create \
     (dot (var l) iterator))) (call (var assert) (bin Eq (resume (var it) _) \
     (num 9))) (call (var assert) (bin Eq (resume (var it) _) (num 8))) (for x \
     (var it) (block (call (var print_int) (var x))))))))"
    (run_parser src)

let tree_iter () =
  let src =
    {|
  struct Tree {
    val: int,
    left: Tree = null,
    right: Tree = null,
    
    fn iterate() void {
      if (self.left) self.left.iterate();
      yield(self.val);
      if (self.right) self.right.iterate();
    }
  }
  |}
  in
  check string "tree_iter"
    "(root (stmts (struct Tree (field val (type int) _) (field left (type \
     Tree) (null)) (field right (type Tree) (null)) (fn iterate (type void) \
     (block (if (dot (var self) left) (call (dot (dot (var self) left) \
     iterate)) _) (yield (dot (var self) val)) (if (dot (var self) right) \
     (call (dot (dot (var self) right) iterate)) _))))))"
    (run_parser src)

let () =
  run "Parser: example programs"
    [
      ( "examples",
        [
          ("linked_list", `Quick, linked_list);
          ("range", `Quick, range);
          ("tree_iter", `Quick, tree_iter);
        ] );
    ]
