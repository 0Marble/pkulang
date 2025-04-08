## About

## Running

To open REPL:
```
dune exec ./bin/pkulang.exe 
```
To interpret multiple lines in REPL, you can add a `\` at the end of the line.
I recommend to install `rlwrap` program to enable the use of arrow keys inside the REPL: `rlwrap dune exec ./bin/pkulang.exe`

To interpret a text file:
```
dune exec ./bin/pkulang.exe filename
```
Here you do not need to add backslashes.

## Testing 

Run all tests:
```
dune test
```
For testing we use [Alcotest](https://opam.ocaml.org/packages/alcotest/).
All tests are placed inside the `tests/` directory. 
I recommend to add tests for every new feature, it helps a lot in the long run.

## Conventions

I recommend we use common conventions for our code to keep it cleaner and simpler to work with.

1. Use formatter. `.ocamlformat` already describes the formatting rules for our project. We want to be consistent so that when someone changes a file someone else was working on, we do not get a bunch of git changes due to formatting.
2. Do testing. Projects such as programming languages have a lot of moving parts and features, it is very easy to break something old by adding something new, and there are a lot of edge cases. That is why we should use automated testing for the project. Don't be lazy!
3. Git. When you are working on a feature, create a new branch for it, named `yourName-featureName`. When you are creating a commit, name the commit `feature-name: message`, for example, `aleksei-basic-runtime` branch, or commit `REPL: reading files and multiline repl`. Try to keep the commits orthogonal to each-other, i.e. only commit one feature at a time. 

## Structure

Let us examine the parts of the interpreter.
All implementations should be in `lib/`.

1. Tokenizer: `lib/tokenizer.ml`. Splits the input text into "tokens", basically determines what string of characters is a keyword, what is an identifier and so on.
2. Parser: `lib/parser.ml`. Takes in a list of tokens, creates a tree representation of the program called the AST. 
3. AST description: `lib/ast.ml`. This file describes the AST structure, produced by the parser. The root of the tree is `Root` node. Note the children of each node are all of type `node` which is a bit misleading: not all nodes can actually be a child of some specific node. For example, for `BinOp` node, only other "expression" like nodes may be children. The actual rules are defined in the parser implementation.
4. Locations: `lib/location.ml`. This file describes the `location` type, which tells us where tokens/AST nodes/... come from in the source file. It is intended for logging purposes: for example, if we encounter some sort of type mismatch, we can use `loc` field of the AST node containing the error to print the error nicely. I recommend to always preserve some sort of `location` information in the parts of the interpreter, so that any error can be reported.
5. Errors: `lib/error.ml`. When an error actually occurs, we can call `Error.fail_at_spot` that throws an exception and reports the `location` of the error. You can look at the example usage in the parser.
6. Trie: `lib/trie.ml`. A trie data structure (also known as prefix tree), it is used inside the tokenizer.
