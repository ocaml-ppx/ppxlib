In this test we verify the behavior of ppxlib with regard to rewriters
error generations.

There is mainly three way for rewriters to handle errors, from best to
worst practice:

1. Putting an "error extension node" in the AST. In this test, the AST
is rewritten to contain two of these nodes.

  $ echo "let _ = [%gen_ext_node] + [%gen_ext_node]" > impl.ml
  $ ./exceptions.exe impl.ml
  let _ =
    ([%ocaml.error "An error message in an extension node"]) +
      ([%ocaml.error "An error message in an extension node"])

(Note that Merlin will notify both errors, while the compiler only
notifies the first.)

2. Raising a located error. This is what the ppx does in this
example. By default, the exception is not caught, so no AST is
produced.

  $ echo "let x = 1+1. \nlet _ = [%gen_raise_located_error]" > impl.ml
  $ export OCAML_ERROR_STYLE=short
  $ ./exceptions.exe impl.ml
  File "impl.ml", line 2, characters 9-35:
  Error: A raised located error
  [1]

When the argument `-embed-errors` is added, the exception is caught
and the whole AST is replaced with a single error extension node. The
first line `let x = 1+1.` is thus not present in the AST, and no error
can be reported about it.

  $ ./exceptions.exe -embed-errors impl.ml
  [%%ocaml.error "A raised located error"]

3. Raising an exception. The exception is not catched by the driver.

  $ echo "let _ = [%gen_raise_exc] + [%gen_raise_exc]" > impl.ml
  $ ./exceptions.exe impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]
  $ ./exceptions.exe -embed-errors impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]
