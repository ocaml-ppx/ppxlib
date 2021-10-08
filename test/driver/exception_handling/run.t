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

2. Raising a located error. In these tests, such an error is raised
during the rewritting of the AST. By default, the exception is not
caught, so no AST is produced.

 In the case of extensions:

  $ echo "let x = 1+1. " > impl.ml
  $ echo "let _ = [%gen_raise_located_error]" >> impl.ml
  $ export OCAML_ERROR_STYLE=short
  $ ./exceptions.exe impl.ml
  File "impl.ml", line 2, characters 8-34:
  Error: A raised located error
  [1]

 In the case of whole file transformations:

  $ ./whole_file_located_error.exe impl.ml
  File "impl.ml", line 1, characters 3-7:
  Error: A located error in a whole file transform
  [1]

When the argument `-embed-errors` is added, the exception is caught
and the whole AST is replaced with a single error extension node. The
first line `let x = 1+1.` is thus not present in the AST, and no error
can be reported about it.

 In the case of extensions:

  $ ./exceptions.exe -embed-errors impl.ml
  [%%ocaml.error "A raised located error"]

 In the case of whole file transformations:

  $ ./whole_file_located_error.exe -embed-errors impl.ml
  [%%ocaml.error "A located error in a whole file transform"]

3. Raising an exception. The exception is not caught by the driver.

 In the case of extensions:

  $ echo "let _ = [%gen_raise_exc] + [%gen_raise_exc]" > impl.ml
  $ ./exceptions.exe impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]
  $ ./exceptions.exe -embed-errors impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]

 In the case of whole file transformations:

  $ ./whole_file_exception.exe impl.ml
  Fatal error: exception (Failure "An exception in a whole file transform")
  [2]
  $ ./whole_file_exception.exe -embed-errors impl.ml
  Fatal error: exception (Failure "An exception in a whole file transform")
  [2]
