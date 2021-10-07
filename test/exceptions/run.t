In this test we verify the behavior of ppxlib with regard to rewriters error generations.

There is mainly three way for rewriters to handle errors, from best to worst practice:

1. Putting an "error extension node" in the AST. Merlin can notify all errors, while the compiler only notify the first.

  $ echo "let _ = [%gen_ext_node] + [%gen_ext_node]" > impl.ml
  $ ocaml -ppx "./exceptions.exe --as-ppx" impl.ml
  File "./impl.ml", line 1, characters 8-23:
  1 | let _ = [%gen_ext_node] + [%gen_ext_node]
              ^^^^^^^^^^^^^^^
  Error: An error message in an extension node
  [2]

2. Raising a located error. Currently, the driver catches it and replace the whole AST with an error extension node

  $ echo "let _ = [%gen_raise_located_error] + [%gen_raise_located_error]" > impl.ml
  $ ocaml -ppx "./exceptions.exe --as-ppx" impl.ml
  File "./impl.ml", line 1, characters 8-34:
  1 | let _ = [%gen_raise_located_error] + [%gen_raise_located_error]
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: A raised located error
  [2]

3. Raising an exception. The exception is not catched by the driver.

  $ echo "let _ = [%gen_raise_exc] + [%gen_raise_exc]" > impl.ml
  $ ./exceptions.exe impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]
