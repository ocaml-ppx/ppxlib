In this test we verify the behavior of ppxlib with regard to rewriters
error generations. We test both extenders, derivers and whole file
rewriters.

There is mainly three way for ppxs to handle errors, from best to
worst practice:

1. Putting an "error extension node" in the AST. In this test, the AST
is rewritten to contain two of these nodes.

 In the case of extenders

  $ echo "let _ = [%gen_ext_node] + [%gen_ext_node]" > impl.ml
  $ ./extender.exe impl.ml
  let _ =
    ([%ocaml.error "An error message in an extension node"]) +
      ([%ocaml.error "An error message in an extension node"])

 In the case of derivers

  $ echo "type a = int [@@deriving deriver_extension_node]" > impl.ml
  $ ./deriver.exe impl.ml
  type a = int[@@deriving deriver_extension_node]
  include
    struct
      let _ = fun (_ : a) -> ()
      [%%ocaml.error "An error message in an extension node"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

 In the case of whole file transformations:

  $ echo "let x = 1+1. " > impl.ml
  $ ./whole_file_extension_point.exe impl.ml
  [%%ocaml.error "An error message in an extension node"]

(Note that Merlin will notify all errors, while the compiler only
notifies the first.)

2. Raising a located error. In these tests, such an error is raised
during the rewritting of the AST. By default, the exception is not
caught, so no AST is produced.

 In the case of extenders:

  $ echo "let x = 1+1. " > impl.ml
  $ echo "let _ = [%gen_raise_located_error]" >> impl.ml
  $ export OCAML_ERROR_STYLE=short
  $ ./extender.exe impl.ml
  File "impl.ml", line 2, characters 8-34:
  Error: A raised located error
  [1]

 In the case of derivers

  $ echo "type a = int" > impl.ml
  $ echo "type b = int [@@deriving deriver_located_error]" >> impl.ml
  $ ./deriver.exe impl.ml
  File "impl.ml", line 2, characters 0-47:
  Error: A raised located error
  [1]

 In the case of whole file transformations:

  $ echo "let x = 1+1. " > impl.ml
  $ ./whole_file_located_error.exe impl.ml
  File "impl.ml", line 1, characters 0-12:
  Error: (raise_exc): A located error in a whole file transform
  (ppxlib): IDE / source code analysis features aren't available anymore due to PPX error reporting.
  [1]

When the argument `-embed-errors` is added, the exception is caught
and the whole AST is replaced with a single error extension node. The
first line `let x = 1+1.` is thus not present in the AST, and no error
can be reported about it.

 In the case of extenders:

  $ echo "let x = 1+1. " > impl.ml
  $ echo "let _ = [%gen_raise_located_error]" >> impl.ml
  $ ./extender.exe -embed-errors impl.ml
  [%%ocaml.error "A raised located error"]

 In the case of derivers

  $ echo "let x = 1+1. " > impl.ml
  $ echo "type a = int" > impl.ml
  $ echo "type b = int [@@deriving deriver_located_error]" >> impl.ml
  $ ./deriver.exe -embed-errors impl.ml
  [%%ocaml.error "A raised located error"]

 In the case of whole file transformations:

  $ echo "let x = 1+1. " > impl.ml
  $ ./whole_file_located_error.exe -embed-errors impl.ml
  [%%ocaml.error
    "(raise_exc): A located error in a whole file transform\n(ppxlib): IDE / source code analysis features aren't available anymore due to PPX error reporting."]

3. Raising an exception. The exception is caught by the driver, and
another exception is raised, containing the initial error message as
well as an explanation of where it comes from.

 In the case of extensions:

  $ echo "let _ = [%gen_raise_exc] + [%gen_raise_exc]" > impl.ml
  $ ./extender.exe impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]
  $ ./extender.exe -embed-errors impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]

 In the case of derivers

  $ echo "type a = int" > impl.ml
  $ echo "type b = int [@@deriving deriver_raised_exception]" >> impl.ml
  $ ./deriver.exe -embed-errors impl.ml
  Fatal error: exception (Failure "A raised exception")
  [2]

 In the case of whole file transformations:

  $ ./whole_file_exception.exe impl.ml
  Fatal error: exception (raise_exc): (Failure "An exception in a whole file transform")
  (ppxlib): the location of the error is undefined and IDE / source code analysis features aren't available anymore, because raise_exc has raised.
  
  [2]
  $ ./whole_file_exception.exe -embed-errors impl.ml
  Fatal error: exception (raise_exc): (Failure "An exception in a whole file transform")
  (ppxlib): the location of the error is undefined and IDE / source code analysis features aren't available anymore, because raise_exc has raised.
  
  [2]

Finally we add some tests for the other phases of rewriting such as
linting, preprocess, and implementation. The behavior is the same as
the main whole file transform phase, so we only test raising a located
error.

  $ ./linter.exe impl.ml
  File "impl.ml", line 1, characters 0-12:
  Error: (raise_in_linter): A located error in a linter
  (ppxlib): IDE / source code analysis features aren't available anymore due to PPX error reporting.
  [1]
  $ ./preprocess.exe impl.ml
  File "impl.ml", line 1, characters 0-12:
  Error: (raise_in_preprocess): A located error in a preprocess
  (ppxlib): IDE / source code analysis features aren't available anymore due to PPX error reporting.
  [1]
  $ ./instrument_before.exe impl.ml
  File "impl.ml", line 1, characters 0-12:
  Error: (raise_in_instrument): A located error in a preprocess
  (ppxlib): IDE / source code analysis features aren't available anymore due to PPX error reporting.
  [1]
  $ ./instrument_after.exe impl.ml
  File "impl.ml", line 1, characters 0-12:
  Error: (raise_in_instrument): A located error in a preprocess
  (ppxlib): IDE / source code analysis features aren't available anymore due to PPX error reporting.
  [1]
