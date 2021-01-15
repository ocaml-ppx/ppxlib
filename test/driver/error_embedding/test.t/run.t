Keep the error output short in order to avoid different error output between
different compiler versions in the subsequent tests

  $ export OCAML_ERROR_STYLE=short

With the `-embed-errors` options, if a PPX raises, the first such exception
is caught and packed into an output AST

  $ echo "let _ = [%raise]" > impl.ml
  $ raiser -embed-errors impl.ml
  [%%ocaml.error "Raising inside the rewriter"]

The same is true when using the `-as-ppx` mode (note that the error is reported
by ocaml itself)

  $ ocaml -ppx 'raiser -as-ppx' impl.ml
  File "./impl.ml", line 1, characters 8-16:
  Error: Raising inside the rewriter
  [2]

Also exceptions raised in a preprocessor get embedded into an AST(while the
error from the preprocessor's stderr also gets reported on the driver's stderr)

  $ touch file.ml
  $ raiser -embed-errors -pp pp file.ml | sed "s/> '.*'/> tmpfile/"
  Fatal error: exception Location.Error(_)
  [%%ocaml.error
    "Error while running external preprocessor\nCommand line: pp 'file.ml' > tmpfile\n"]

[To be fixed]
Also `unknown version` errors get embedded into an AST when using the main
standalone. But at the moment they get recognized as syntax errors
instead of unknown version errors. That makes the following test variant
under different versions and would break the CI at the moment.

$ raiser -embed-errors -intf unknown_version_binary_ast
File "unknown_version_binary_ast", line 1, characters 0-13:
Alert deprecated: ISO-Latin1 characters in identifiers
[%%ocaml.error "Syntax error"]


[To be fixed]
... but the `-as-ppx` standalone raises them (although at the moment they get
recognized as 'expected a binary AST as input' errors instead of unknown version errors)

  $ raiser -as-ppx unknown_version_binary_ast output
  File "unknown_version_binary_ast", line 1:
  Error: Expected a binary AST as input
  [1]

Similar for 'input doesn't exist' errors: they get embedded by the main standalone...

  $ raiser -embed-errors -impl non_existing_file
  [%%ocaml.error "I/O error: non_existing_file: No such file or directory"]

[To be fixed]
... but not by the `-as-ppx` standalone, which should report the missing file
name correctly when raising, but doesn't at the moment

  $ raiser -as-ppx non_existing_file output
  File "_none_", line 1:
  Error: I/O error: non_existing_file: No such file or directory
  [1]
