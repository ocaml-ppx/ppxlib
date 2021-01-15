Keep the error output short in order to avoid different error output between
different compiler versions in the subsequent tests

  $ export OCAML_ERROR_STYLE=short

The registered rewriters get applied when using `run_as_ppx_rewriter` as entry point

  $ cat > file.ml << EOF
  > let () = [%print_hi]
  > let () = [%print_bye]
  > EOF
  $ ocaml -ppx 'print_greetings' file.ml
  hi
  bye

The driver's `shared_args` arguments should be taken into account with the
exception of the perform check arguments.

[To be fixed]
But at the moment, it's the other way around: the check argument is taken into
account...

  $ echo "[@@@attr non_registered_attr]" > attribute_file.ml
  $ ocaml -ppx 'print_greetings -check' attribute_file.ml
  File "./attribute_file.ml", line 1, characters 4-8:
  Error: Attribute `attr' was not used
  [2]

[To be fixed]
...while some of the other `shared_args` aren't

  $ ocaml -ppx 'print_greetings -apply print_hi' file.ml
  hi
  bye

[To be fixed]
If a non-compatible file gets fed, the file name should get reported correctly,
but isn't at the moment

  $ touch no_binary_ast.ml
  $ print_greetings no_binary_ast.ml some_output
  End_of_file
  [2]

The only possible usage is [extra_args] <infile> <outfile>...

  $ print_greetings some_input 2>&1 | sed 's/Usage: .*_build/Usage: (...)_build/'
  Usage: (...)_build/default/test/driver/run_as_ppx_rewriter/print_greetings.exe [extra_args] <infile> <outfile>

[To be fixed]
...in particular the order between the flags and the input/output matters.
That should get reported nicely, but isn't at the moment

  $ touch some_output
  $ print_greetings some_input some_output -check
  End_of_file
  [2]

[To be fixed]
The only exception should be consulting help, but isn't at the moment

  $ print_greetings -help 2>&1 | sed 's/Usage: .*_build/Usage: (...)_build/'
  Usage: (...)_build/default/test/driver/run_as_ppx_rewriter/print_greetings.exe [extra_args] <infile> <outfile>

[To be fixed]
Binary AST's of any by ppxlib supported OCaml version should be supported as input;
but at the moment, only the OCaml version the driver got compiled with is supported.
At the moment, that test would break the CI on 4.06. In the future, it can be tested
as follows.

$ cat 406_binary_ast | print_magic_number
Magic number: Caml1999N022

$ print_greetings 406_binary_ast /dev/stdout | print_magic_number
(Failure "Ast_mapper: OCaml version mismatch or malformed input")
Magic number: 
