When the ppxlib driver reports an error by itself, source quotation should work
properly.

We start off by explicitly setting the error reporting style to contextual to
ensure source quotation is enabled:

  $ export OCAML_ERROR_STYLE=contextual

Here we have a driver compiled with a single rule that will raise a located
exception for every "[%raise]" extension point.

We need an input file:

  $ cat > file.ml << EOF
  > let x = [%raise]
  > EOF

When running the driver on this file, it should report the error and show
the relevant quoted source:

  $ ./raising_driver.exe -impl file.ml
  File "file.ml", line 1, characters 8-16:
  1 | let x = [%raise]
              ^^^^^^^^
  Error: An exception, raise be!
  [1]
