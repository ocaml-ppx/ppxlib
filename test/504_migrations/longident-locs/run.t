We have a custom driver that will force migration of the AST down to 5.2 and back to
the compiler's version and print it as source code using the compiler's printer,
regardless of ppxlib's internal AST version.

If we run the driver on the following source file:

  $ cat > test.ml << EOF
  > let () = NonExistingModule.foo () 
  > EOF

then the non-existing module should have a sensible error location.

  $ ocamlc -ppx "./driver.exe --as-ppx" test.ml test.ml.pp
  File "_none_", line 1:
  Error: Unbound module NonExistingModule
  [2]

Another longident usage:

  $ cat > test.ml << EOF
  > let t = { ThisModule.age = 43 }
  > EOF

  $ ocamlc -ppx "./driver.exe --as-ppx" test.ml test.ml.pp
  File "_none_", line 1:
  Error: Unbound module ThisModule
  [2]
 
