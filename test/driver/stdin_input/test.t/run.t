The driver can read from stdin. Both when the input is source code...


  $ identity_driver -impl - << EOF
  > let a = 1
  > EOF
  let a = 1

[to be fixed]
...and when the input is a binary AST. At the moment the test would break
the CI since it's variant under different versions. Once, ppxlib supports
all supported ocaml versions in binary AST inputs, it can be tested as
follows:

$ cat binary_ast | identity_driver -impl -
let b = 2
