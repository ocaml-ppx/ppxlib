Binary AST's of any by ppxlib supported OCaml version are supported.
The version is preserved.

  $ cat 406_binary_ast | ../print_magic_number.exe
  Magic number: Caml1999N022

  $ ../identity_standalone.exe --intf 406_binary_ast -o transformed --dump-ast
  File "_none_", line 1:
  Error: Internal error: invalid [@@ocaml.ppx.context { load_path }] pair syntax
  [1]

  $ ../print_magic_number.exe < transformed
  cannot open transformed: No such file
  [2]
