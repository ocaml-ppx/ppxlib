This test checks that labeled tuple types are correctly encoded
when migrated down to our 5.2 AST

  $ cat > test.ml << EOF
  > type t = (a: int * b: int * string)
  > EOF

  $ ./id_driver.exe test.ml
  type t =
    [%ppxlib.migration.ptyp_labeled_tuple_504 :
      (('a * int) * ('b * int) * (_ * string))]

And that it is correctly decoded when migrated back up to 5.4+ ASTS:

  $ ./id_driver.exe test.ml --use-compiler-pp
  type t = (a:int * b:int * string)
