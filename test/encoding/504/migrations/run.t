This test checks that labeled tuple types are correctly encoded
when migrated down to our 5.2 AST

  $ cat > type.ml << EOF
  > type t = (a: int * b: int * string)
  > EOF

  $ ./id_driver.exe type.ml
  type t =
    [%ppxlib.migration.ptyp_labeled_tuple_504 :
      (('a * int) * ('b * int) * (_ * string))]

And that it is correctly decoded when migrated back up to 5.4+ ASTS:

  $ ./id_driver.exe type.ml --use-compiler-pp
  type t = (a:int * b:int * string)

Same for expressions:

  $ cat > expression.ml << EOF
  > let x = (~a:0, ~b:1, "abc")
  > EOF

  $ ./id_driver.exe expression.ml
  let x =
    [%ppxlib.migration.pexp_labeled_tuple_504
      (((`Some `a), 0), ((`Some `b), 1), (`None, "abc"))]

  $ ./id_driver.exe expression.ml --use-compiler-pp
  let x = (~a:0, ~b:1, "abc")
