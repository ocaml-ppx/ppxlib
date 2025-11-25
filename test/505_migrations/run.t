OCaml 5.5 Migrations
--------------------

1. Pexp_struct_item AST node

We first check that the changes to various let-bindings are suitably migrated.
[let module M = T in], [let open M in] and [let exception C in] are now all
represented with the same AST node.

We have a custom driver that will force migration of the AST down to 5.2 and back to
the compiler's version and print it as source code using the compiler's printer,
regardless of ppxlib's internal AST version.

If we run the driver on the following source file:

  $ cat > test.ml << EOF
  > module T = struct let x = 1 end
  > let f =
  >   let exception E of int in
  >   let module X = T in
  >   let open X in
  >   x
  > EOF

it should successfully roundtrip to 5.2 and print the source code unchanged:

  $ ./driver.exe test.ml --use-compiler-pp
  module T = struct let x = 1 end
  let f = let exception E of int  in let module X = T in let open X in x

2. Ptyp_extension

A new feature of OCaml 5.5 are external types (e.g. [type t = external "t"]).

  $ cat > test.ml << EOF
  > type t = external "t"
  > EOF

For now, we do not support these and raise an error. In the future we may wish
to encode this feature into attributes and this test, along with this comment,
will need updated.

  $ ./driver.exe test.ml --use-compiler-pp
  File "test.ml", line 1:
  Error: External types are not supported.
  [1]
