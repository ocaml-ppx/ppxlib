The -dune-optional-output flag is meant for dune to be able
to use ppx internally without having a build dependency on ppxlib
or any ppx.

When enabled, it should not write to the output if it can absolutely
tell no transformation occured.

We have a driver with a single context free rule to expand [%iam1] extension

Let us consider the following file:

  $ cat > foo.ml << EOF
  > let x = 1
  > let y = 2
  > EOF

If we call the driver with the -dune-optional-output flag, it should not write a file:

  $ ./context_free_only_driver.exe -impl -dune-optional-output -o foo.pp.ml foo.ml
  $ ls foo.*
  foo.ml

We can see that it did not write foo.pp.ml

Now if we actually use the extension:

  $ cat > bar.ml << EOF
  > let x = [%iam1]
  > let y = 2
  > EOF

It should actually detect the transformation and therefore write the output file:

  $ ./context_free_only_driver.exe -impl -dune-optional-output -o bar.pp.ml bar.ml
  $ ls bar.*
  bar.ml
  bar.pp.ml

Now we have another driver that has the same context free rule but also another
transformation with an "impl", i.e. a rule to rewrite the whole AST unconditionally.
This rule does not rewrite anything and is just the identity rewriter.
We cannot tell without actually comparing the ASTs if any rewriting happened so in
that case we always write to the output.

  $ cat > baz.ml << EOF
  > let x = 1
  > let y = 2
  > EOF
  $ ./driver_with_impl.exe -impl -dune-optional-output -o baz.pp.ml baz.ml
  $ ls baz.*
  baz.ml
  baz.pp.ml
