# Non-continuous `trunk`-support for Ppxlib

On this branch, we provide a Ppxlib version, which can be compiled with
OCaml's `trunk` branch. Whenever you want to compile a project that
(transitively) depends on PPXs with `trunk`, please pin Ppxlib to
this branch.

The branch is maintained manually. So each time the compiler adds a new
syntax feature, first we need to bring this branch back in sync with
`trunk` and then you'll need to re-pin your Ppxlib. Please, open an
issue or a PR, if this branch doesn't compile with `trunk`. Also open
an issue, if there's a feature/bug-fix on Ppxlib's `main` branch that
you'd like to have here, so that we rebase.

## `trunk`-support vs `trunk`-feature support

Ppxlib supporting `trunk` can be interpreted in two different ways.

### User perspective on the difference

One notion of `trunk`-support is that a project depending on Ppxlib can
be compiled with `trunk` _under the assumption_ that the project
doesn't use the new syntax features in `trunk`. That's what this branch
provides.

The other notion of `trunk`-support should actually be called
`trunk`-feature support: you can use the new `trunk` syntax features
in a project which depends on Ppxlib and that you compile with `trunk`.
It's not unlikely that you'll get a Ppxlib runtime error (i.e. project
compile time error), if you try to use this branch in that situation.

### Ppxlib behavior perspective on the difference

The Ppxlib behavior in the situation of `trunk` support is: The parsetree
version exposed by Ppxlib is still the one from the latest stable compiler
and all PPXs are defined against that one. However, Ppxlib has hold of
`trunk`'s parsetree version and knows how to migrate between that
parsetree version and the parsetree version of the latest stable compiler.
You can find those parsetrees and parsertree migration modules in Ppxlib's
`Astlib` library. So, when compiling a file with `trunk`, the Ppxlib driver
will parse the file into the `trunk` parsetree, migrate down to the latest
stable parsetree, do the PPX expansions and migrate back to `trunk`'s
parsetree. The downward migration will error, if the parsetree contains a
feature that isn't well-defined in the latest stable parsetree version.

The behavior in the case of `trunk`-feature support would be different.
In that case, Ppxlib would have to expose the `trunk` parsetree, so that
the PPXs would be defined against that one. So, Ppxlib would have to bump
its internal parsetree to the `trunk` parsetree. Bumping the Ppxlib parsetree
always means potentially breaking PPXs. When bumping the Ppxlib parsetree
to the parsetree of an about-to-be-released stable compiler version, we
create a work-space with all opam PPXs and send patch PRs to the ones we
break. However, for `trunk` we wouldn't do such an effort.

## Incompatibility with stable compiler versions

This branch is incompatible with stable compiler versions. The reason
for that is that Ppxlib has different input modes. One possibility is
to receive a marchalled parsetree. Marshalled parsetrees contain meta-data
reflecting the parsetree version they correspond to. That meta-data is
called the parsetree magic number. The magic number needs to be unique
among supported parsetree versions to have meaning. The compiler bumps
the magic number at the very end of a release cycle. Before, the `trunk`
magic number coincides with the magic number of the last stable parsetree.
Therefore it's impossible for Ppxlib to support both the latest stable
parsetree and the compiler parsetree at the same time.
