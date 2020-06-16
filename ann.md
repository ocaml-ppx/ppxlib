ocaml-migrate-parsetree 2.0.0
=============================

We are preparing a 2.0.0 release of [ocaml-migrate-parsetree][omp].
This new major release will simplify the project to its core purpose,
which is to provide migration functions between the various versions
of the parsetree types. Everything else, such as the driver or the
per-version copy of various modules from the compiler distribution
will go away. This will have an impact for projects that are currently
using these functionalities and this post describes what we think such
projects should do and how we plan to help.

The motiviation for this change is to simplify ocaml-migrate-aprsetree
and make it easier to maintain. The core functionality it provides is
valuable and worth maintaining, however right now the project is way
too big and difficult to maintain. Going through compiler upgrades has
been unpleasant and difficult. And because of this, bugs of the
segfault kind have accidentally been introduced and detected much
later.

Once we have droppped everything that doesn't fit in the purpose of
the project, what will remain will be pretty simple, coherent and easy
to maintain.

### What is the replacement?

At the moment, ocaml-migrate-parsetree offers one way of writing ppx
rewriters. Following the simplification, ocaml-migrate-parsetree
itself will no longer be enough.

We recommend that ppx rewriters using the ocaml-migrate-parsetree API
switch to [ppxlib][ppxlib].  The purpose of ppxlib is to provide a
good API for ppx rewriters with in particular a simple and good
composition model, so it seems like a natural choice. Since ppxlib
will take a more central place in the ecosystem, we are currently
addressing the more controversial aspects of this library. For
instance, we are dropping the dependency on Base.

### Organising the transition

The following ticket is the meta issue tracking the preparation of the
2.0.0 release:

https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/101

To make this transition smoother, we are planning to help with the
porting effort. We have prepared [a list of ppx rewriters to port to
ppxlib][toport].  This list is not exhaustive and in particular
excludes ppx rewriters that have no reverse dependencies in opam in
order to keep this work maneageable for us.  In any case, help porting
projects to ppxlib would be very much appreciated.

If you would like to help porting a project, please mention on the
corresponding ticket from the list that you are looking at porting
this project so that we know someone is working on it. If you would
like to port a project that is not on this list, you should feel free
to open a new ticket and mention that you are porting this project. In
this case, please make sure to add the label `port-to-ppxlib` to the
issue.

Once we have successfully converted a few projects, we will proceed
with the release of ocaml-migrate-parsetree 2.0.0 and a compatible
ppxlib release.  At this point, most exsiting reverse dependencies of
ocaml-migrate-parsetree will be marked as incompatible with
ocaml-migrate-parsetree 2.0.0 and we will continue porting existing
projects to ppxlib to gradually make the world compatible with
ocaml-migrate-parsetree 2.0.0.

### Consequences for other ppx libraries

#### ppx_deriving

The master of [ppx_deriving][pd] is now using ppxlib, so there
shouldn't be much to do to make ppx\_deriving compatible with
ocaml-migrate-parsetree 2.0.0.

#### ppx_tools_versioned

[ppx_tools_versioned][ptv] will no longer have a reason to exist in
this new state of the world and so will stop being updated.

[omp]: https://github.com/ocaml-ppx/ocaml-migrate-parsetree/
[ppxlib]: https://github.com/ocaml-ppx/ppxlib/
[toport]: https://github.com/ocaml-ppx/ppxlib/issues?q=is%3Aopen+is%3Aissue+label%3Aport-to-ppxlib
[ptv]: https://github.com/ocaml-ppx/ppx_tools_versioned
[pd]: https://github.com/ocaml-ppx/ppx_deriving
