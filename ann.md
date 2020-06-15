ocaml-migrate-parsetree 2.0.0
=============================

We are preparing a 2.0.0 release of
[ocaml-migrate-parsetree][omp]. This new major release will simplify
the project to its core purpose, dropping all the functionalities that
don't belong in it. The following ticket is the meta issue tracking
the preparation of the 2.0.0 release:

https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/101

ocaml-migrate-parsetree 2.0.0 will no longer provide a driver
functionality, nor it will snapshot various modules from the compiler
distribution for each major release of OCaml. This latter point is
what makes ocaml-migrate-parsetree particularly difficult to maintain.

The main motiviation for this change is to simplify the project and
make it easier to maintain. Incidentally, this will also make our
[on-going effort of simplifying the ppx world][future-of-ppx] easier
since it will reduce the number of APIs available to write ppx
rewriters.

Indeed, following this release ocaml-migrate-parsetree itself will no
longer be enough to write ppx rewriters. We expect that ppx rewriters
currently using ocaml-migrate-parsetree will switch to
[ppxlib][ppxlib].  The purpose of ppxlib is to provide a good API for
ppx rewriters with in particular a simple and good composition model,
so it seems like a natural choice. Since ppxlib will take a more
central place in the ecosystem, we are currently addressing the more
controversial aspects of this library such as the dependency on Base.

To make this transition smoother, we are planning to port ourselves
most ppx rewriters that are currently using ocaml-migrate-parsetree to
ppxlib. We have prepared [a list of ppx rewriters to port to
ppxlib][toport].  This list is not exhaustive and excludes ppx
rewriters that have no reverse dependencies in opam in order to keep
this work maneageable for us.  In any case, help porting projects to
ppxlib would be very much appreciated. If you are willing to help,
please don't hesitate to comment here on one of the tickets from the
list.

Once we have successfully converted a reasonable number of projects,
we will proceed with the release of ocaml-migrate-parsetree 2.0.0 and
ppxlib 1.0.0, where ppxlib 1.0.0 will be compatible with
ocaml-migrate-parsetree 2.0.0. At this point, most exsiting reverse
dependencies of ocaml-migrate-parsetree will be marked as incompatible
with ocaml-migrate-parsetree 2.0.0.

We will then continue submitting patches to port existing projects to
ppxlib until we have gone through the list.

Versioning story?
-----------------

The versioning story is a bit different between the current
ocaml-migrate-parsetree and ppxlib.

Let's take a ppx rewriter `ppx_foo` that uses
ocaml-migrate-parsetree. `ppx_foo` selects one version of the AST,
most often the latest available at the time of writing the code. Let's
assume `ppx_foo` is written against the 4.07 AST.

When OCaml 4.08 is released and after ocaml-migrate-parsetree has been
updated to support the 4.08 AST, existing releases of `ppx_foo` are
immediately compatible with OCaml 4.08. However, users of `ppx_foo`
are not able to use newer language features such as `let+` until
`ppx_foo` has bumped the version of the AST it is based on to 4.08 and
makde a new release.

So essentially, we get a certain form of continuity but ultimately all
ppx rewriters must still regularly update themselves.

Now let's take the example of another ppx rewriter `ppx_bar` that uses
ppxlib. ppxlib choses one version the AST for its implementation and
API. Let's assume ppxlib is using the 4.07 AST. After OCaml 4.08 has
been released and ocaml-migrate-parsetree has been released, existing
releases of `ppx_bar` are immediately compatible with OCaml
4.08. However, users of `ppx_bar` are not able to use the newer
language features such as `let+` until ppxlib has bumped the version
of the AST it is based on to 4.08 and made a new release. Depending on
the actual changes to the AST, `ppx_bar` might be broken by this new
release and might need to be updated and released as well.

The trade-off between the two approaches is as follow: as long as
users are not eager to use the new langauge feature, the
ocaml-migrate-parsetree method allows one to release a ppx rewriter
and pretty much never update it. For the ppxlib approach to work well,
the various ppx rewriters need to be kept up to date and possibly be
re-released after each compiler release.

The new [ppx project][ppx] we envisioned in [our original
plan][future-of-ppx] aims to solve all problems at once. i.e. a ppx
rewriter is released once and pretty much never needs to be
re-released again, without preventing the use of new language
features. The trade-off is that the mechanics of the system are a bit
more complex to reason about.

For the time being, we are going to focus on the making the ppxlib
versioning story be as smooth as possible. And if it turns out to work
well in praticte, we will diverge from our original plan in order to
end up with a system that is technically simpler.

Making upgrade of ppxlib smooth
-------------------------------

Bumping the verions of the AST selected by ppxlib is a breaking
change. It might not break all users of ppxlib, but some will surely
break. The technical work required to update rewriters that break is
relatively small and easy. In fact, we have quite a lot of experience
with this at Jane Street and coping with ppxlib upgrade is pretty
easy; a single person sit down in front of their computer, upgrade
ppxlib, goes through the build errors and fix them one by one. The
whole operation rarely takes more than a couple of hours.

In the open source world, the situation is a bit more complicated as
the various ppx rewriters are developed and maintained by independent
groups of people. Going through an upgrade of ppxlib means pushing
some work on all these groups of poeple and waiting for them to do it.

However, at this point the world is actually a bit more uniform since
most ppx rewriters are now using Dune. Given that Dune is composable,
it becomes realistic for one person to do all the upgrading work and
send PRs to the various ppx rewriters. Ultimately, the various
maintainers still have to review, merge the PRs and re-release their
ppx rewriters. So we don't know if using Dune to upgrade all ppx
rewriters at once will be a good enough story to make writing and
maintaining ppx rewriters painless.

However, because it leads to a world that is technically simpler and
easier to reason about, we want to give this idea a proper
try. @NathanReb is going to try this process for the upcoming 4.10
upgrade of ppxlib. Depending on how this goes, we will decide whether
to continue with our original plan or diverge and follow a plan based
on the ocaml-migrate-parsetree technology rather than the dynamic AST
we have been working on.

[omp]: https://github.com/ocaml-ppx/ocaml-migrate-parsetree/
[ppxlib]: https://github.com/ocaml-ppx/ppxlib/
[toport]: https://github.com/ocaml-ppx/ppxlib/issues?q=is%3Aopen+is%3Aissue+label%3Aport-to-ppxlib
[future-of-ppx]: https://discuss.ocaml.org/t/the-future-of-ppx/3766
[ppx]: https://github.com/ocaml-ppx/ppx/
[dr]: https://github.com/ocamllabs/dune-release
