## rev-deps.sh

You can use this script to fetch ppxlib's rev-deps and clone them locally to test them
against the latest changes in ppxlib and eventually send patch to them.
Eventually this will make it into a duniverse feature but in the meantime it can prove useful
to reproduce the steps we went through to send patches ahead of the 0.14.0 release.

This script is used to assemble a dune-workspace containing all you need to build ppxlib and a
co-installable subset of its reverse dependencies together. It's split into three steps.

### Getting the rev deps locally

The first step is to actually compute the list of rev-deps we want and clone them. For this you can
run the following, from the root of the repo:
- `./dev/rev-deps.sh pull` if you want the non-JaneStreet rev deps.
- `./dev/rev-deps.sh pull janestreet` if you want only the JaneStreet packages.

JS and non-JS ppx-es are handled separately because they tend to be non-coinstallable. Furthermore
we want to send patches to the JS ppx-es on top of the latest release branch instead of the master
branch for other ppx-es. When cloned, the JS rev deps will be checked out to the latest release
branch.

The script will clone them in a `dunireverse` folder at the repo's root.

### Installing their dependencies

You can either install them through opam by running `./dev/rev-deps.sh install-deps opam` or
get the sources locally in the dune-workspace (prefered) by running:
`./dev/rev-deps.sh install-deps duniverse`. 

The opam installation step is very naive and probably won't stand the test of time but might be
better when the rev deps have too strict constraints. The duniverse approach is prefered as it will
work even if some of the rev-deps depend on each other but it requires the rev deps to all be
coninstallable or the duniverse solver will fail.

To get the duniverse tool, simply clone the duniverse repo at
[https://github.com/ocamllabs/duniverse](https://github.com/ocamllabs/duniverse), build it
and add it to your path. When initially writing this, I used duniverse's master, ie at the time
`47faea8522e8e44ba0dbd04403425970aa5c2662`.

### Building them

Since some of the cloned repos might contain more than just the ppx-es we're interested in,
running just `dune build` might not do it. You can use `./dev/rev-deps.sh build` which will build
exactly what you need, ie ppxlib and all the rev-deps packages you cloned.

No black magic here, it's just running `dune build -p ppxlib,...` where `...` is the list of
rev-deps packages. The `-p` is also helpful to avoid annoying warnings getting in the way.
