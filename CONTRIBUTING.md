This repository contains core libraries and tools used to develop ppx
rewriters. The code was originally developped and is still maintained
and used by [Jane Street][js].

This repository is not the first piece of open source software
released by Jane Street, however it is the first to be entirely
developped on github. We are hopping that opening the development of
this repository will help collaboration with other open source users.

We welcome contributions and we will be happy to add contributors,
given that they are motivated to help maintain and grow the
project. However, given how important this code is to the functioning
of Jane Street, we do require that at least one Jane Street developper
reads every pull request that modifies the source code.

Additionally, all contributors must sign our [Contributor License
Agreement][CLA], except for very simple contributions.

### Developing patches

We ask that patches changing the code respect the overall coding
style. In particular, the code should be indented using
[ocp-indent][ocpi]. Additionally the test suite should pass on the
contributor's machine before a patch is submitted for review.

Note that in addition to the normal dependencies, you need to install
[cinaps][cinaps] in order to modify the code. This is because some
part of the code are auto-generated and committed in the repository.

So before submitting a PR, make sure to check all the following
points:

- all the modified code is correctly indented according to ocp-indent
- `make` succeeds
- `make test` succeeds

### Submitting patches and code review

Once a patch is ready according to the criterion stated in the
previous section, it should be submited via the github website. When
submitting a pull request, we ask that you tick the `Allow edits from
maintainers` box. This will allow us to push new commits to your
PR. We ask this for two reasons; first it is much simpler and quicker
to fix typos or do simple improvements directly rather than go back
and forth through the web interface.

The second reason is that when one accepts a patch on their project,
it is natural to want to rewrite some part of it to match the overall
style of the project, enforce some invariant the contributor might not
be aware of, or even just make sure that it matches the personal
tastes of the maintainer, since they are going to be the one
maintaining the code moving forward. In any case, when part of a PR is
rewritten by a maintainer, it doesn't mean that the original code was
badly written.

[js]:     https://opensource.janestreet.com/
[CLA]:    https://janestreet.github.io/contributing.html
[ocpi]:   https://github.com/OCamlPro/ocp-indent
[cinaps]: https://github.com/janestreet/cinaps
