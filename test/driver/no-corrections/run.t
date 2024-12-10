Here we will test the -no-corrections flag.

First, a bit of context on that feature:

Before the introduction of this flag, the only viable use case for
someone that wanted to use [@@deriving_inline ...] to avoid having a build
dependency on a ppx was fairly limited. They couldn't use anything but
correction based ppx-es, i.e. [@@deriving_inline] itself or ppx-es that used the
same correction style.
The way they had to go about it was to have no preprocess field declared in their
dune file, i.e. at build time no ppx were involved. They would run those ppx by
configuring a (lint (pps ...)) field instead in their dune file.

There are situations where one might want to use a set of ppx-es without having
a dependency on a subset of those and this was not possible because the driver
would error out upon finding a `[@@deriving_inline x]` node when ppx x was not
linked with the driver. That means that you had to add ppx-es used with
deriving_inline to your (preprocess (pps ...)) field, making them a build
dependency of your project and defeating the purpose of [@@deriving_inline].

The -no-correction flag allows to work around this limitation. By adding
this flag to the driver invocation (it can be done by adding the flag directly
to the (preprocess (pps ...)) field), [@@deriving_inline] and other such
attributes are properly ignored.

Now with the test.

To properly test this we define three ppx-es:
- ppx_deriving_x which is a regular deriver
- ppx_deriving_y which is another regular deriver but one that we'll only use
with [@@deriving_inline]
- ppx_gen_stuff which is a custom ppx that use the same mechanism as
[@@deriving_inline] and that should also be ignored when -no-corrections is
passed

We also manually build two different drivers:
- driver_all.exe which is a driver with all three ppx-es linked, that
corresponds to the driver dune would generate for the (lint (pps ...)) field
- driver_deriving_x which is a driver with only ppx_deriving_x linked, that
corresponds to the driver dune would generate for the (preprocess (pps ...))
field

Let's consider the following source file:

  $ cat > test.ml << EOF
  > type t [@@deriving x]
  > type t2
  > [@@deriving_inline y]
  > [@@@deriving.end]
  > type t3 [@@gen_stuff]
  > [@@@deriving.end]
  > EOF

If we run our driver for preprocessing, it will produce errors for the unknown
deriver y in the .ppx-corrected along with unused attribute errors for [@@gen_stuff]
and the last [@@@deriving.end] that comes with it.

  $ ./driver_deriving_x.exe -impl test.ml -check -diff-cmd diff
  [%%ocaml.error "Attribute `gen_stuff' was not used"]
  [%%ocaml.error "Attribute `deriving.end' was not used"]
  type t[@@deriving x]
  include struct let _ = fun (_ : t) -> ()
                 let x = 2
                 let _ = x end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t2[@@deriving_inline y]
  [@@@deriving.end ]
  type t3[@@gen_stuff ]
  [@@@deriving.end ]
  3a4,6
  > let _ = fun (_ : t2) -> ()
  > [%%ocaml.error
  >   "Ppxlib.Deriving: 'y' is not a supported type deriving generator"]
  [1]

Now if we run it with -no-corrections, there should be no .ppx-corrected file
and associated diff and the [@@@deriving.end] attribute error should go away.
We unfortunately cannot prevent the unused [@@gen_stuff] attribute as the driver
has no knowledge of it but we consider this to be an okay limitation, especially
since the unused attributes check is disabled by default.

  $ ./driver_deriving_x.exe -impl test.ml -check -no-corrections -diff-cmd diff
  [%%ocaml.error "Attribute `gen_stuff' was not used"]
  type t[@@deriving x]
  include struct let _ = fun (_ : t) -> ()
                 let x = 2
                 let _ = x end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t2[@@deriving_inline y]
  [@@@deriving.end ]
  type t3[@@gen_stuff ]
  [@@@deriving.end ]

Now if we run our driver with the whole set of ppx-es, everything should go as
expected and all corrections will be correctly generated

  $ ./driver_all.exe -impl test.ml -check -diff-cmd diff
  type t[@@deriving x]
  include struct let _ = fun (_ : t) -> ()
                 let x = 2
                 let _ = x end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t2[@@deriving_inline y]
  [@@@deriving.end ]
  type t3[@@gen_stuff ]
  [@@@deriving.end ]
  3a4,6
  > let _ = fun (_ : t2) -> ()
  > let y = 3
  > let _ = y
  5a9
  > let stuff = 4
  [1]

For reference and to document the behaviour of the -no-corrections flag in this
situation, running the same driver with the flag will generate no corrections and
no attribute warnings since this time, it knows about the [@@gen_stuff] attribute
and explicitly skips it.

  $ ./driver_all.exe -impl test.ml -check -no-corrections -diff-cmd diff
  type t[@@deriving x]
  include struct let _ = fun (_ : t) -> ()
                 let x = 2
                 let _ = x end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t2[@@deriving_inline y]
  [@@@deriving.end ]
  type t3[@@gen_stuff ]
  [@@@deriving.end ]
