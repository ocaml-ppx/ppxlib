We have a driver with 4 derivers linked in:
- zero_do_warn
- one_no_warn
- two_do_warn
- alias_warn

----------------------------------------

Let's consider the following ocaml source file using the zero_do_warn deriver

  $ cat > zero_do_warn.ml << EOF
  > type t = int [@@deriving zero_do_warn]
  > EOF

Zero_do_warn is registered with unused_code_warning set to true meaning it allows
the driver not to silence unused code and unused module warnings if the
-unused-code-warning flag is set to true.

Let's call the driver with -unused-code-warnings=false:

  $ ./driver.exe -unused-code-warnings=false -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include
    struct
      [@@@ocaml.warning "-60"]
      module Zero = struct type t =
                             | T0  end
      let zero = Zero.T0
      let _ = zero
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

The generated code is wrapped in an include struct ... end to disable unused module
warnings, as expected. The derived value zero is followed by a let _ = zero to
disable unused value warning.

Now if we use -unused-code-warnings=true:

  $ ./driver.exe -unused-code-warnings=true -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct module Zero = struct type t =
                                        | T0  end
                 let zero = Zero.T0 end[@@ocaml.doc "@inline"][@@merlin.hide ]

Here the warning silencing was disabled as it was both allowed by the driver
invocation and the deriver itself. No include wrapping, no warning disabled, no
let _.

Note that this also applies to .mli files.

Consider:

  $ cat > zero_do_warn.mli << EOF
  > type t = int [@@deriving zero_do_warn]
  > EOF

and compare the result of both driver invocations:

  $ ./driver.exe -unused-code-warnings=false -intf zero_do_warn.mli
  type t = int[@@deriving zero_do_warn]
  include
    sig
      [@@@ocaml.warning "-32-60"]
      module Zero : sig type t end
      val zero : Zero.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=true -intf zero_do_warn.mli
  type t = int[@@deriving zero_do_warn]
  include sig module Zero : sig type t end val zero : Zero.t end[@@ocaml.doc
                                                                  "@inline"]
  [@@merlin.hide ]

-----------------------------------------------

There is another value possible for the -unused-code-warnings flag: "force".
This allows the warnings to be enabled even if the deriver does not allow it. In
this example though, using `force` or `true` results in the same output, since
the deriver `zero_do_warn` already allows the warning to be enabled.

  $ ./driver.exe -unused-code-warnings=force -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct module Zero = struct type t =
                                        | T0  end
                 let zero = Zero.T0 end[@@ocaml.doc "@inline"][@@merlin.hide ]

We'll see below other examples where the `force` flag is actually useful.

-----------------------------------------------

As a side note, to disable warnings for the generated values, it is possible to
resort to using unused warning attributes (warning -32) rather than the `let _ =
zero` construct. See below:

  $ ./driver.exe -unused-code-warnings=false -impl zero_do_warn.ml \
  >   -deriving-disable-w32-method=attribute
  type t = int[@@deriving zero_do_warn]
  include struct [@@@ocaml.warning "-32"]
                 let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include
    struct
      [@@@ocaml.warning "-32-60"]
      module Zero = struct type t =
                             | T0  end
      let zero = Zero.T0
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

-----------------------------------------------

Let's consider the following ocaml source file using the one_no_warn deriver

  $ cat > one_no_warn.ml << EOF
  > type t = int [@@deriving one_no_warn]
  > EOF

One_no_warn is registered with unused_code_warning set to false, meaning the driver
should always disable warnings for the generated code, even when the value of
the -unused-code-warning is set to true. The following driver invocations have the
same output:

  $ ./driver.exe -unused-code-warnings=false -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
      let _ = one
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=true -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : t) -> ()
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
      let _ = one
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Same goes for .mli files:

  $ cat > one_no_warn.mli << EOF
  > type t = int [@@deriving one_no_warn]
  > EOF

  $ ./driver.exe -unused-code-warnings=false -intf one_no_warn.mli
  type t = int[@@deriving one_no_warn]
  include
    sig
      [@@@ocaml.warning "-32-60"]
      module One : sig type 'a t end
      val one : Zero.t One.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=true -intf one_no_warn.mli
  type t = int[@@deriving one_no_warn]
  include
    sig
      [@@@ocaml.warning "-32-60"]
      module One : sig type 'a t end
      val one : Zero.t One.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

When using a deriving that does not allow the warning to be enabled (such as
`one_no_warn` here), it is still possible to force it from the user side. That's
what the `force` argument for the driver flag is for. See below:

  $ ./driver.exe -unused-code-warnings=force -impl one_no_warn.ml
  type t = int[@@deriving one_no_warn]
  include
    struct
      let _ = fun (_ : t) -> ()
      module One = struct type 'a t =
                            | T1 of 'a  end
      let one = One.T1 zero
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Same goes for .mli files:

  $ ./driver.exe -unused-code-warnings=force -intf one_no_warn.mli
  type t = int[@@deriving one_no_warn]
  include sig module One : sig type 'a t end val one : Zero.t One.t end
  [@@ocaml.doc "@inline"][@@merlin.hide ]

-------------------------------------------------

The alias_warn deriver is in fact an alias for two derivers:
- alias_do_warn, which is registered with unused_code_warnings=true
and derives a single `unit_one : unit` value
- alias_no_warn, which is registered with unused_code_warnings=false
and derives a single `unit_two : unit` value

For the following code:

  $ cat > alias_warn.ml << EOF
  > type t = int [@@deriving alias_warn]
  > EOF

We expect that the driver will do the right thing and disable the warning only for
unit_one:

  $ ./driver.exe -unused-code-warnings=true -impl alias_warn.ml
  type t = int[@@deriving alias_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct let unit_one = () end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include struct let unit_two = unit_one
                 let _ = unit_two end[@@ocaml.doc "@inline"][@@merlin.hide ]

As expected, there is a let _ = unit_two but nothing for unit_one. If we turn off
the unused-code-warnings flag, there will be one for both:

  $ ./driver.exe -unused-code-warnings=false -impl alias_warn.ml
  type t = int[@@deriving alias_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct let unit_one = ()
                 let _ = unit_one end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include struct let unit_two = unit_one
                 let _ = unit_two end[@@ocaml.doc "@inline"][@@merlin.hide ]

As expected, if we force the unused-code-warnings, there will be no let _ for
any of the two values.

  $ ./driver.exe -unused-code-warnings=force -impl alias_warn.ml
  type t = int[@@deriving alias_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct let unit_one = () end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include struct let unit_two = unit_one end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                     ]

Same goes for .mli:

  $ cat > alias_warn.mli << EOF
  > type t = int [@@deriving alias_warn]
  > EOF

  $ ./driver.exe -unused-code-warnings=true -intf alias_warn.mli
  type t = int[@@deriving alias_warn]
  include sig val unit_one : unit end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include sig [@@@ocaml.warning "-32"] val unit_two : unit end[@@ocaml.doc
                                                                "@inline"]
  [@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=false -intf alias_warn.mli
  type t = int[@@deriving alias_warn]
  include sig [@@@ocaml.warning "-32"] val unit_one : unit end[@@ocaml.doc
                                                                "@inline"]
  [@@merlin.hide ]
  include sig [@@@ocaml.warning "-32"] val unit_two : unit end[@@ocaml.doc
                                                                "@inline"]
  [@@merlin.hide ]

  $ ./driver.exe -unused-code-warnings=force -intf alias_warn.mli
  type t = int[@@deriving alias_warn]
  include sig val unit_one : unit end[@@ocaml.doc "@inline"][@@merlin.hide ]
  include sig val unit_two : unit end[@@ocaml.doc "@inline"][@@merlin.hide ]

-------------------------------------------------

The following monitors a behavior that may change in the future. We're
exercising in this test to help maintaining awareness of the current behavior.
At the moment, a line is generated by ppxlib to make sure that the type is used.
This is the construct `let _ = fun (_ : t) -> ()` below. This is done to avoid
a potential unused type warning 34. Note that this line is generated even when
`unused-code-warnings=force` (this flag does not impact its generation).

  $ ./driver.exe -unused-code-warnings=force -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct let _ = fun (_ : t) -> () end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  include struct module Zero = struct type t =
                                        | T0  end
                 let zero = Zero.T0 end[@@ocaml.doc "@inline"][@@merlin.hide ]

At the moment, it is possible to disable this line by using the
`-deriving-keep-w32` flag, set to either `impl` or `both`, as shown below:

  $ ./driver.exe -unused-code-warnings=force -deriving-keep-w32=impl -impl zero_do_warn.ml
  type t = int[@@deriving zero_do_warn]
  include struct module Zero = struct type t =
                                        | T0  end
                 let zero = Zero.T0 end[@@ocaml.doc "@inline"][@@merlin.hide ]

This behavior is due to historical reasons - the unused-type warning has the
number 34, not 32. This may change in the future.
