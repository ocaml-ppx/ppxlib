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

Zero_do_warn is registered with unused_code_warning set true meaning it allows
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

Let's consider the following ocaml source file using the one_no_warn deriver

  $ cat > one_no_warn.ml << EOF
  > type t = int [@@deriving one_no_warn]
  > EOF

One_no_warn is registered with unused_code_warning set to false, meaning the driver
should always disable warnings for the generated code, regardless of the value of
the -unused-code-warning flag. The following driver invocations have the same
output:

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
