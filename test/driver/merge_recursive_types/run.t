When deriving values for recursive type declarations, we only want to derive
the set once, even if the user has added a deriving declaration to each type
in the set. Otherwise, the resulting code can be extremely large.

To test this, we have a simple deriver that adds value bindings for the names
of the types. We expect a single set.

  $ cat > test.ml << EOF
  > type t = { name : name }[@@deriving type_name]
  > and name = { txt : txt } [@@deriving type_name]
  > and txt = string [@@deriving type_name]
  > EOF

  $ ./driver.exe test.ml
  type t = {
    name: name }[@@deriving type_name]
  and name = {
    txt: txt }[@@deriving type_name]
  and txt = string[@@deriving type_name]
  include
    struct
      let _ = fun (_ : t) -> ()
      let _ = fun (_ : name) -> ()
      let _ = fun (_ : txt) -> ()
      let t = "t"
      let _ = t
      let name = "name"
      let _ = name
      let txt = "txt"
      let _ = txt
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

We also test for when, by some chance, a deriver is called multiple times on a
recursive type declaration, but with different arguments.

  $ cat > test.ml << EOF
  > type t = { name : name }[@@deriving type_name]
  > and name = string[@@deriving type_name ~prefix:"PREFIX_"]
  > EOF

  $ ./driver.exe test.ml
  type t = {
    name: name }[@@deriving type_name]
  and name = string[@@deriving type_name ~prefix:"PREFIX_"]
  include
    struct
      let _ = fun (_ : t) -> ()
      let _ = fun (_ : name) -> ()
      let t = "t"
      let _ = t
      let name = "name"
      let _ = name
      let t = "PREFIX_t"
      let _ = t
      let name = "PREFIX_name"
      let _ = name
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

But again, if we can show that the derivers are completely equal, we merge them.

  $ cat > test.ml << EOF
  > type t = { name : name }[@@deriving type_name ~prefix:"PREFIX_"]
  > and name = string[@@deriving type_name ~prefix:"PREFIX_"]
  > EOF

  $ ./driver.exe test.ml
  type t = {
    name: name }[@@deriving type_name ~prefix:"PREFIX_"]
  and name = string[@@deriving type_name ~prefix:"PREFIX_"]
  include
    struct
      let _ = fun (_ : t) -> ()
      let _ = fun (_ : name) -> ()
      let t = "PREFIX_t"
      let _ = t
      let name = "PREFIX_name"
      let _ = name
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

