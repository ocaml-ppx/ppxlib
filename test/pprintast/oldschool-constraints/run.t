We have two executables that prints the same bit of code:
let f : 'a . 'a -> unit = ()
but represented with different ASTs: pprint_pvb_constraint encodes the type
constraint in the pvb_constraint field of the value_binding while
pprint_ppat_constraint encodes it in the pvb_pat field, i.e. the legacy way.


  $ ./pprint_pvb_constraint.exe
  let f : 'a . 'a -> unit = ()

  $ ./pprint_ppat_constraint.exe
  let (f : 'a . 'a -> unit) = ()

The legacy gets printed with wrapping parens around the whole pattern but a
polymorphic type cannot be written in there and the latter doesn't parse.
These should instead be printed as the former instead and the output should be correctly
parsed by the compiler:

  $ ./pprint_ppat_constraint.exe > test.ml
  $ ocamlc test.ml
  File "test.ml", line 1, characters 12-13:
  1 | let (f : 'a . 'a -> unit) = ()
                  ^
  Error: Syntax error: ) expected
  File "test.ml", line 1, characters 4-5:
  1 | let (f : 'a . 'a -> unit) = ()
          ^
    This ( might be unmatched
  [2]
