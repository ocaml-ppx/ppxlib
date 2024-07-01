We want to make sure that migrations from 5.2 to previous versions still
produce valid location ranges between parent and child.
  $ cat > test.ml << EOF
  > let make ~foo ~bar = foo ^ bar
  > EOF

We run a custom driver that will read our ast, migrate it back to 5.01, and
check that the locations are valid (the parent range is larger than the child
range).

  $ ./check_locations_integrity.exe --impl test.ml -o ignore.ml
  File "test.ml", line 1, characters 14-18:
  1 | let make ~foo ~bar = foo ^ bar
                    ^^^^
  Error: Function's location is not larger than its body
  Parent location: File "test.ml", line 1, characters 14-18
  Child location: File "test.ml", line 1, characters 21-30
  [1]

Locations should also be well formed for Pparam_newtype
  $ cat > test.ml << EOF
  > let make (type t) (type u) ~foo ~bar = foo ^ bar
  > EOF

  $ ./check_locations_integrity.exe --impl test.ml -o ignore.ml
  File "test.ml", line 1, characters 18-26:
  1 | let make (type t) (type u) ~foo ~bar = foo ^ bar
                        ^^^^^^^^
  Error: Function's location is not larger than its body
  Parent location: File "test.ml", line 1, characters 18-26
  Child location: File "test.ml", line 1, characters 27-31
  [1]
